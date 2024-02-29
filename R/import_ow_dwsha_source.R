#--------------------------------------------------------------------------------------
#' @title import_ow_dwsha_source
#' @description Load OW Drinking Water Standards data into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @return None; data is added to toxval_source
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{row_number}}, \code{\link[dplyr]{bind_rows}}
#'  \code{\link[tidyr]{separate}}, \code{\link[tidyr]{pivot_longer}}
#'  \code{\link[stringr]{str_trim}}
#' @rdname import_ow_dwsha_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate case_when filter row_number bind_rows
#' @importFrom tidyr separate pivot_longer
#' @importFrom stringr str_squish
#--------------------------------------------------------------------------------------
import_ow_dwsha_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source <- "OW Drinking Water Standards"
  source_table = "source_ow_dwsha"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2018-03-01")
  dir = paste0(toxval.config()$datapath,"ow_dwsha/ow_dwsha_files/")
  file = paste0(dir,"OW_DWSHA_for_toxval.xlsx")
  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  res = res0 %>%
    dplyr::mutate(
      # Override URL from extraction document (broken link) - use URL from load script
      source_url = "https://www.epa.gov/ground-water-and-drinking-water",

      # Set study_type from RAC
      study_type = risk_assessment_class,

      # Translate "MFL" (million fibers per liter) to fibers/L
      toxval_units = dplyr::case_when(
        grepl("MFL", toxval_numeric) ~ "fibers/L",
        toxval_units == "MFL" ~ "fibers/L",
        TRUE ~ toxval_units
      ),

      toxval_numeric = dplyr::case_when(
        # Multiply by 1mil to account for MFL conversion, where appropriate
        toxval_units == "fibers/L" ~ toxval_numeric %>%
          gsub("\\-MFL", "", .) %>%
          paste0(., "000000"),
        TRUE ~ toxval_numeric
      ), #%>%
        # # Handle exponents
        # gsub("E\\-0*", "e-0", .),

      # Set study_duration values based on toxval_subtype
      study_duration_value = dplyr::case_when(
        grepl("one day", toxval_subtype) ~ 1,
        grepl("10 day", toxval_subtype) ~ 10,
        grepl("Lifetime", toxval_subtype) ~ 1,
        TRUE ~ NA
      ),
      study_duration_units = dplyr::case_when(
        grepl("day", toxval_subtype) ~ "day",
        grepl("Lifetime", toxval_subtype) ~ "lifetime",
        TRUE ~ as.character(NA)
      ),

      # Set critical_effect from toxval_subtype
      critical_effect = dplyr::case_when(
        grepl("Cancer", toxval_subtype) ~ toxval_subtype,
        TRUE ~ as.character(NA)
      )
    )

  # Handle ranged toxval_numeric values
  ranged_res = res %>% dplyr::filter(grepl("to", toxval_numeric))
  if(nrow(ranged_res)) {
    ranged_res = ranged_res %>%
      dplyr::mutate(
        numeric_relationship_id = dplyr::row_number()
      ) %>%
      tidyr::separate(
        col = toxval_numeric,
        into = c("Lower Range", "Upper Range"),
        sep = " to ",
        remove = TRUE
      ) %>%
      tidyr::pivot_longer(
        cols = c("Lower Range", "Upper Range"),
        values_to = "toxval_numeric",
        names_to = "numeric_relationship_description"
      )
  } else {
    # Empty dataframe with res cols to bind_rows()
    ranged_res = res[0,]
  }

  # Add ranged data to res
  res = res %>%
    dplyr::filter(!grepl("to", toxval_numeric)) %>%
    dplyr::bind_rows(ranged_res) %>%
    dplyr::mutate(toxval_numeric = as.numeric(toxval_numeric))

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"

  # Add version date. Can be converted to a mutate statement as needed
  res$source_version_date <- src_version_date
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db=db,
                       source=source,
                       table=source_table,
                       res=res,
                       do.reset=do.reset,
                       do.insert=do.insert,
                       chem.check.halt=chem.check.halt,
                       hashing_cols=toxval.config()$hashing_cols)
}




