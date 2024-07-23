#--------------------------------------------------------------------------------------
#' @description Load Copper Manufacturers data into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_copper_source
#' @return None; data is pushed to toxval_source
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[dplyr]{rename}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{row_number}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{distinct}}
#'  \code{\link[tidyr]{drop_na}}, \code{\link[tidyr]{separate}}
#'  \code{\link[stringr]{str_extract}}, \code{\link[stringr]{str_trim}}
#' @rdname import_copper_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr rename mutate row_number case_when filter select bind_rows distinct
#' @importFrom tidyr drop_na separate
#' @importFrom stringr str_extract str_squish
#--------------------------------------------------------------------------------------
import_copper_source <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "Copper Manufacturers"
  source_table = "source_copper"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2020-11-20")
  dir = paste0(toxval.config()$datapath,"copper/copper_files/")
  file = paste0(dir,"Copper Data Entry - Final.xlsx")
  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  res = res0 %>%
    # Fix duplicate field names
    dplyr::rename(
      toxval_subtype = `toxval_subtype...7`, toxval_subtype1 = `toxval_subtype...8`,
      year = `year...28`, year1 = `year...34`
    ) %>%

    dplyr::mutate(casrn = dplyr::na_if(casrn, "NA")) %>%

    # Drop entries with no casrn
    tidyr::drop_na(casrn) %>%

    # Split toxval_numeric into lower and upper range values
    tidyr::separate(
      toxval_numeric,
      into=c("toxval_numeric", "toxval_numeric_upper"),
      sep="-",
      fill="right"
    ) %>%

    dplyr::mutate(
      source_url = url,
      subsource_url = source_url,

      # Set relationship ID to handle ranged toxval_numeric entries
      range_relationship_id = dplyr::row_number(),

      # Extract toxval_numeric_qualifier from toxval_numeric
      toxval_numeric_qualifier = stringr::str_extract(toxval_numeric, "([<>~=])", group=1),
      toxval_numeric = gsub("<|>|~|=", "", toxval_numeric),

      # Fill in NA toxval_subtype
      toxval_subtype = dplyr::case_when(
        toxval_subtype %in% c(as.character(NA), "-") ~ toxval_subtype1,
        TRUE ~ toxval_subtype
      ),

      # Translate sex values
      sex = sex %>%
        gsub("M", "male", .) %>%
        gsub("F", "female", .),

      # Extract and clean study_duration_value
      study_duration_value = study_duration_value %>%
        stringr::str_extract("([0-9]+(?:\\s*\\-\\s*[0-9]+)?)", group = 1) %>%
        gsub("\\s*\\-\\s*", "-", .) %>%
        stringr::str_squish(),

      # Clean species field
      species = species %>%
        tolower() %>%
        gsub("infant", "", .) %>%
        stringr::str_squish()
    )

  # Handle entries with upper ranges
  lower_range_res = res %>%
    dplyr::filter(!is.na(toxval_numeric_upper)) %>%
    dplyr::select(-toxval_numeric_upper) %>%
    dplyr::mutate(
      toxval_relationship = "Lower Range",
      toxval_numeric_qualifier = dplyr::case_when(
        is.na(toxval_numeric_qualifier) ~ ">=",
        TRUE ~ toxval_numeric_qualifier
      )
    )
  upper_range_res = res %>%
    dplyr::filter(!is.na(toxval_numeric_upper)) %>%
    dplyr::mutate(
      toxval_numeric = toxval_numeric_upper,
      toxval_relationship = "Upper Range",
      toxval_numeric_qualifier = "<="
    ) %>%
    dplyr::select(-toxval_numeric_upper)
  # Recombine ranged entries with original data
  res = res %>%
    dplyr::filter(is.na(toxval_numeric_upper)) %>%
    dplyr::select(-toxval_numeric_upper) %>%
    dplyr::bind_rows(lower_range_res, upper_range_res) %>%

    # Perform final cleaning operations
    dplyr::mutate(
      toxval_numeric = as.numeric(toxval_numeric),
      toxval_numeric_qualifier = dplyr::case_when(
        is.na(toxval_numeric_qualifier) ~ "=",
        TRUE ~ toxval_numeric_qualifier
      )
    ) %>%

    # Drop unused columns
    dplyr::select(-toxval_subtype1, year1) %>%
    dplyr::distinct()

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"

  # Perform deduping
  res = toxval.source.import.dedup(res)

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
