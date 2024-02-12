#--------------------------------------------------------------------------------------
#' @title import_niosh_source
#' @description Load NIOSH data into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
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
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{case_when}}
#'  \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_extract}}
#'  \code{\link[tidyr]{separate}}, \code{\link[tidyr]{drop_na}} \code{\link[tidyr]{pivot_longer}}
#' @rdname import_niosh_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate case_when
#' @importFrom stringr str_squish str_extract
#' @importFrom tidyr separate drop_na pivot_longer
#--------------------------------------------------------------------------------------
import_niosh_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "NIOSH"
  source_table = "source_niosh"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2019-10-08")
  dir = paste0(toxval.config()$datapath,"niosh/niosh_files/")
  file = paste0(dir,"updated_niosh_extraction_20240207.xlsx")
  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  res = res0 %>%
    # Get toxval_numeric_units (maintain separate entries for updated/original values)
    tidyr::pivot_longer(
      cols = c("IDLH Value (1994)*", "New/Updated Values (2016-present)**"),
      names_to = "version",
      values_to = "toxval_numeric_units"
    ) %>%

    dplyr::mutate(
      # Rename cols/add hard-coded values
      name = Substance,
      toxval_type = "NIOSH IDLH Concentration",
      exposure_route = "inhalation",
      source_url = "https://www.cdc.gov/niosh/idlh/intridl4.html",

      # Remove casrn noise
      casrn = `CAS no.` %>%
        gsub("\\(.+\\)", "", .) %>%
        stringr::str_squish(),

      # Extract long_ref
      long_ref = toxval_numeric_units %>%
        # Fix case of NIOSH Pub missing space
        gsub("NIOSHPub.", "NIOSH Pub.", .) %>%
        stringr::str_extract("NIOSH Pub\\. No\\. \\d{4}\\-\\d{3}") %>%
        c() %>% stringr::str_squish(),

      # Extract clean values from toxval_numeric_units
      toxval_numeric_units = dplyr::case_when(
        # If updated value is available, handle appropriately
        version == "New/Updated Values (2016-present)**" ~ stringr::str_extract(
          toxval_numeric_units,
          "[0-9,\\.]+?\\sppm"
        ) %>% c(),
        # Replace "Unknown" with NA
        grepl("Unknown", toxval_numeric_units, ignore.case=TRUE) ~ as.character(NA),
        # Keep value as-is
        TRUE ~ toxval_numeric_units
      ),

      # Get year
      year = dplyr::case_when(
        # Use year of update, if available
        !is.na(long_ref) ~ stringr::str_extract(long_ref, "\\d{4}")%>% c() %>% as.numeric(),
        # Otherwise, use 1994
        TRUE ~ 1994
      )
    ) %>%

    # Separate toxval_numeric and toxval_units
    tidyr::separate(
      toxval_numeric_units,
      into = c("toxval_numeric", "toxval_units"),
      sep = " ",
      extra = "merge",
    ) %>%

    # Conduct final cleaning operations
    dplyr::mutate(
      # Clean toxval_units
      toxval_units = toxval_units %>%
        # Standardize substance-specific mg/m3
        gsub("mg.*\\/.*m3", "mg/m3", .) %>%
        # Remove noise contained in brackets or parentheses
        gsub("\\(.+\\)|\\[.+\\]", "", .) %>%
        stringr::str_squish(),

      # Ensure toxval_numeric is numeric type
      toxval_numeric = toxval_numeric %>%
        gsub(",", "", .) %>%
        as.numeric()
    ) %>%

    # Remove entries without necessary toxval columns
    tidyr::drop_na(toxval_numeric, toxval_units)

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    # Remove *
    gsub("\\*", "", .) %>%
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




