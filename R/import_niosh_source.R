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
#'  \code{\link[dplyr]{mutate}}
#'  \code{\link[stringr]{str_extract}}, \code{\link[stringr]{str_trim}}
#'  \code{\link[tidyr]{drop_na}}
#' @rdname import_niosh_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate
#' @importFrom stringr str_extract str_squish
#' @importFrom tidyr drop_na
#--------------------------------------------------------------------------------------
import_niosh_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "NIOSH"
  source_table = "source_niosh"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2019-10-08")
  dir = paste0(toxval.config()$datapath,"niosh/niosh_files/")
  file = paste0(dir,"niosh_IDLH_2020.xlsx")
  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  res = res0 %>%
    dplyr::mutate(
      # Add hard-coded values
      toxval_type = "NIOSH IDLH Concentration",
      exposure_route = "inhalation",

      # UNCOMMENT if old load script source_url is preferred
      # source_url = "https://www.cdc.gov/niosh/idlh/default.html",

      # Standardize compound-specific units
      toxval_units = toxval_units %>%
        gsub("mg [a-zA-Z]{1,2}\\/m3", "mg/m3", .),

      # Extract long_ref
      long_ref = toxval_numeric_details %>%
        stringr::str_extract("NIOSH Pub\\. No\\. \\d{4}\\-\\d{3}") %>%
        c() %>% stringr::str_squish(),

      # Ensure toxval_numeric is of numeric type
      toxval_numeric = as.numeric(toxval_numeric)
    ) %>%

    # Remove entries without necessary toxval columns
    tidyr::drop_na(toxval_numeric, toxval_units)

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




