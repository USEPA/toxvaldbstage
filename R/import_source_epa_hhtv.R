#--------------------------------------------------------------------------------------
#' @description Push EPA HHTV data to toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_source_epa_hhtv
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
#'  \code{\link[stringr]{str_trim}}
#' @rdname import_source_epa_hhtv
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_squish
#' @importFrom dplyr mutate across where
#' @importFrom tidyr replace_na
#--------------------------------------------------------------------------------------
import_source_epa_hhtv <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "EPA HHTV"
  source_table = "source_epa_hhtv"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2024-06-03")
  dir = paste0(toxval.config()$datapath,"epa_hhtv/epa_hhtv_files/")
  file = paste0(dir,"EPA HHTV extraction_2024-06-03.xlsx")
  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  res = res0 %>%
    dplyr::mutate(
      # Add hardcoded fields
      long_ref = study_reference,
      year = last_updated,
      source_url = url,

      # Match sex formatting to toxval
      sex = sex %>%
        gsub(", ", "/", .),
      # Normalize units
      toxval_units = dplyr::case_when(
        toxval_units == "mg/kg-d" ~ "mg/kg-day",
        TRUE ~ toxval_units
      )
    )

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  res = res %>%
    # Generic cleanup of strings before dedup check
    dplyr::mutate(
      dplyr::across(dplyr::where(is.character), ~tidyr::replace_na(., "-") %>%
                      fix.replace.unicode() %>%
                      stringr::str_squish()),
      dplyr::across(dplyr::where(is.character), ~gsub("\\r|\\n|\\\\r|\\\\n", "", .)),
      dplyr::across(dplyr::where(is.character), ~gsub("\\\\'", "'", .)),
      dplyr::across(dplyr::where(is.character), ~gsub('\\\\\\"', '"', .))
    )

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"

  # Perform deduping
  res = toxval.source.import.dedup(res, hashing_cols=toxval.config()$hashing_cols)

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
