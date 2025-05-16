#--------------------------------------------------------------------------------------
#' @description Import of EPA DCAP 2025-05-07 source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_epa_dcap_source
#' @return None. SQL statements executed.
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
#' @rdname import_generic_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_squish
#' @importFrom dplyr mutate across where
#' @importFrom tidyr replace_na
#--------------------------------------------------------------------------------------
import_epa_dcap_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "EPA DCAP"
  source_table = "source_epa_dcap_ctvs"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2025-05-07")
  dir = paste0(toxval.config()$datapath,"epa_dcap_ctvs/epa_dcap_ctvs_files/")
  file = paste0(dir, "DCAP Output Table.xlsx")
  res0 = readxl::read_xlsx(file, sheet = "DCAP")
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  #
  # the final file should have column names that include "name" and "casrn"
  # additionally, the names in res need to match names in the source
  # database table. You do not need to add any of the generic columns
  # described in the SOP - they will get added in source_prep_and_load
  #

  # Add source specific transformations
  res = res0 %>%
    dplyr::mutate(toxval_type = "CTV",
           toxval_numeric = CTV_arithmetic,
           toxval_units = "mg/kg-d",
           toxval_numeric_qualifier = "=",
           study_type = "toxicity value",
           species = "human",
           exposure_route = "oral",
           year = 2025,
           # qc_status = "pass",
           # TODO fill in updated long_ref once published
           long_ref = paste0("Harrill, Alison H., Shintaro Hagiwara, Chelsea A. Weitekamp, Paul C. Stanish, ",
                             "Jonathan T. Wall, Risa R. Sayre, Sarah E. Davidson-Fritz, Kelsey Vitense, Daniel T. Chang, ",
                             "Michael J. Devito, Chris J. Gonzales, Maxwell Groover, Michael F. Hughes, Richard S. Judson, ",
                             "Jason C. Lambert, Charles N. Lowe, Esra Mutlu, Katie Paul-Friedman, Andrew M. Watkins, Antony J. Williams, ",
                             "Daniel Krewski, Greg M. Paoli, Russell S. Thomas. ",
                             "Estimation of database-calibrated toxicity values for human health assessment using existing toxicology data for ",
                             "one thousand chemicals, In review: Journal of Toxicology and Environmental Health."),
           # TODO fill in URL with journal link once published
           url = "https://zenodo.org/records/15357834")

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
