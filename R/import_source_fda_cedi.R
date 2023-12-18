#--------------------------------------------------------------------------------------
#' @description A generic template for adding data to toxval_source for a new source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title FUNCTION_TITLE
#' @return OUTPUT_DESCRIPTION
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
#--------------------------------------------------------------------------------------
import_source_fda_cedi <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "FDA CEDI"
  source_table = "source_fda_cedi"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2023-11-17")
  dir = paste0(toxval.config()$datapath,"fda_cedi/fda_cedi_files/")
  file = paste0(dir,"source_fda_cedi_20231117.csv")
  res0 = readr::read_csv(file, skip=4)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  #
  # the final file should have column names that include "name" and "casrn"
  # additionally, the names in res need to match names in the source
  # database table. You do not need to add any of the generic columns
  # described in the SOP - they will get added in source_prep_and_load
  #

  res = res0 %>%
    dplyr::mutate(name = `Substance`,
                  casrn = `CAS Reg. No. (or other ID)`,
                  year = substr(`Calculation/update date`, 7, 10)
    ) %>%
    tidyr::unite(Reg, dplyr::starts_with("Reg"), sep = ",", na.rm = TRUE)

  res <- res %>%
    pivot_longer(cols = c("CEDI (&micro;g/kb bw/d)", "CDC (ppb)"),
                 names_to = "toxval_type",
                 values_to = "toxval_numeric") %>%
    tidyr::separate(col="toxval_type",
                    into = c("toxval_type", "toxval_units"), sep = "\\(") %>%
    mutate(toxval_type = case_when(
      grepl("CEDI", toxval_type) ~ "Cumulative Estimated Daily Intake",
      grepl("CDC", toxval_type) ~ "Cumulative Dietary Concentration"
    ),
    toxval_units = toxval_units %>%
      gsub("\\)", "", .),
    toxval_numeric = toxval_numeric %>%
      sub('.*?"(.*?)"\\)', '\\1', .))

  res$source_url = "https://cfsanappsexternal.fda.gov/scripts/fdcc/?set=CEDI"
  res$subsource = "FDA CFSAN"
  res$risk_assessment = "chronic"

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




