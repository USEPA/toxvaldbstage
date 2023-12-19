#--------------------------------------------------------------------------------------
#' @description Import FDA CEDI data into source_fda_cedi
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
#'  \code{\link[readr]{read_csv}}
#'  \code{\link[stringr]{str_trim}}
#' @rdname import_generic_source
#' @export
#' @importFrom readr read_csv
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
  # Fixes error with stringi functions regarding UTF-8 byte sequences
  res0$`Substance` <-iconv(res0$`Substance`, from="UTF-8", to="ASCII")
  res = res0 %>%
    dplyr::mutate(name = `Substance` %>% fix.replace.unicode(),
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
      sub('.*?"(.*?)"\\)', '\\1', .),
    source_url = "https://cfsanappsexternal.fda.gov/scripts/fdcc/?set=CEDI",
    subsource = "FDA CFSAN",
    risk_assessment = "chronic",
    source_version_date = src_version_date
    )

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"
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
