#--------------------------------------------------------------------------------------
#' @description Import of WHO JECFA ADI data
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
import_source_who_jecfa_adi <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "WHO JECFA ADI"
  source_table = "source_who_jecfa_adi"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2022-11-01")
  dir = paste0(toxval.config()$datapath,"who_jecfa_adi/who_jecfa_adi_files/")
  file = paste0(dir,"source_who_jecfa_raw_data_20231101.xlsx")
  res0 = readxl::read_xlsx(file)
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
    # Rename toxval names
    dplyr::rename(name = 'Webpage Name',
                  casrn = 'CAS number',
                  year = 'Evaluation year')

  res = res %>%
    # Derive toxval_numeric, toxval_units, toxval_numeric_qualifier from ADI column
    tidyr::separate_rows(ADI, sep = ';') %>%
    dplyr::mutate(
      cleaned_adi = gsub("\\(.*?\\)","",ADI),
      cleaned_adi = gsub("–", "-", cleaned_adi),
      cleaned_adi = gsub("\\s*-\\s*", "-", cleaned_adi),
      toxval_numeric = stringr::str_extract(cleaned_adi, "\\d+\\.?\\d*-?\\d*\\.?\\d*"),
      toxval_units = stringr::str_replace(cleaned_adi, paste0(".*", toxval_numeric),""),
      toxval_units = stringr::str_trim(toxval_units),
      toxval_units = if_else(is.na(toxval_numeric), NA_character_, toxval_units),
      toxval_numeric_qualifier = dplyr::case_when(
        grepl(">", cleaned_adi) ~ ">",
        grepl("<", cleaned_adi) ~ "<",
        grepl("=", cleaned_adi) ~ "=",
        grepl("~", cleaned_adi) ~ "~",
        TRUE ~ "-"
      )
    )


  res$toxval_type <- "ADI"
  res$species <- "human"
  res$exposure_route <- "oral"
  res$exposure_method <- "diet"

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




