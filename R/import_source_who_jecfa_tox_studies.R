#--------------------------------------------------------------------------------------
#' @title import_source_who_jecfa_tox_studies
#' @description Import of WHO JECFA Tox Studies data
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALS
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
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{filter}}
#'  \code{\link[tidyr]{separate_rows}}
#'  \code{\link[stringr]{str_trim}}
#' @rdname import_source_who_jecfa_tox_studies
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate case_when filter
#' @importFrom tidyr separate_rows
#' @importFrom stringr str_squish
#--------------------------------------------------------------------------------------
import_source_who_jecfa_tox_studies <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "WHO JECFA Tox Studies"
  source_table = "source_who_jecfa_tox_studies"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2022-11-01")
  dir = paste0(toxval.config()$datapath,"who_jecfa_tox_studies/who_jecfa_tox_studies_files/")
  file = paste0(dir,"who_jecfa_toxicological_data_manual.xlsx")
  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  res = res0 %>%
    # Copy toxval fields from originals
    dplyr::mutate(
      name = fix.replace.unicode(name) %>% toupper(),
      casrn = gsub("\\s*\\([^\\)]+\\)","", casrn),
      toxval_units = fix.replace.unicode(toxval_units),

      # Replace non-number toxval_numeric values with blank entry (not NA to preserve char col)
      toxval_numeric = dplyr::case_when(
        grepl("[a-zA-Z]", toxval_numeric) ~ "-",
        TRUE ~ toxval_numeric
      ),

      # Translate sex into M/F format
      sex = dplyr::case_when(
        sex == "male" ~ "M",
        sex == "female" ~ "F",
        sex == "male; female" ~ "M/F",
        TRUE ~ sex
      ),

      # Set "none"-type critical_effect to NA
      critical_effect = dplyr::case_when(
        critical_effect %in% c("None",
                               "None reported",
                               "not specified",
                               "No significant adverse effects") ~ NA,
        TRUE ~ critical_effect
      )
    ) %>%

    # Drop rows without toxval_numeric
    dplyr::filter(toxval_numeric != "-") %>%

    # Remove entries with blank toxval_type
    dplyr::filter(toxval_type != "-") %>%

    # Handle casrn
    tidyr::separate_rows(casrn, sep=";") %>%
    dplyr::mutate(
      casrn = casrn %>%
        stringr::str_squish(),
    )

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




