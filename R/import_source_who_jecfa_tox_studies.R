#--------------------------------------------------------------------------------------
#' @description Import of WHO JECFA Tox Studies data
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
import_source_who_jecfa_tox_studies <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "WHO JECFA Tox Studies"
  source_table = "source_who_jecfa_tox_studies"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2022-11-01")
  dir = paste0(toxval.config()$datapath,"who_jecfa_tox_studies/who_jecfa_tox_studies_files/")
  file = paste0(dir,"source_who_jecfa_raw_toxicological_data_20231101.xlsx")
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
  library(readr)
  res = res0 %>%
    # Copy toxval fields from originals
    dplyr::mutate(name = `Webpage Name` %>%
                    # Replace trademark/copyright symbol
                    gsub("\u00ae|<U+00ae>|\u00a9|\u2122", "", .) %>%
                    fix.greek.symbols(),
                  casrn = `CAS number` %>%
                    # Remove parenthetics
                    gsub("\\s*\\([^\\)]+\\)","", .),
                  critical_effect = `Effect`,
                  species = `Animal Specie`,
                  year = `Evaluation year`) %>%
    tidyr::separate_rows(casrn, sep=";") %>%
    dplyr::mutate(casrn = casrn %>%
                    stringr::str_squish())

  # Deal with two toxval_numeric values in one. Hardcode split
  row_to_split <- res %>%
    dplyr::filter(NOAEL == "Rats only:  0.67 mg/kg bw/d (males) and 1.88 mg/kg bw/d (females)")
  split_rows <- row_to_split %>%
    tidyr::separate_rows(NOAEL, sep = " and ")

  if (nrow(split_rows) > 0){
    res <- res %>%
      dplyr::filter(!NOAEL %in% "Rats only:  0.67 mg/kg bw/d (males) and 1.88 mg/kg bw/d (females)")
    res <- dplyr::bind_rows(res, split_rows)
  }


  res = res %>%
    pivot_longer(cols = c(NOAEL, NOEL, LOEL, PMTDI, LOAEL, PTWI, PTMI, PTDI), names_to= "toxval_type", values_to = "value") %>%
    tidyr::separate_rows(value, sep = ';') %>%
    dplyr::mutate(
      value = fix.greek.symbols(value)
    ) %>%
    dplyr::mutate(
      cleaned_value = value %>%
        gsub("\\(.*?\\)","",.) %>%
        gsub("–", "-", .) %>%
        gsub("\\s*-\\s*", "-", .),
      toxval_numeric = cleaned_value %>%
        stringr::str_extract(., "\\d+\\.?\\d*-?\\d*\\.?\\d*"),
      # Fixes a few cases of the toxval_numeric occurring twice in the string, causing issues with toxval_units
      toxval_units = stringr::str_match(cleaned_value, paste0(toxval_numeric, "(.*)"))[,2],
      toxval_units = stringr::str_trim(toxval_units),
      toxval_units = if_else(is.na(toxval_numeric), NA_character_, toxval_units),
      toxval_numeric_qualifier = dplyr::case_when(
        grepl(">", cleaned_value) ~ ">",
        grepl("<", cleaned_value) ~ "<",
        grepl("=", cleaned_value) ~ "=",
        grepl("~", cleaned_value) ~ "~",
        TRUE ~ "-"
      ),
      # Extract sex
      sex = str_extract(value, "\\((males|females)\\)")
    ) %>%
    mutate(
      sex = if_else(!is.na(sex), str_extract(sex, "(males|females)"), "-"),
    ) %>%

    dplyr::filter(!is.na(toxval_numeric))

  res$toxval_units[is.na(res$toxval_numeric)] = NA

  res = res %>%
    dplyr::select(-cleaned_value, -value)

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




