#--------------------------------------------------------------------------------------
#' @description Import of EPA TSCA 8e source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_epa_tsca_8e_source
#' @return None; data is pushed to toxval_source
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
import_epa_tsca_8e_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "EPA TSCA 8e"
  source_table = "source_epa_tsca_8e"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2024-07-27")
  dir = paste0(toxval.config()$datapath,"epa_tsca_8e/epa_tsca_8e_files/")
  file = paste0(dir, "epa_tsca_8e.xlsx")
  res0 = readxl::read_xlsx(file, sheet = "Data curation")
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  # Add source specific transformations
  res = res0 %>%
    dplyr::filter(`ToxVal Relevance` %in% c("Relevant")) %>%
    dplyr::mutate(year = year_study_performed,
                  toxval_type = effect_type,
                  toxval_numeric_qualifier = effect_type_qualifier,
                  toxval_numeric = effect_type_dose,
                  toxval_units = effect_type_dose_units,
                  critical_effect = effects,
                  casrn = dplyr::case_when(
                    # Ends with ;
                    grepl(";$", casrn) ~ gsub(";$", "", casrn),
                    grepl("CBI", casrn) ~ NA,
                    TRUE ~ casrn
                  ),
                  chem_mixture_flag = stringr::str_count(name, ";")
    ) %>%
    # TODO Decide to filter or split some other way
    # Filter out mixture records with multiple ";"
    dplyr::filter(chem_mixture_flag == 0) %>%
    dplyr::select(-chem_mixture_flag) %>%
    dplyr::mutate(casrn = stringr::str_squish(casrn)) %>%
    # Remove empty rows that only have NA values
    .[rowSums(is.na(.)) < ncol(.), ] %>%
    # Filter out records that do not have a name and casrn
    dplyr::filter(!(is.na(name) & is.na(casrn))) %>%
    tidyr::drop_na(toxval_type, toxval_numeric)

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
