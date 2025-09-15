#--------------------------------------------------------------------------------------
#' @description Import of EU SCOEL source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_eu_scoel_source
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
import_eu_scoel_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "EU SCOEL"
  source_table = "source_eu_scoel"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2024-11-11")
  dir = paste0(toxval.config()$datapath,"eu_scoel/eu_scoel_files/")
  file = paste0(dir, "EU_SCOEL_Derivations_QC_final.xlsx")
  # Skip first few rows that were manually curated as metadata for the file
  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  # Add source specific transformations
  res = res0 %>%
    # Renaming field since values curated in generation fit population better
    dplyr::rename(population = generation) %>%
    dplyr::mutate(
      qc_status = dplyr::case_when(
        !is.na(`QC result`) ~ "pass",
        TRUE ~ "undetermined"
      ),
      year = summary_doc_year,
      casrn = dplyr::case_when(
        grepl("See Notes|substances", casrn, ignore.case = TRUE) ~ NA,
        TRUE ~ casrn
      ),
      long_ref = dplyr::case_when(
        long_ref %in% c("unspecified", "various studies", "various studies and reviews") ~ "-",
        TRUE ~ long_ref
      ),
      toxval_numeric_qualifier = dplyr::case_when(
        grepl("about", toxval_numeric) ~ "~",
        TRUE ~ toxval_numeric_qualifier
      ) %>%
        gsub("about", "~", .),
      toxval_numeric = dplyr::case_when(
        toxval_numeric %in% c("not assigned", "not determined", "no entry",
                              "none", "none identified (see notes column)",
                              "see notes", "None", "not applicable", "Not assigned",
                              "no recommendation made", "insufficient data") ~ NA,
        grepl("carcinogen|not feasible|adequate", toxval_numeric, ignore.case = TRUE) ~ NA,
        TRUE ~ toxval_numeric
      ) %>%
        gsub("about", "", .) %>%
        stringr::str_squish() %>%
        as.numeric()
    ) %>%
    # Remove empty rows that only have NA values
    .[rowSums(is.na(.)) < ncol(.), ] %>%
    # Filter out records that do not have a name and casrn
    dplyr::filter(!(is.na(name) & is.na(casrn))) %>%
    dplyr::filter(!toxval_type %in% c("Additional categorisation:", "unidentified POD")) %>%
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
