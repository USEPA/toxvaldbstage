#--------------------------------------------------------------------------------------
#' @description Import of TX TCEQ source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_tx_tceq_source
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
import_tx_tceq_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "TX TCEQ"
  source_table = "source_tx_tceq"
  # Date provided by the source or the date the data was extracted
  # src_version_date = as.Date("YYYY-MM-DD")
  dir = paste0(toxval.config()$datapath,"tx_tceq/tx_tceq_files/")
  file_list = list.files(dir, pattern = "xlsx", full.names = TRUE)

  res0 = lapply(file_list, function(f){
    tmp = readxl::read_xlsx(f) %>%
      dplyr::mutate(data_filename = basename(f),
                    # Add version date by file
                    source_version_date = dplyr::case_when(
                      grepl("amcv", data_filename, ignore.case = TRUE) ~ as.Date("2024-06-11"),
                      grepl("esl", data_filename, ignore.case = TRUE) ~ as.Date("2025-01-20"),
                      TRUE ~ NA
                    ))
  }) %>%
    dplyr::bind_rows() %>%
    # Combine footnote fields
    tidyr::unite(
      col = "footnotes",
      footnotes, Footnotes,
      sep = "; ",
      na.rm = TRUE)

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
    dplyr::mutate(year = summary_doc_year,
                  casrn = dplyr::case_when(
                    grepl("problem|NOCAS", casrn, ignore.case = TRUE) ~ NA,
                    TRUE ~ casrn
                  ) %>%
                    # Remove parentheses
                    gsub("\\s*\\([^)]+\\)", "", .),
                  name = dplyr::case_when(
                    grepl("unspecified", name, ignore.case = TRUE) ~ NA,
                    # Remove starting and ending brackets (e.g., [Hexamethylenediamine])
                    grepl("^\\[.*\\]$", name) ~ name %>%
                      gsub("^\\[|\\]$", "", .),
                    TRUE ~ name
                  ) %>%
                    gsub(":$", "", .)) %>%
    # Remove empty rows that only have NA values
    .[rowSums(is.na(.)) < ncol(.), ] %>%
    # Remove columns where all values are NA
    .[, colSums(is.na(.)) != nrow(.)] %>%
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
