#--------------------------------------------------------------------------------------
#' @description Import of NJ DEP source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_nj_dep_source
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
import_nj_dep_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "NJ DEP"
  source_table = "source_nj_dep"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2024-06-11")
  dir = paste0(toxval.config()$datapath,"nj_dep/nj_dep_files/")

  file_list = list.files(dir, pattern = "xlsx")
  # Load all files
  res0 = lapply(file_list, function(f){
    tmp = readxl::read_xlsx(paste0(dir, f)) %>%
      dplyr::mutate(data_filename = f)

    if("study_year" %in% names(tmp)){
      tmp = tmp %>%
        dplyr::mutate(study_year = as.character(study_year))
    }

    if("summary_doc_year" %in% names(tmp)){
      tmp = tmp %>%
        dplyr::mutate(summary_doc_year = as.character(summary_doc_year))
    }

    # Standardize the names
    names(tmp) <- names(tmp) %>%
      stringr::str_squish() %>%
      # Replace whitespace and periods with underscore
      gsub("[[:space:]]|[.]", "_", .) %>%
      tolower()
    return(tmp)
  }) %>%
    dplyr::bind_rows()
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  # Add source specific transformations
  res = res0 %>%
    tidyr::separate_longer_delim(casrn, delim = " & ") %>%
    dplyr::mutate(year = summary_doc_year,
                  casrn = dplyr::case_when(
                    grepl("No CASRN|group|NOCAS|DSSTox|DTXSID|See|used|N/A", casrn, ignore.case = TRUE) ~ NA,
                    TRUE ~ casrn
                  ) %>%
                    # Remove parentheses
                    gsub("\\s*\\([^)]+\\)", "", .) %>%
                    gsub("CASRN: |)$", "", .)) %>%
    # Remove empty rows that only have NA values
    .[rowSums(is.na(.)) < ncol(.), ] %>%
    tidyr::drop_na(toxval_type, toxval_numeric) %>%
    # Remove non-chemical names
    dplyr::filter(!grepl("bacteria|\\bodor\\b|pH|Color|Dissolved Solids|Taste", name, ignore.case = TRUE)) %>%
    # Filter out records that do not have a name and casrn
    dplyr::filter(!(is.na(name) & is.na(casrn)))

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
