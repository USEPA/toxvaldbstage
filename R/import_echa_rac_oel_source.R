#--------------------------------------------------------------------------------------
#' @description Import of ECHA EAC OEL source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title FUNCTION_TITLE
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
import_echa_rac_oel_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "ECHA RAC OEL"
  source_table = "source_echa_rac_oel"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2025-01-10")
  dir = paste0(toxval.config()$datapath,"echa_rac_oel/echa_rac_oel_files/")
  file = paste0(dir, "ECHA_RAC_OELs_Derivation_2025_QC_final.xlsx")
  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  # Add source specific transformations
  res = res0 %>%
    dplyr::mutate(
      qc_status = dplyr::case_when(
        !is.na(`QC result`) ~ "pass",
        TRUE ~ "undetermined"
      ),
      year = summary_doc_year,
      casrn = dplyr::case_when(
        grepl("See Notes|NOCAS|example", casrn, ignore.case = TRUE) ~ NA,
        TRUE ~ casrn
      ),
      # Fix exposure_form
      exposure_form = exposure_form %>%
        gsub("male F344", "-", .),
      # TODO Fix toxval_numeric
      toxval_numeric = dplyr::case_when(
        grepl("none|not|see|confirmed|carc|group", toxval_numeric, ignore.case=TRUE) ~ NA,
        TRUE ~ toxval_numeric
      ) %>%
        gsub("^\\[|\\]$", "", .) %>%
        as.numeric(),
      # Fix sex
      sex = dplyr::case_when(
        sex %in% c("M") ~ "male",
        sex %in% c("M/F") ~ "male/female",
        sex %in% c("F") ~ "female",
        TRUE ~ sex
      )
    ) %>%
    # Remove empty rows that only have NA values
    .[rowSums(is.na(.)) < ncol(.), ] %>%
    tidyr::separate_longer_delim(casrn, delim = "; ") %>%
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
