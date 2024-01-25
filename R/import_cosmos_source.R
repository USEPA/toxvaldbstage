#--------------------------------------------------------------------------------------
#' @title import_source_cosmos
#' @description Load COSMOS source data into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#' @param do.reset If TRUE, delete data from the database for this source before inserting new data
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @return None; data is loaded into toxval_source
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
#'  \code{\link[stringr]{str_trim}}
#'  \code{\link[tidyr]{separate_rows}}, \code{\link[tidyr]{drop_na}}, \code{\link[tidyr]{separate}}
#' @rdname import_source_cosmos
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate case_when filter
#' @importFrom stringr str_squish
#' @importFrom tidyr separate_rows drop_na separate
#--------------------------------------------------------------------------------------
import_source_cosmos <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "COSMOS"
  source_table = "source_cosmos"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2016-04-02")
  dir = paste0(toxval.config()$datapath,"cosmos/cosmos_files/")
  file = paste0(dir,"COSMOS_DB_v1_export_2016_04_02_study_data.xlsx")
  res0 = readxl::read_xlsx(file, sheet = "STUDY INFORMATION")
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  res = res0 %>%
    dplyr::mutate(
      # Add/rename basic columns as needed
      source_study_id = `STUDY #`,
      subsource = `DOCUMENT SOURCE`,
      quality = `DATA QUALITY`,
      casrn = `REGISTRY NUMBER`,
      long_ref = `STUDY REFERENCE`,
      title = `STUDY TITLE`,
      year = `YEAR (REPORT/CITATION)` %>%
        as.character() %>%
        gsub("1697", "1967", .) %>%
        as.numeric(),
      source_url = "https://www.ng.cosmosdb.eu/",

      # COMMENT OUT IF DATA QUALITY TOO POOR
      # Treat STUDY RESULT COMMENTS`field as critical_effect
      critical_effect = dplyr::case_when(
        # Filter for list of key terms generally found in actual effect descriptions
        grepl(paste0("DEMONSTRATE|INCREASE|DECREASE|CHANGE|TOXICITY|DARK|NECROSIS|INFLAMMATION|",
                     "DEFECTIVE|\\bDIED|ABNORMALITIES|ATROPHY|TUMOR|VOMIT|LESIONS|OBSERVED|DISRUPTION|",
                     "SWOLLEN|CONGESTED|ENLARGED|HISTOPATH|NEOPLASIA|MUTATION|hypertrophy|",
                     "TERATOGENESIS|ANOPTHTHALOMIA|DELAY|dyscrasia|critical effect|DYSTROPHIC"),
              `STUDY RESULT COMMENTS`, ignore.case = TRUE) ~ `STUDY RESULT COMMENTS`,
        TRUE ~ as.character(NA)
      ) %>%
        gsub('"|\\*', "", .) %>%
        stringr::str_squish(),

      # Remove string NA values
      `ROUTE OF EXPOSURE` = gsub("NA", as.character(NA), `ROUTE OF EXPOSURE`),
      DURATION = gsub("NA", as.character(NA), DURATION),
      STRAIN = gsub("NA", as.character(NA), STRAIN),

      # Extract preferred name if available (otherwise, use first name listed)
      name = dplyr::case_when(
        grepl("Preferred", `TEST SUBSTANCE NAME`) ~ `TEST SUBSTANCE NAME` %>%
          gsub(" \\(Preferred Term\\)(?:.+)?", "", .) %>%
          gsub(".+\\(INCI\\);", "", .) %>%
          stringr::str_squish(),
        grepl("INCI", `TEST SUBSTANCE NAME`) ~ `TEST SUBSTANCE NAME` %>%
          gsub(" \\(INCI\\)(?:.+)?", "", .) %>%
          stringr::str_squish(),
        TRUE ~ `TEST SUBSTANCE NAME`
      ),

      # Handle typo in data to ensure accurate split
      `STUDY RESULTS` = gsub("NOEL;", "NOEL:", `STUDY RESULTS`)
    ) %>%

    # Split results into different rows (one per toxval_type)
    tidyr::separate_rows(
      `STUDY RESULTS`,
      sep = ";|\\|"
    ) %>%

    # Remove entries without valid STUDY RESULTS values
    tidyr::drop_na(`STUDY RESULTS`) %>%
    dplyr::filter(
      !grepl("Positive|Negative", `STUDY RESULTS`),
      !grepl("Not established", `STUDY RESULTS`),
    ) %>%

    # Prepare STUDY RESULTS column for splitting
    dplyr::mutate(
      study_results_split = `STUDY RESULTS` %>%
        gsub("NOEL=", "NOEL:", .) %>%
        gsub(":", " ", .) %>%
        gsub("%", " %", .) %>%
        gsub("m", " m", .) %>%
        gsub("\\.$", "", .) %>%
        stringr::str_squish() %>%
        fix.replace.unicode()
    ) %>%

    # Get toxval_type, toxval_numeric, and toxval_units from each row
    tidyr::separate(
      col = study_results_split,
      into = c("toxval_type", "toxval_numeric", "toxval_units"),
      sep = "\\s",
      remove = TRUE,
      fill = "right",
      extra = "merge"
    ) %>%

    # Drop entries without toxval_units
    tidyr::drop_na(toxval_units) %>%

    # Get study_duration_value and study_duration_units
    tidyr::separate(
      col = DURATION,
      into = c("study_duration_value", "study_duration_units"),
      sep = " ",
      remove = FALSE,
      fill = "left",
      extra = "merge"
    ) %>%

    # Get exposure_route and exposure_method from ROUTE OF EXPOSURE
    tidyr::separate(
      col = `ROUTE OF EXPOSURE`,
      into = c("exposure_route", "exposure_method"),
      sep = " - ",
      remove = FALSE,
      fill = "right"
    ) %>%

    # Conduct final cleaning
    dplyr::mutate(
      # Clean toxval_units (handle edge cases)
      toxval_units = toxval_units %>%
        gsub("\\(conversion\\)", "", .) %>%
        gsub("LOEL.+", "", .) %>%
        gsub(" m", "m", .) %>%
        gsub("mmole", "mmol", .) %>%
        gsub("\\/l", "/L", .) %>%
        gsub("mL", "ml", .) %>%
        gsub("kd", "kg", .) %>%
        gsub("Molar \\(M\\)", "mol", .) %>%
        gsub("M", "mol", .) %>%
        gsub("-", " ", .) %>%
        gsub("kg day", "kg\\/day", .) %>%
        stringr::str_squish(),

      # UNCOMMENT TO USE LOWER VALUE FOR STUDY_DURATION_VALUE
      # study_duration_value = study_duration_value %>%
      #   gsub("\\-.+", "", .) %>%
      #   as.numeric(),

      # UNCOMMENT TO USE HIGHER VALUE FOR STUDY_DURATION_VALUE
      # study_duration_value = study_duration_value %>%
      #   gsub(".+\\-", "", .) %>%
      #   as.numeric(),

      # Ensure toxval_numeric is of numeric type
      toxval_numeric = as.numeric(toxval_numeric),
      exposure_route = tolower(exposure_route),
      exposure_method = tolower(exposure_method),
      # Remove excess whitespace
      dplyr::across(where(is.character), stringr::str_squish)
    ) %>%
    dplyr::distinct()

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
                       hashing_cols=c(toxval.config()$hashing_cols, "source_study_id"))
}




