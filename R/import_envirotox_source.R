#--------------------------------------------------------------------------------------
#' @title import_envirotox_source
#' @description Load EnviroTox.V2 Source data into toxval_source
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
#'  \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{na_if}}, \code{\link[dplyr]{case_when}}
#'  \code{\link[stringr]{str_match}}, \code{\link[stringr]{str_trim}}
#' @rdname import_envirotox_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr left_join mutate na_if case_when
#' @importFrom stringr str_match str_squish
#--------------------------------------------------------------------------------------
import_envirotox_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "EnviroTox_v2"
  source_table = "source_envirotox"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2021-09-28")
  dir = paste0(toxval.config()$datapath,"envirotox/envirotox_files/")
  file = paste0(dir, "envirotox_20240102183325.xlsx")
  res0 = readxl::read_xlsx(file, sheet="test")
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  # Read in taxonomy sheet to get medium information
  res0_taxonomy = readxl::read_xlsx(file, sheet="taxonomy") %>%
    dplyr::select(`Latin name`, Medium)
  res0_merged = res0 %>%
    dplyr::left_join(res0_taxonomy,
                     by="Latin name")

  res <- res0_merged %>%
    dplyr::rename(long_ref = Source,
                  envirotox_version = version) %>%
    dplyr::mutate(
      # Perform initial renaming/basic transformations as needed
      name = `Chemical name` %>%
        # Select first chemical name before ";"
        sub(';.*', '', .) %>%
        fix.replace.unicode() %>%
        stringr::str_squish(),
      casrn = sapply(CAS, FUN=fix.casrn) %>% dplyr::na_if("NOCAS"),
      species = `Latin name`,
      critical_effect = Effect,
      toxval_numeric = `Effect value` %>% as.numeric(),
      toxval_units = Unit,
      toxval_type = gsub("\\*", "", `Test statistic`),
      # long_ref = "Health and Environmental Sciences Institute (HESI). 2024. EnviroTox Database & Tools. Version 2.0.0 Available: http://www.envirotoxdatabase.org/ (accessed January 02, 2024)",
      source_url = "https://envirotoxdatabase.org/",
      media = gsub("Both", "Freshwater/Saltwater", Medium),

      # Get study_type by translating "Test type" values
      study_type = dplyr::case_when(
        `Test type` == "A" ~ "acute",
        `Test type` == "C" ~ "chronic"
      ),

      # Get study_duration_value, study_duration_units, and study_duration_qualifier
      study_duration_qualifier = stringr::str_match(Duration, "[~<>=]+") %>% c(),
      study_duration_value = stringr::str_match(Duration, "[0-9\\.;]+") %>%
        gsub("[0-9\\.]+;", "", .) %>%
        as.numeric(),
      study_duration_units = Duration %>%
        gsub("NR", NA, .) %>%
        gsub("Day\\(s\\)", "days", .) %>%
        # Hardcode case
        gsub("until hatch", "hours until hatch", .) %>%
        stringr::str_match(., "[\\(\\)a-zA-Z\\-\\s]+") %>%
        c() %>%
        stringr::str_squish()
    )

  # study duration lists handling, use their reported days/hours
  res$study_duration_value[grepl(";", res$Duration) & grepl("days", res$study_duration_units)] = res$`Duration (days)`[grepl(";", res$Duration) & grepl("days", res$study_duration_units)]
  res$study_duration_value[grepl(";", res$Duration) & grepl("hours", res$study_duration_units)] = res$`Duration (hours)`[grepl(";", res$Duration) & grepl("hours", res$study_duration_units)]

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




