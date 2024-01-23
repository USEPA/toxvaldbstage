#--------------------------------------------------------------------------------------
#' @description Load DOD MEG to toxval_source.
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_dod_meg_source
#' @return None. Data is processed into the database
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{distinct}}
#'  \code{\link[stringr]{str_trim}}
#'  \code{\link[tidyr]{unite}}
#' @rdname import_dod_meg_source
#' @export
#' @importFrom readxl read_xls
#' @importFrom dplyr mutate case_when distinct
#' @importFrom stringr str_squish
#' @importFrom tidyr unite
#--------------------------------------------------------------------------------------
import_dod_meg_source <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "DOD"
  source_table = "source_dod_meg"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2013-01-01")
  dir = paste0(toxval.config()$datapath,"dod_meg/dod_meg_files/")

  dir = ""
  file = paste0(dir,"TG230MilitaryExposureGuidelines.xls")
  res0 = readxl::read_xls(file, sheet = "All MEGs (vertical)")
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  res <- res0 %>%
    dplyr::mutate(
      # Get rid of excess whitespace
      dplyr::mutate(dplyr::across(where(is.character), stringr::str_squish)),
      # Assign appropriate names
      name = TG230_CHEMICAL_NAME,
      casrn = TG230_CASRN,
      toxval_numeric = as.numeric(MEGvalue),
      toxval_units = UNITS,
      subsource = BASIS,
      exposure_method = MEDIA,
      meg_type = SEVERITY,
      duration = TIMEFRAME,
      toxval_type = "MEG",
      year = "2013",
      long_ref = "U.S. Army Public Health Command (2013) TG 230 Military Exposure Guidelines Table. Army Public Health Center.",
      species = "human",

      # Get appropriate subtype
      subtype = dplyr::case_when(
        grepl("min|hour|day", duration) ~ "Short-Term",
        grepl("year", duration) ~ "Long-Term",
      ),

      # Get appropriate exposure_route
      exposure_route = dplyr::case_when(
        exposure_method == "Air" ~ "Inhalation",
        exposure_method == "Soil" ~ "Soil",
        exposure_method == "Water" ~ "Oral"
      ),

      # Fix name field
      name = name %>%
        # Fix unicode symbols
        fix.replace.unicode() %>%

        # Handle extra whitespace
        stringr::str_squish(),

      # Fix CASRN - instead use checksum to narrow down
      casrn = casrn %>%
        # Remove *
        gsub("\\*+", "", .) %>%

        # Remove "numbering" (e.g., (2))
        gsub("\\s\\([0-9*]\\)", "", .) %>%

        # Handle extra whitespace
        stringr::str_squish(),

      # Fix subsource
      subsource = subsource %>%
        # Remove *
        gsub("\\*+", "", .) %>%

        # Fix symbols
        fix.replace.unicode() %>%

        # Remove trailing underscores
        gsub("_$", "", .) %>%

        # Handle extra whitespace
        stringr::str_squish(),

      # Get study_duration_value
      study_duration_value = gsub("[a-zA-Z]+", "", duration),

      # Get study_duration_units
      study_duration_units = gsub("[0-9]+", "", duration),

      # Add per day to rate
      Intake_Rate = paste0(Intake_Rate, "/d") %>%
        gsub("NA/d", NA, .)
    ) %>%

    # Unite subtype and intake_rate, and remove entries with no intake_rate
    tidyr::unite("toxval_subtype", c("subtype", "Intake_Rate"), sep = ", ", remove = FALSE) %>%
    tidyr::unite(toxval_subtype, c(toxval_subtype, meg_type, exposure_method), sep=" ", remove=FALSE) %>%
    dplyr::mutate(toxval_subtype = toxval_subtype %>%
                    gsub(", NA", "", .)) %>%

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
                       hashing_cols=toxval.config()$hashing_cols)
}




