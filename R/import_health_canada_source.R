#--------------------------------------------------------------------------------------
#' @title import_health_canada_source
#' @description Transforms and loads Health Canada data into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
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
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{na_if}}
#'  \code{\link[stringr]{str_extract}}, \code{\link[stringr]{str_trim}}, \code{\link[stringr]{modifiers}}, \code{\link[stringr]{str_count}}
#' @rdname import_health_canada_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate across where case_when na_if
#' @importFrom stringr str_extract str_squish regex str_count
#--------------------------------------------------------------------------------------
import_health_canada_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "Health Canada"
  source_table = "source_health_canada"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2010-09-01")
  dir = paste0(toxval.config()$datapath,"health_canada/health_canada_files/")
  file = paste0(dir,"HealthCanada_TRVs_2010_AppendixA v2.xlsx")
  sheets = readxl::excel_sheets(file)
  res0 = lapply(sheets, function(s) {
    readxl::read_xlsx(file, sheet=s)
  }) %>%
    dplyr::bind_rows()
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  res = res0 %>%
    dplyr::mutate(
      species_raw = species,
      # Fix unicode symbols in character fields
      dplyr::across(dplyr::where(is.character), fix.replace.unicode),

      long_ref = trv_source,

      # If routes are not needed for toxval_type, uncomment this logic
      # toxval_type = toxval_type %>%
      #   gsub("inhalation|oral|injection", "", .) %>%
      #   stringr::str_squish(),

      # Extract study_duration_value and study_duration_units
      study_duration = dplyr::case_when(
        # Add in hardcoding for certain values to match previous load script
        duration == "gestational days 0-17" ~ "17 gestational days",
        duration == "days 1-24 (rabbits) and 1-19 (rats) of gestational period" ~ "19 gestational days",
        duration == "1 gestational period, 48 d post-natal exposure" ~ "1 generations",
        duration == "2 years, 3 generations" ~ "3 generations",
        grepl("for [0-9]+ weeks", duration) ~ stringr::str_extract(duration, "for ([0-9]+ weeks)", group=1),
        grepl("F0", duration) ~ "2 generations",
        grepl("Duration and Dosing Regime:", duration) ~ NA,
        TRUE ~ duration
      ) %>%
        gsub("\\bto\\b", "-", ., ignore.case=TRUE) %>%
        gsub(" - ", "-", .) %>%
        gsub("\\bone\\b", "1", ., ignore.case=TRUE) %>%
        gsub("\\btwo\\b", "2", ., ignore.case=TRUE) %>%
        gsub("\\bthree\\b", "3", ., ignore.case=TRUE) %>%
        gsub("\\bfour\\b", "4", ., ignore.case=TRUE) %>%
        gsub("\\bfive\\b", "5", ., ignore.case=TRUE) %>%
        gsub("\\bsix\\b", "6", ., ignore.case=TRUE) %>%
        gsub("\\bseven\\b", "7", ., ignore.case=TRUE) %>%
        gsub("\\beight\\b", "8", ., ignore.case=TRUE) %>%
        gsub("\\bnine\\b", "9", ., ignore.case=TRUE) %>%
        gsub("\\bten\\b", "10", ., ignore.case=TRUE) %>%
        gsub("\\-\\s*week", " week", ., ignore.case=TRUE) %>%
        gsub("\\-\\s*hour", " hour", ., ignore.case=TRUE) %>%
        gsub("\\-\\s*day", " day", ., ignore.case=TRUE) %>%
        gsub("\\-\\s*month", " month", ., ignore.case=TRUE) %>%
        gsub("\\-\\s*year", " year", ., ignore.case=TRUE) %>%
        gsub("([0-9\\.,]+)(h|d|w|m|y)", "\\1 \\2", .) %>%
        gsub("[0-9\\.,]+\\s*\\(?(?:mg|kg|ppm|mg\\kg)\\)?,?", "", .) %>%
        stringr::str_squish(),
      # Use first number appearance (range possible) as study_duration_value
      study_duration_value = study_duration %>%
        stringr::str_extract(stringr::regex(paste0("([0-9\\.,]+(?:\\-[0-9\\.,]+)?)\\s?",
                                                   "(?:hour|\\bh\\b|[0-9\\.,]h\\b|",
                                                   "gestational day|",
                                                   "day|\\bd\\b|[0-9\\.,]d\\b|",
                                                   "week|\\bw\\b|[0-9\\.,]w\\b|wk|weeek|wwek|",
                                                   "month|\\bm\\b|[0-9\\.,]m\\b|",
                                                   "year|\\by\\b|[0-9\\.,]y\\b|yr|generations)"),
                                            ignore_case = TRUE), group=1) %>%
        c() %>% stringr::str_squish(),
      # Use first "timeframe" appearance as study_duration_units
      study_duration_units = study_duration %>%
        stringr::str_extract(stringr::regex(paste0("[0-9\\.,]+(?:\\-[0-9\\.,]+)?\\s?",
                                                   "(hour|\\bh\\b|[0-9\\.,]h\\b|",
                                                   "gestational day|",
                                                   "day|\\bd\\b|[0-9\\.,]d\\b|",
                                                   "week|\\bw\\b|[0-9\\.,]w\\b|wk|weeek|wwek|",
                                                   "month|\\bm\\b|[0-9\\.,]m\\b|",
                                                   "year|\\by\\b|[0-9\\.,]y\\b|yr|generations)"),
                                            ignore_case = TRUE), group=1) %>%
        c(),
      # Perform final processing
      study_duration_units = dplyr::case_when(
        grepl("generations", study_duration_units) ~ "generations",
        grepl("m", study_duration_units) ~ "months",
        grepl("h", study_duration_units) ~ "hours",
        grepl("d", study_duration_units) ~ "days",
        grepl("w", study_duration_units) ~ "weeks",
        grepl("y", study_duration_units) ~ "years",
        TRUE ~ as.character(NA)
      ),

      study_duration_units = dplyr::case_when(
        grepl("gestation", duration) & study_duration_units %in% c("days") ~ "gestational days",
        TRUE ~ study_duration_units
      ),

      # Extract study_duration_qualifier
      study_duration_qualifier = stringr::str_extract(duration, ">=|<=|<|>"),

      # Extract exposure_route and exposure_method
      exposure_route_raw = exposure_route,
      exposure_route = dplyr::case_when(
        exposure_route_raw == "nd" ~ stringr::str_extract(exposure_route_raw, "inhalation|injection|oral"),
        grepl("oral|oal|drinking water|various", exposure_route_raw) ~ "oral",
        grepl("injection", exposure_route_raw) ~ "injection",
        grepl("inhalation", exposure_route_raw) ~ "inhalation",
        TRUE ~ as.character(NA)
      ),
      exposure_method = dplyr::case_when(
        grepl("various", exposure_route_raw) ~ stringr::str_extract(exposure_route_raw, "\\((.+)\\)",
                                                                    group=1),
        TRUE ~ exposure_route_raw
      ) %>%
        gsub(".+:|Cr\\(IV\\) in | injection", "", .) %>%
        dplyr::na_if("nd") %>%
        stringr::str_squish(),

      # Clean critical_effect values with open parentheses
      critical_effect = dplyr::case_when(
        grepl("\\(", critical_effect) & !grepl("\\)", critical_effect) ~ gsub("\\(", "", critical_effect),
        grepl("\\)", critical_effect) & !grepl("\\(", critical_effect) ~ gsub("\\)", "", critical_effect),
        stringr::str_count(critical_effect, "\\(") == 6 ~ gsub("(.*\\(.*\\)\\;\\s+)(\\()(.*)",
                                                               "\\1\\3", critical_effect),
        TRUE ~ critical_effect
      ),

      # Extract sex, strain, and lifestage from species, where applicable
      sex = stringr::str_extract(species, "(?:fe)?male") %>% c(),
      strain = stringr::str_extract(species, "Wistar|rhesus") %>% c(),
      lifestage = stringr::str_extract(species, "adult|children|infant") %>% c(),
      # Extract lifestage qualifiers from toxval_type
      lifestage = dplyr::case_when(
        grepl("(infants and children)", toxval_type, fixed=TRUE) ~ "infants and children",
        grepl("(adult)", toxval_type, fixed=TRUE) ~ "adult",
        TRUE ~ lifestage
      ),
      # Remove lifestage qualifiers
      toxval_type = toxval_type %>%
        gsub("\\(infants and children\\)", "", .) %>%
        gsub("\\(adult\\)", "", .) %>%
        stringr::str_squish(),

      # Clean species
      species = species %>%
        gsub("\\(.+|rhesus|volunteers|epidem.+", "", .) %>%
        gsub(" and ", ", ", .) %>%
        stringr::str_squish() %>%
        tolower(),

      # Extract study_type from study_duration_class
      study_type = study_duration_class,

      # Replace nd/NA values with actual NA
      dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., "nd")),
      dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., "NA")),

      toxval_numeric = as.numeric(toxval_numeric)
    )

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"

  # Handle deduping
  res = toxval.source.import.dedup(res)

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
