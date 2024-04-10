#--------------------------------------------------------------------------------------
#' @title import_atsdr_pfas_2021_source
#' @description Load ATSDR PFAS 2021 data to toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @return None; data is pushed to ToxVal
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[janitor]{remove_empty}}
#'  \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{arrange}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{group_by}}
#'  \code{\link[tidyr]{separate_rows}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{drop_na}}
#'  \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_extract}}, \code{\link[stringr]{str_replace}}
#'  \code{\link[tibble]{enframe}}
#'  \code{\link[tidyselect]{all_of}}
#' @rdname import_atsdr_pfas_2021_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom janitor remove_empty
#' @importFrom dplyr bind_rows summarise mutate arrange distinct case_when select group_by ungroup
#' @importFrom tidyr separate_rows separate pivot_longer drop_na
#' @importFrom stringr str_squish str_extract str_replace_all
#' @importFrom tibble deframe
#' @importFrom tidyselect any_of
#--------------------------------------------------------------------------------------
import_atsdr_pfas_2021_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "ATSDR PFAS 2021"
  source_table = "source_atsdr_pfas_2021"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2021-05-01")
  dir = paste0(toxval.config()$datapath,"atsdr_pfas/atsdr_pfas_files/")

  # Get all 6 filepaths
  file1 = paste0(dir,"ATSDR_TP_2021_Perfluoroalkyls_Oral.xlsx")
  file2 = paste0(dir,"ATSDR_TP_2021_PFNA_Inhalation.xlsx")
  file3 = paste0(dir,"ATSDR_TP_2021_PFOA_Dermal.xlsx")
  file4 = paste0(dir,"ATSDR_TP_2021_PFOA_Inhalation.xlsx")
  file5 = paste0(dir,"ATSDR_TP_2021_PFOA_Oral.xlsx")
  file6 = paste0(dir,"ATSDR_TP_2021_PFOS_Oral.xlsx")

  # Read in initial data
  res1 = readxl::read_xlsx(file1, col_names=FALSE, skip=9, n_max=262, col_types="text")
  res2 = readxl::read_xlsx(file2, col_names=FALSE, skip=9, n_max=4, col_types="text")
  res3 = readxl::read_xlsx(file3, col_names=FALSE, skip=7, n_max=22, col_types="text")
  res4 = readxl::read_xlsx(file4, col_names=FALSE, skip=9, n_max=27, col_types="text")
  res5 = readxl::read_xlsx(file5, col_names=FALSE, skip=8, n_max=225, col_types="text")
  res6 = readxl::read_xlsx(file6, col_names=FALSE, skip=8, n_max=194, col_types="text")

  # Remove empty columns with no data/header
  res1 = res1 %>% janitor::remove_empty(which="cols")
  res2 = res2 %>% janitor::remove_empty(which="cols")
  res3 = res3 %>% janitor::remove_empty(which="cols")
  res4 = res4 %>% janitor::remove_empty(which="cols")
  res5 = res5 %>% janitor::remove_empty(which="cols")
  res6 = res6 %>% janitor::remove_empty(which="cols")

  # Rename columns for each source
  # 22 cols
  name_list1 = c("source_name_sid", "casrn","name","source_url","subsource","source_name_cid",
                "key_to_figure","study_type_exposure_route", "short_name", "short_ref",
                "species_strain","sex","duration","doses","parameters_monitored","endpoint",
                "NOAEL","LOAEL_less_serious","LOAEL_serious","effect","comments","long_ref")
  names(res1) = name_list1
  res1$filename = "ATSDR_TP_2021_Perfluoroalkyls_Oral.xlsx"

  # missing "comments", exposure_form used instead of exposure_route
  name_list2 = c("source_name_sid", "casrn","name","source_url","subsource","source_name_cid",
                "key_to_figure","study_type_exposure_form", "short_name", "short_ref",
                "species_strain","sex","duration","doses","parameters_monitored","endpoint",
                "NOAEL","LOAEL_less_serious","LOAEL_serious","effect","long_ref")
  names(res2) = name_list2
  res2$filename = "ATSDR_TP_2021_PFNA_Inhalation.xlsx"

  # missing key_to_figure
  name_list3 = c("source_name_sid", "casrn","name","source_url","subsource","source_name_cid",
                "study_type_exposure_route", "short_name", "short_ref","species_strain","sex",
                "duration","doses","parameters_monitored","endpoint","NOAEL","LOAEL_less_serious",
                "LOAEL_serious","effect","comments","long_ref")
  names(res3) = name_list3
  res3$filename = "ATSDR_TP_2021_PFOA_Dermal.xlsx"

  # missing comments, study_type used instead of study_type_exposure_route
  name_list4 = c("source_name_sid", "casrn","name","source_url","subsource","source_name_cid",
                 "key_to_figure","study_type", "short_ref", "short_name","species_strain","sex",
                 "duration","doses","parameters_monitored","endpoint","NOAEL","LOAEL_less_serious",
                 "LOAEL_serious","effect","long_ref")
  names(res4) = name_list4
  res4$filename = "ATSDR_TP_2021_PFOA_Inhalation.xlsx"

  # Same as 1
  name_list5 = c("source_name_sid", "casrn","name","source_url","subsource","source_name_cid",
                  "key_to_figure","study_type_exposure_route", "short_name", "short_ref",
                  "species_strain","sex","duration","doses","parameters_monitored","endpoint",
                  "NOAEL","LOAEL_less_serious","LOAEL_serious","effect","comments","long_ref")
  names(res5) = name_list5
  res5$filename = "ATSDR_TP_2021_PFOA_Oral.xlsx"

  # Same as 1
  name_list6 = c("source_name_sid", "casrn","name","source_url","subsource","source_name_cid",
                 "key_to_figure","study_type_exposure_route", "short_name", "short_ref",
                 "species_strain","sex","duration","doses","parameters_monitored","endpoint",
                 "NOAEL","LOAEL_less_serious","LOAEL_serious","effect","long_ref","comments")
  names(res6) = name_list6
  res6$filename = "ATSDR_TP_2021_PFOS_Oral.xlsx"

  #####################################################################
  cat("Build combined dataframe of all atsdr pfas 2021 sources \n")
  #####################################################################
  res0 = dplyr::bind_rows(res1, res2, res3, res4, res5, res6)

  # Read individual key tables and combine values into single row
  key1 = readxl::read_xlsx(file1, range="H281:H293", col_names=c("Key")) %>%
    dplyr::summarise(Key = paste(Key, collapse = " "))
  key2 = readxl::read_xlsx(file2, range="H18:H19", col_names=c("Key")) %>%
    dplyr::summarise(Key = paste(Key, collapse = " "))
  key3 = readxl::read_xlsx(file3, range="D32:D32", col_names=c("Key")) %>%
    dplyr::summarise(Key = paste(Key, collapse = " "))
  key4 = readxl::read_xlsx(file4, range="H40:H44", col_names=c("Key")) %>%
    dplyr::summarise(Key = paste(Key, collapse = " "))
  key5 = readxl::read_xlsx(file5, range="H239:H248", col_names=c("Key")) %>%
    dplyr::summarise(Key = paste(Key, collapse = " "))
  key6 = readxl::read_xlsx(file6, range="H210:H219", col_names=c("Key")) %>%
    dplyr::summarise(Key = paste(Key, collapse = " "))

  # Build aggregate key table, split into k/v pairs, and remove duplicates
  agg_key = dplyr::bind_rows(key1, key2, key3, key4, key5, key6) %>%
    tidyr::separate_rows(
      Key,
      sep = ";"
    ) %>%
    tidyr::separate(
      col=Key,
      into=c("Key", "Value"),
      sep="=",
    ) %>%
    tidyr::separate_rows(
      Key,
      sep = "or"
    ) %>%
    dplyr::mutate(
      Key = stringr::str_squish(Key) %>% paste0("\\b", ., "\\b"),
      Value = stringr::str_squish(Value)
    ) %>%
    dplyr::arrange(Key) %>%
    # Keep only first appearance of key; value differences do not hold important information
    dplyr::distinct(Key, .keep_all = TRUE) %>%
    tibble::deframe()
  # Add extra key to handle different Musc/skel capitalization
  agg_key["Musc/Skel"] = "musculoskeletal"

  res = res0 %>%
    dplyr::mutate(
      # Extract exposure_route/form and study type info from study_type_exposure_route
      exposure_route = study_type_exposure_route %>%
        gsub("(.*\\-\\s+)(.*)", "\\2", .) %>%
        tolower() %>%
        stringr::str_squish(),
      exposure_form = study_type_exposure_form %>%
        gsub("(.*\\s+\\(+)(.*)(\\s*\\)+)", "\\2", .) %>%
        stringr::str_squish(),

      study_type = dplyr::case_when(
        !is.na(study_type) ~ study_type,
        !is.na(study_type_exposure_form) ~ gsub("(.*)(\\s+\\(+.*)", "\\1", study_type_exposure_form),
        !is.na(study_type_exposure_route) ~ gsub("(.*)(\\s+\\-\\s+.*)", "\\1", study_type_exposure_route),
        TRUE ~ as.character(NA)
      ) %>%
        gsub("\\s.+", "", .) %>%
        tolower() %>%
        stringr::str_squish(),

      # Extract species and strain info from species_strain
      species = dplyr::case_when(
        filename == "ATSDR_TP_2021_PFOA_Dermal.xlsx" ~ gsub("(.*)(\\s+\\(+.*)", "\\1", species_strain),
        filename == "ATSDR_TP_2021_PFOS_Oral.xlsx" ~ species_strain %>%
          gsub("(potassium salt 10\\s+)(.*)", "\\2", .) %>%
          gsub("(.*?)(\\s+\\(.*)", "\\1", .),
        TRUE ~ gsub("(.*?)(\\s+.*)", "\\1", species_strain)
      ) %>% tolower() %>% stringr::str_squish(),

      strain = dplyr::case_when(
        filename == "ATSDR_TP_2021_PFOS_Oral.xlsx" ~ species_strain %>%
          gsub("(.*\\s+)(\\(.*)", "\\2", .) %>%
          gsub("(^\\()(.*)(\\)$)", "\\2", .) %>%
          gsub("(^\\()(.*)", "\\2", .),
        TRUE ~ species_strain %>%
          gsub("(.*?\\s+)(.*)", "\\2", .) %>%
          gsub("^\\s+|\\s+$", "", .) %>%
          gsub("(^\\()(.*)(\\)$)", "\\2", .),
      ),
      strain = dplyr::case_when(
        grepl("\\([^\\)][A-Za-z]+$", strain) ~ gsub("(.*)($)", "\\1", strain),
        TRUE ~ strain
      ) %>%
        gsub("Sprague\\s?Dawley", "Sprague Dawley", .) %>%
        fix.replace.unicode() %>%
        stringr::str_squish(),

      # Translate sex info
      sex = dplyr::case_when(
        grepl("M", sex) & grepl("F", sex) ~ "male/female",
        grepl("M", sex) ~ "male",
        grepl("F", sex) ~ "female",
        TRUE ~ as.character(NA)
      ),

      # Extract document name from source url
      extraction_document_name = gsub("(.*\\/)(.*)", "\\2", source_url),

      # Extract exposure_method from duration (File 1 only)
      exposure_method = dplyr::case_when(
        filename == "ATSDR_TP_2021_Perfluoroalkyls_Oral.xlsx" ~ gsub("(.*\\s*)(\\(.*\\))(\\s*.*)", "\\2", duration),
        TRUE ~ as.character(NA)
      ),

      # Extract study_duration_value and study_duration_units from duration field
      study_duration_value = dplyr::case_when(
        grepl("1, 3, 7", duration) ~ "1-7",
        grepl("10, 13, 15", duration) ~ "10-15",
        grepl("[0-9]+\\s*\\-?\\s*[0-9]*\\s*(?:day|hour|month|week|year)", duration) ~ stringr::str_extract(duration,
                                                                                        "([0-9]+\\s*\\-?\\s*[0-9]*)\\s*(?:day|hour|month|week|year)",
                                                                                        group=1),
        grepl("GD", duration) ~ duration %>%
          gsub("\\s?to\\s?PND|\\s?to\\s?PND", "-", .) %>%
          fix.replace.unicode() %>%
          stringr::str_extract("([0-9]+\\s*\\-?\\s*[0-9]*)", group=1),
        grepl("Once|Single dose", duration) ~ "1",
        TRUE ~ as.character(NA)
      ) %>% fix.replace.unicode() %>% gsub("\\s?\\-\\s?", "-", .) %>% gsub("\\s.+", "", .),

      # Follow same patterns as above for study_duration_units
      study_duration_units = dplyr::case_when(
        grepl("[0-9]+\\s*\\-?\\s*[0-9]*\\s*(?:day|hour|month|week|year)", duration) ~ stringr::str_extract(duration,
                                                                                                           "[0-9]+\\s*\\-?\\s*[0-9]*\\s*(day|hour|month|week|year)",
                                                                                                           group=1),
        grepl("GD", duration) ~ "GD",
        grepl("Once|Single dose", duration) ~ "day",
        TRUE ~ as.character(NA)
      )
    ) %>%

    # Fix GD study_duration values (subtract top range from low range)
    tidyr::separate(
      col="study_duration_value",
      into=c("study_duration_low", "study_duration_high"),
      sep="-",
      remove=FALSE,
      fill="right"
    ) %>%
    dplyr::mutate(
      study_duration_low = as.numeric(study_duration_low),
      study_duration_high = as.numeric(study_duration_high),

      study_duration_value = dplyr::case_when(
        study_duration_units == "GD" ~ as.character(study_duration_high - study_duration_low + 1),
        TRUE ~ study_duration_value
      ) %>% stringr::str_squish(),

      study_duration_units = gsub("GD", "day", study_duration_units)
    ) %>%
    dplyr::select(!tidyselect::any_of(c("study_duration_low", "study_duration_high"))) %>%

    dplyr::mutate(
      # Get critical_effect and toxval_details
      critical_effect = effect,
      toxval_details = comments,

      # Assign toxval_units based on file
      toxval_units = dplyr::case_when(
        grepl("Inhalation", filename) ~ "mg/m3",
        grepl("Oral", filename) ~ "mg/kg/day",
        grepl("Dermal", filename) ~ ifelse(grepl("mg.kg.day", doses), "mg/kg/day",
                                           ifelse(grepl("mg.kg", doses), "mg/kg", "mg")),
        TRUE ~ as.character(NA)
      ),

      # Clean "doses" col
      doses = doses %>%
        # Remove units (units listed in doses match original toxval_units)
        gsub("mg\\/kg(?:\\/day)?|mg|kg", "", .) %>%
        # Remove other noise
        gsub("\\(.+\\)", "", .) %>%
        gsub("and", ",", .),

      # Move doses key values to parameters_monitored
      parameters_monitored = dplyr::case_when(
        grepl("\\s[a-zA-Z\\s,]+$", doses) ~ paste0(parameters_monitored,
                                                   ", ",
                                                   stringr::str_extract(doses, "\\s([a-zA-Z\\s,]+$)", group=1)),
        TRUE ~ parameters_monitored
      ) %>%
        gsub(",?\\s?NA,?", "", .) %>%
        stringr::str_squish(),

      # Make key replacements
      endpoint = endpoint %>%
        stringr::str_replace_all(!!agg_key) %>%
        stringr::str_squish(),
      paramaters_monitored = parameters_monitored %>%
        stringr::str_replace_all(!!agg_key) %>%
        stringr::str_squish(),
      exposure_method = exposure_method %>%
        stringr::str_replace_all(!!agg_key) %>%
        gsub("\\(|\\)", "", .) %>%
        stringr::str_squish(),

      # Set exposure_route for entries that do not have on assigned
      exposure_route = dplyr::case_when(
        !is.na(exposure_route) ~ exposure_route,
        grepl("Inhalation", filename) ~ "Inhalation",
        grepl("Oral", filename) ~ "Oral",
        grepl("Dermal", filename) ~ "Dermal",
        TRUE ~ as.character(NA)
      ) %>% tolower(),

      # Extract year info from short_ref
      year = gsub("(.*)([0-9]{4})(.*)", "\\2", short_ref),

      # Clean long_ref
      long_ref = long_ref %>%
        gsub("^\\+", "", .) %>%
        fix.replace.unicode() %>%
        stringr::str_squish(),

      # Handle special doses case
      doses = dplyr::case_when(
        grepl("^5 days.*at PND 21 \\(GW\\)", duration) ~ "0, 1, 5, 10",
        grepl("^5 days.*0\\, 5 4 weeks \\(GW\\)", duration) ~ "0, 5",
        TRUE ~ doses
      ) %>%
        # Remove key values
        gsub("\\s[a-zA-Z\\s,]+$", "", .) %>%
        stringr::str_squish(),

      # Fix error with sex/species switch (one case, so hard-code solution)
      sex = dplyr::case_when(
        grepl("2\\-5 F", species) ~ "female",
        TRUE ~ sex
      ),
      strain = dplyr::case_when(
        grepl("2\\-5 F", species) ~ "C57BL/6",
        TRUE ~ strain
      ),
      species = dplyr::case_when(
        grepl("2\\-5 F", species) ~ "mouse",
        TRUE ~ species
      ),

      # Remove incorrect exposure_method entries
      exposure_method = dplyr::case_when(
        grepl("day|time", exposure_method) ~ as.character(NA),
        TRUE ~ exposure_method
      )
    ) %>%

    # Get toxval_numeric and toxval_type
    tidyr::pivot_longer(
      cols = c("NOAEL", "LOAEL_less_serious", "LOAEL_serious"),
      names_to = "toxval_type",
      values_to = "toxval_numeric",
    ) %>%

    dplyr::mutate(
      # Override sex according to toxval_numeric
      sex = dplyr::case_when(
        grepl("M", toxval_numeric) & grepl("F", toxval_numeric) ~ "male/female",
        grepl("M", toxval_numeric) ~ "male",
        grepl("F", toxval_numeric) ~ "female",
        TRUE ~ sex
      ),

      # Clean toxval_numeric
      toxval_numeric = dplyr::case_when(
        toxval_numeric == "Develop" ~ as.character(NA),
        TRUE ~ toxval_numeric
      ) %>%
        gsub(",", "", .) %>%
        gsub("\\s.+", "", .) %>%
        gsub("[M|F|c]", "", .),

      # Ensure final values are of correct type
      toxval_numeric = as.numeric(toxval_numeric),
      year = as.integer(year),

      # Comment out if splitting toxval_type into toxval_subtype
      toxval_type = toxval_type %>%
        gsub("_less_serious", " Less Serious", .) %>%
        gsub("_serious", " Serious", .),

      # Uncomment if splitting toxval_type into toxval_subtype
      # # Separate "less_serious" and "serious" into toxval_subtype
      # toxval_subtype = dplyr::case_when(
      #   grepl("_less_serious", toxval_type) ~ "Less Serious",
      #   grepl("_serious", toxval_type) ~ "Serious",
      #   TRUE ~ as.character(NA)
      # ),
      # toxval_type = gsub("_.+", "", toxval_type),

      # Use information stored in ref to fix line with shifted data
      # Ref: Zhao Y, Tan YS, Haslam SZ, et al. 2010. Perfluorooctanoic
      # acid effects on steroid hormone and growth factor levels mediate
      # stimulation of peripubertal mammary gland development in C57B1/6 mice.
      # Toxicol Sci 115(1):214-224
      strain = dplyr::case_when(
        species == "2-5" ~ "C57B1/6",
        TRUE ~ strain
      ),
      sex = dplyr::case_when(
        species == "2-5" ~ "female",
        TRUE ~ sex
      ),
      species = dplyr::case_when(
        species == "2-5" ~ "mouse",
        TRUE ~ species
      ),

      # Add columns previously handled in load
      human_eco = "human_health",
      toxval_numeric_qualifier = "=",
      risk_assessment_class = study_type,
      source_url = "https://www.atsdr.cdc.gov/toxprofiledocs/index.html"
    ) %>%

    # Drop entries without required ToxVal information
    tidyr::drop_na(toxval_type, toxval_numeric, toxval_units) %>%
    dplyr::distinct()

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"

  # Perform deduping
  res = toxval.source.import.dedup(res, dedup_fields=c("endpoint"), delim="; ")

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
