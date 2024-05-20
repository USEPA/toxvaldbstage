#--------------------------------------------------------------------------------------
#' @description Load PFAS 150 SEM V2 Source data into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_pfas_150_sem_v2_source
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
#'  \code{\link[stringr]{str_trim}}
#' @rdname import_pfas_150_sem_v2_source
#' @export
#' #' @importFrom openxlsx read.xlsx
#' @importFrom digest digest
#' @importFrom generics is.element
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_squish
#' @importFrom dplyr rename distinct mutate left_join case_when select rename_with bind_cols bind_rows across na_if where
#' @importFrom tidyselect matches
#' @importFrom tidyr unite drop_na replace_na
#--------------------------------------------------------------------------------------
import_pfas_150_sem_v2_source <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "PFAS 150 SEM v2"
  source_table = "source_pfas_150_sem_v2"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2022-05-17")
  dir = paste0(toxval.config()$datapath,"pfas_150_sem_v2/pfas_150_sem_v2_files/")

  # Read separate files for experimental data, chemical information, and citation
  file_chems = paste0(dir,"PFAS 150 SEM chemicals.xlsx")
  res0_chems = readxl::read_xlsx(file_chems)

  file_results = paste0(dir,"PFAS 150 SEM results.xlsx")
  res0_results = readxl::read_xlsx(file_results)

  file_hero = paste0(dir,"PFAS 150 SEM HERO ID vs citation.xlsx")
  res0_hero = readxl::read_xlsx(file_hero)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  # Prepare chemical/citation files for merging
  res_chems = res0_chems %>%
    dplyr::rename(chemical_name = name) %>%
    dplyr::distinct()
  res_hero = res0_hero %>%
    dplyr::rename(long_ref = Citation, hero_id = `HERO ID`) %>%
    dplyr::mutate(hero_id = as.character(hero_id)) %>%
    dplyr::distinct()

  res = res0_results %>%
    # Add dtxsid, casrn, and long_ref information from input files
    dplyr::left_join(res_chems, by="chemical_name") %>%
    dplyr::left_join(res_hero, by="hero_id") %>%

    dplyr::mutate(
      # Set missing long_ref values and clean entries
      long_ref = dplyr::case_when(
        long_ref %in% c(as.character(NA), "-", "") ~ citation,
        TRUE ~ long_ref
      ) %>%
        gsub("(?:\\.,)+", ".,", .) %>%
        gsub("\\b\\.,\\b", "", .) %>%
        gsub("M, \\.,", "M.,", .) %>%
        gsub("\\.,\\.", ".", .) %>%
        stringr::str_squish()
    )

  # Extract columns that match each set of observations
  res_basic = res %>%
    dplyr::select(!tidyselect::matches("[1-2]_(?:system|study)") & !tidyselect::matches("level_[1-2]")) %>%
    dplyr::select(!tidyselect::matches("effect_level_rationale"))

  # Extract columns for each separate set of observations
  res_study_1 = res %>%
    dplyr::select(tidyselect::matches("1_study|study_effect_level_1|study_effect_level_rationale")) %>%
    dplyr::rename_with(~gsub("_[1-2]_(?:study|system)|_level_[1-2]", "", .)) %>%
    dplyr::rename(effect_level_rationale = study_effect_level_rationale) %>%
    dplyr::mutate(toxval_subtype = "Study-level NOAEL") %>%
    dplyr::bind_cols(res_basic)
  res_study_2 = res %>%
    dplyr::select(tidyselect::matches("2_study|study_effect_level_2|study_effect_level_rationale")) %>%
    dplyr::rename_with(~gsub("_[1-2]_(?:study|system)|_level_[1-2]", "", .)) %>%
    dplyr::rename(effect_level_rationale = study_effect_level_rationale) %>%
    dplyr::mutate(toxval_subtype = "Study-level LOAEL") %>%
    dplyr::bind_cols(res_basic)
  res_system_1 = res %>%
    dplyr::select(tidyselect::matches("1_system|system_effect_level_1|system_effect_level_rationale")) %>%
    dplyr::rename_with(~gsub("_[1-2]_(?:study|system)|_level_[1-2]", "", .)) %>%
    dplyr::rename(effect_level_rationale = system_effect_level_rationale) %>%
    dplyr::mutate(toxval_subtype = "System-level NOAEL") %>%
    dplyr::bind_cols(res_basic)
  res_system_2 = res %>%
    dplyr::select(tidyselect::matches("2_system|system_effect_level_2|system_effect_level_rationale")) %>%
    dplyr::rename_with(~gsub("_[1-2]_(?:study|system)|_level_[1-2]", "", .)) %>%
    dplyr::rename(effect_level_rationale = system_effect_level_rationale) %>%
    dplyr::mutate(toxval_subtype = "System-level LOAEL") %>%
    dplyr::bind_cols(res_basic)

  # Combine observation sets
  res = dplyr::bind_rows(res_study_1, res_study_2, res_system_1, res_system_2) %>%
    # Remove "_original" fields
    dplyr::select(-dplyr::contains("_original")) %>%
    dplyr::mutate(
      # Extract appropriate "effect" from study/system_effect
      effect = dplyr::case_when(
        grepl("System", toxval_subtype) ~ system_effect,
        grepl("Study", toxval_subtype) ~ study_effect,
        TRUE ~ as.character(NA)
      ),

      # Set "unknown" values as NA
      dplyr::across(c("strain", "sex", "generation", "endpoint_system", "endpoint_organ", "endpoint"),
                    ~dplyr::na_if(., "unknown") %>%
                      dplyr::na_if("-")),

      # Translate sex values
      sex = dplyr::case_when(
        grepl("M", sex) & grepl("F", sex) ~ "male/female",
        grepl("F", sex) ~ "female",
        grepl("M", sex) ~ "male",
        TRUE ~ as.character(NA)
      ),

      # Handle GD/LD/PNW study_duration values
      study_duration = study_duration_units %>%
        gsub("D([0-9])", "D \\1", .),
      study_duration_value = dplyr::case_when(
        !is.na(study_duration_value) ~ as.character(study_duration_value),
        grepl("GD [0-9]+\\-[0-9]+", study_duration) ~ stringr::str_extract(study_duration,
                                                                           "GD ([0-9]+\\-[0-9]+)",
                                                                           group=1),
        grepl("LD|PNW", study_duration) ~ stringr::str_extract(study_duration,
                                                               "(GD [0-9]+\\-(?:LD|PNW) [0-9]+)",
                                                               group=1),
        TRUE ~ as.character(study_duration_value)
      ),
      study_duration_units = dplyr::case_when(
        !grepl("GD", study_duration) ~ study_duration_units,
        grepl("GD [0-9]+\\-[0-9]+", study_duration) ~ "GD",
        grepl("LD|PNW", study_duration) ~ gsub("(GD) [0-9]+\\-(LD|PNW) [0-9]+", "\\1,\\2", study_duration),
        TRUE ~ study_duration_units
      ),

      # Add hard-coded fields
      name = chemical_name,
      external_source_id = hero_id,
      external_source_id_desc = "HERO ID",
      source_url = "https://ehp.niehs.nih.gov/doi/full/10.1289/EHP10343",
      population = animal_group_name,
      toxval_numeric = as.numeric(toxval_numeric)
    ) %>%
    # Build critical_effect field
    tidyr::unite("critical_effect",
                 generation, endpoint_system, endpoint_organ, endpoint,
                 sep = ": ",
                 remove = FALSE,
                 na.rm = TRUE) %>%
    # Drop intermediate columns
    dplyr::select(-c(study_effect, system_effect, study_duration)) %>%
    # Drop rows that do not have necessary ToxVal information
    tidyr::drop_na(toxval_numeric, toxval_type, toxval_units)

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

  # Perform deduping/collapse critical_effect
  hashing_cols = c(toxval.config()$hashing_cols[!(toxval.config()$hashing_cols %in% c("critical_effect"))])
  res = toxval.source.import.dedup(res, hashing_cols=hashing_cols) %>%
    # Replace "|::|" in critical_effect with "|" delimiter
    dplyr::mutate(
      critical_effect = critical_effect %>%
        gsub(" \\|::\\| ", "|", .)
    )

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
