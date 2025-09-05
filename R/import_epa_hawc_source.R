#--------------------------------------------------------------------------------------
#' @title import_epa_hawc_source
#' @description Prepare EPA HAWC data from input dataframe list.
#' @param df_list List of dataframes of "raw", "doses" and "groups" from HAWC API for an assessment ID.
#' @return Returns processed HAWC data by assessment ID.
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{case_when}}
#'  \code{\link[tidyselect]{all_of}}
#'  \code{\link[tidyr]{pivot_wider}}, \code{\link[tidyr]{unite}}, \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{drop_na}}, \code{\link[tidyr]{separate_rows}}
#'  \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_detect}}, \code{\link[stringr]{str_extract}}
#'  \code{\link[purrr]{map2}}
#'  \code{\link[digest]{digest}}
#' @rdname import_hawc_pfas_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate select distinct arrange count left_join filter rename case_when
#' @importFrom tidyselect all_of
#' @importFrom tidyr pivot_wider unite pivot_longer separate drop_na separate_rows
#' @importFrom stringr str_squish str_detect str_extract
#' @importFrom purrr map2_chr
#' @importFrom digest digest
#' @importFrom openxlsx read.xlsx
#--------------------------------------------------------------------------------------
import_epa_hawc_source <- function(df_list=NULL) {
  if(is.null(df_list)) {
    return()
  }

  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  # Build original_hawc_pfas table from source file
  res_pfas3 = df_list$raw

  res_pfas3 = res_pfas3 %>%
    dplyr::mutate(
      # All closed empty square bracket entries to - in effects
      effects = gsub("^\\[\\]$", "-", effects),

      # Strip square brackets from beginning and end
      animal_group.experiment.study.searches = gsub("\\]|\\[", "", animal_group.experiment.study.searches),
      animal_group.experiment.study.identifiers = gsub("\\]|\\[", "", animal_group.experiment.study.identifiers),

      #variations in NA to NA
      # bmd_notes = gsub("[NA\\,\\\n]+", NA, bmd_notes),
      confidence_interval = gsub("[NA\\,\\\n]+", NA, confidence_interval),
      animal_group.experiment.study.block_id = gsub("[NA\\,\\\n]+", NA, animal_group.experiment.study.block_id)
    )

  # Read in dose data
  res_dose3 = df_list$doses %>%
    dplyr::select(tidyselect::all_of(c("dose_regime", "dose_group_id", "dose", "dose_units.name"))) %>%
    dplyr::distinct()

  # Read in dose groups
  res_groups3 <- df_list$groups %>%
    dplyr::select(#endpoint,
      dose_group_id) %>%
    dplyr::distinct()

  res_groups3[] = lapply(res_groups3, as.character)

  # Select relevant columns
  hawc_pfas = res_pfas3 %>%
    dplyr::select(dplyr::all_of(c("assessment","name" ,"organ","NOEL","LOEL",
                                       "FEL","url", "data_location",
                                       "bmds","animal_group.experiment.study.id","animal_group.experiment.study.title" ,"animal_group.experiment.study.authors_short",
                                       "animal_group.experiment.study.authors","animal_group.experiment.study.year","animal_group.experiment.study.journal",
                                       "animal_group.experiment.study.full_text_url","animal_group.experiment.study.short_citation","animal_group.experiment.study.full_citation",
                                       "animal_group.experiment.study.url","animal_group.experiment.name","animal_group.experiment.type",
                                       "animal_group.experiment.chemical","animal_group.experiment.cas","animal_group.experiment.chemical_source",
                                       "animal_group.experiment.vehicle","animal_group.experiment.guideline_compliance",
                                       "animal_group.dosing_regime.id","animal_group.dosing_regime.route_of_exposure",
                                       "animal_group.dosing_regime.duration_exposure","animal_group.dosing_regime.duration_exposure_text",
                                       "animal_group.species","animal_group.strain" ,"animal_group.sex","animal_group.name","animal_group.generation",
                                       "noel_names.noel","noel_names.loel")))

  # Create ordered dose_dict
  dose_dict <- res_dose3 %>%
    dplyr::mutate(dose_group_id = as.character(dose_group_id)) %>%
    dplyr::left_join(res_groups3,
                     by="dose_group_id") %>%
    dplyr::arrange(dose_regime, dose_units.name, dose)

  # Maintain unaltered dose_dict
  dose_dict_orig = dose_dict

  # Get counts of dose entries per dose_regime - units pairs
  dose_dict = dose_dict %>%
    dplyr::select(dose_regime, dose_units.name, dose) %>%
    dplyr::distinct() %>%
    dplyr::count(dose_regime, dose_units.name) %>%
    dplyr::mutate(dose_units.name_n = paste0("(", n, ") ", dose_units.name)) %>%
    dplyr::select(-n) %>%
    dplyr::left_join(dose_dict, by=c("dose_regime", "dose_units.name")) %>%
    # Combine doses by regime unit groups
    tidyr::pivot_wider(id_cols = c("dose_regime", "dose_units.name", "dose_units.name_n"),
                       names_from = "dose_group_id",
                       values_from = "dose") %>%
    tidyr::unite("dose",
                 -dose_regime, -dose_units.name, -dose_units.name_n,
                 sep=", ", na.rm = TRUE) %>%
    dplyr::mutate(dose = gsub(", NA", "", dose))

  # Helper function to manage the multiple dosing weirdness but maintain value-unit pairs
  split_dose_dose_units <- function(r, before_r = TRUE){
    # Split the input strings
    r = r %>%
      strsplit(split = ";") %>%
      unlist() %>%
      stringr::str_squish()
    # Value vs. units are separated by "||", select which is needed, before/after "||"
    if(before_r){
      lapply(r, function(x){ sub('\\|\\|.*', '', x) }) %>%
        paste(collapse = "; ") %>%
        return()
    } else {
      lapply(r, function(x){ sub('.*\\|\\|', '', x) }) %>%
        paste(collapse = "; ") %>%
        return()
    }
  }

  # Sort out dose value - unit pairs for regimes with multiples
  doses = dose_dict %>%
    dplyr::select(dose_regime, dose, dose_units.name) %>%
    tidyr::unite("dose_dose_units", -dose_regime, sep = "||", na.rm = TRUE) %>%
    tidyr::pivot_wider(id_cols = dose_regime,
                       names_from = "dose_dose_units",
                       values_from = "dose_dose_units") %>%
    tidyr::unite("dose_dose_units", -dose_regime, sep="; ", na.rm = TRUE) %>%
    dplyr::mutate(dose_dose_units = gsub("; NA|NA; ", "", dose_dose_units)) %>%
    dplyr::mutate(dose = purrr::map2_chr(dose_dose_units, TRUE, split_dose_dose_units),
                  dose_units.name = purrr::map2_chr(dose_dose_units, FALSE, split_dose_dose_units)) %>%
    dplyr::select(-dose_dose_units)

  # Select units to keep
  dose_regime_unit_selection <- c(
    `mg/day` = "mg/day",
    `mg/m3` = "mg/m3",
    `mg/kg` = "mg/kg",
    `Normal` = "Normal",
    `%` = "%",
    `mg/L` = "mg/L",
    `mg/kg-day` = "mg/kg-day",
    `mg/kg bw/day` = "mg/kg bw/day",
    `ppm` = "ppm",
    `mg/m3; ppm` = "mg/m3",
    `mg/kg food; mg/kg-day` = "mg/kg-day",
    `mg/kg-day; ppm` = "mg/kg-day",
    `ppm; mg/kg-day` = "mg/kg-day",
    `ppm; mg/m3` = "mg/m3",
    `mg/kg-day; ug/kg/day; ug/mL` = "mg/kg-day",
    `mg/L; mg/m3; ppm` = "mg/m3",
    `ppm; weight % (in diet)` = "ppm",
    `mg/L; ppm` = "mg/L",
    `mg/kg-day; mg/kg-day HED` = "mg/kg-day HED",
    `mg/kg bw` = "mg/kg bw",
    `other` = "other",
    `ug/day` = "ug/day",
    `g/kg bw` = "g/kg bw",
    `uM` = "uM",
    `diet (% fed)` = "diet (% fed)",
    `ug/L` = "ug/L",
    `%; mg/kg-day` = "mg/kg-day",
    `mg/kg-day; weight % (in diet)` = "mg/kg-day",
    `%; mg/kg bw` = "mg/kg bw",
    `ug/mL` = "ug/mL",
    `mg/kg-day; mg/kg-week` = "mg/kg-day",
    `mg/kg; mg/kg-day` = "mg/kg-day",
    `mM; mg/kg-day` = "mg/kg-day",
    `weight % (in diet)` = "weight % (in diet)",
    `umol/kg` = "umol/kg",
    `mg/kg-day t0; mg/kg-day t4` = "mg/kg-day t4",
    `mg/L; mg/kg-day; ng/ml serum` = "mg/kg-day",
    `mg/kg-day; ug/g liver; ug/mL serum` = "mg/kg-day",
    `mg/kg-day; ng/mg liver` = "mg/kg-day",
    `mg/kg-day; ug/g liver` = "mg/kg-day",
    `mg/kg-day; mg/L serum 4 weeks; mg/L serum 8 weeks` = "mg/kg-day",
    `mg/kg-day; ng/g blood; ng/g liver` = "mg/kg-day",
    `mg/kg-day; ug/g testis` = "mg/kg-day",
    `mg/L; mg/kg-day` = "mg/kg-day",
    `mg/kg-day; ng/g liver; ng/ml serum` = "mg/kg-day",
    `mg/kg-day; ug/g (whole embryo); ug/g liver; ug/mL amniotic fluid; ug/mL serum` = "mg/kg-day",
    `mg/kg-day; ug/g (whole embryo); ug/g liver; ug/mL serum` = "mg/kg-day",
    `mg/kg-day; ug/mL` = "mg/kg-day",
    `mg/kg-day; ug/mL serum` = "mg/kg-day",
    `Liver/Plasma Ratio; mg/kg-day; ng/mg liver; ng/ml serum; uM in plasma; uM liver; um/mmol/kg/day liver; um/mmol/kg/day serum` = "mg/kg-day",
    `mg/kg-day; ng/ml serum; uM in plasma; um/mmol/kg/day serum` = "mg/kg-day",
    `Liver/Plasma Ratio; mg/kg-day; ng/g liver; ng/ml serum; uM in plasma; uM liver; um/mmol/kg/day liver; um/mmol/kg/day serum` = "mg/kg-day",
    `mg/kg-day; mmol/kg-day; ng/ml serum; uM in plasma; um/mmol/kg/day serum` = "mg/kg-day",
    `liver:serum; mg/kg-day; ppm; ug/g liver; ug/mL serum` = "mg/kg-day",
    `ppm; liver:serum; mg/kg-day; ug/g liver; ug/mL serum` = "mg/kg-day",
    `mg/kg-day; ppm liver; ppm serum` = "mg/kg-day",
    `ppm; Cum. dose (mg/kg); liver:serum; mg/kg-day; ug/g liver; ug/mL serum` = "mg/kg-day",
    `ppm; % in liver; % in serum; Cum. dose (mg/kg); liver:serum; mg/kg-day; ug/g liver; ug/mL serum` = "mg/kg-day",
    `mg/kg-day; ng/ml serum` = "mg/kg-day",
    `mg/kg-day; ppm; ug/mL serum` = "mg/kg-day",
    `mg/kg-day; ug/g brain; ug/g liver; ug/mL serum` = "mg/kg-day",
    `mg/L blood; mg/kg-day` = "mg/kg-day",
    `mg/L serum; mg/kg-day` = "mg/kg-day",
    `mg/kg-day; ng/g liver; ng/g placenta` = "mg/kg-day",
    `mg/kg-day; ug/g lung` = "mg/kg-day",
    `mg/kg-day; ppm; ug/g blood` = "mg/kg-day",
    `ppm; mg/kg-day; ug/g blood` = "mg/kg-day",
    `mg/kg-day; mg/kg-day ADD; ppm` = "mg/kg-day",
    `ppm; mg/kg-day; mg/kg-day ADD` = "mg/kg-day",
    `mg/kg bw/day; ppm` = "mg/kg bw/day"
  ) %>%
    data.frame(group = names(.), select=unname(.))

  # Check if missing any dose_regime unit selections
  if(any(!doses$dose_units.name %in% dose_regime_unit_selection$group)){
    message("Missing dose_regime_unit_selection entry:")
    cat(unique(doses$dose_units.name[!doses$dose_units.name %in% dose_regime_unit_selection$group]), sep = "\n")
    stop("Missing dose_regime_unit_selection entry...")
  }

  # Join to selected units and remove any that are not selected
  doses = doses %>%
    dplyr::left_join(dose_regime_unit_selection,
                     by = c("dose_units.name"="group")) %>%
    tidyr::separate_longer_delim(
      cols = -dose_regime,
      delim = "; "
    ) %>%
    dplyr::filter(dose_units.name == select)

  # Join/fill in dose and dose_units
  hawc_pfas$doses <- doses$dose[match(hawc_pfas$animal_group.dosing_regime.id,doses$dose_regime)]
  hawc_pfas$doses_units <- doses$dose_units.name[match(hawc_pfas$animal_group.dosing_regime.id,doses$dose_regime)]

  # Filter original dose_dict to selected units by dose_regime
  dose_dict_orig = dose_dict_orig %>%
    dplyr::left_join(doses %>%
                       dplyr::select(dose_regime, dose_units.name, select),
                     by = c("dose_regime", "dose_units.name")) %>%
    dplyr::filter(!is.na(select))

  # Fix NOEL dict
  noel_dict = dose_dict_orig %>%
    dplyr::left_join(hawc_pfas %>%
                       dplyr::select(animal_group.dosing_regime.id, NOEL),
                     by = c("dose_regime"="animal_group.dosing_regime.id")) %>%
    dplyr::filter(NOEL == dose_group_id) %>%
    dplyr::select(-dose_group_id) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dose_regime, dose_units.name, dose)
  # Sort out the NOEL values and units by combining and splitting
  noel_values = data.frame()
  if(nrow(noel_dict)){
    noel_values = noel_dict %>%
      tidyr::unite("dose_dose_units", -dose_regime, -NOEL, sep = "||", na.rm = TRUE) %>%
      tidyr::pivot_wider(id_cols = c("dose_regime", "NOEL"),
                         names_from = "dose_dose_units",
                         values_from = "dose_dose_units") %>%
      tidyr::unite("dose_dose_units", -dose_regime, -NOEL, sep="; ", na.rm=TRUE) %>%
      dplyr::mutate(dose_dose_units = gsub("; NA|NA; ", "", dose_dose_units)) %>%
      dplyr::mutate(dose = purrr::map2_chr(dose_dose_units, TRUE, split_dose_dose_units),
                    dose_units.name = purrr::map2_chr(dose_dose_units, FALSE, split_dose_dose_units)) %>%
      dplyr::select(-dose_dose_units)
  }

  # Fix LOEL dict
  loel_dict = dose_dict_orig %>%
    dplyr::left_join(hawc_pfas %>%
                       dplyr::select(animal_group.dosing_regime.id, LOEL),
                     by = c("dose_regime"="animal_group.dosing_regime.id")) %>%
    dplyr::filter(LOEL == dose_group_id) %>%
    dplyr::select(-dose_group_id) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dose_regime, dose_units.name, dose)
  # Sort out the LOEL values and units by combining and splitting
  loel_values = data.frame()
  if(nrow(loel_dict)){
    loel_values = loel_dict %>%
      tidyr::unite("dose_dose_units", -dose_regime, -LOEL, sep = "||", na.rm = TRUE) %>%
      tidyr::pivot_wider(id_cols = c("dose_regime", "LOEL"),
                         names_from = "dose_dose_units",
                         values_from = "dose_dose_units") %>%
      tidyr::unite("dose_dose_units", -dose_regime, -LOEL, sep="; ", na.rm = TRUE) %>%
      dplyr::mutate(dose_dose_units = gsub("; NA|NA; ", "", dose_dose_units)) %>%
      dplyr::mutate(dose = purrr::map2_chr(dose_dose_units, TRUE, split_dose_dose_units),
                    dose_units.name = purrr::map2_chr(dose_dose_units, FALSE, split_dose_dose_units)) %>%
      dplyr::select(-dose_dose_units)
  }

  # Fix FEL dict
  fel_dict = dose_dict_orig %>%
    dplyr::left_join(hawc_pfas %>%
                       dplyr::select(animal_group.dosing_regime.id, FEL),
                     by = c("dose_regime"="animal_group.dosing_regime.id")) %>%
    dplyr::filter(FEL == dose_group_id) %>%
    dplyr::select(-dose_group_id) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dose_regime, dose_units.name, dose)
  # Sort out the FEL values and units by combining and splitting
  fel_values = data.frame()
  if(nrow(fel_dict)){
    fel_values = fel_dict %>%
      tidyr::unite("dose_dose_units", -dose_regime, -FEL, sep = "||", na.rm=TRUE) %>%
      tidyr::pivot_wider(id_cols = c("dose_regime", "FEL"),
                         names_from = "dose_dose_units",
                         values_from = "dose_dose_units") %>%
      tidyr::unite("dose_dose_units", -dose_regime, -FEL, sep="; ", na.rm = TRUE) %>%
      dplyr::mutate(dose_dose_units = gsub("; NA|NA; ", "", dose_dose_units)) %>%
      dplyr::mutate(dose = purrr::map2_chr(dose_dose_units, TRUE, split_dose_dose_units),
                    dose_units.name = purrr::map2_chr(dose_dose_units, FALSE, split_dose_dose_units)) %>%
      dplyr::select(-dose_dose_units)
  }

  # Match NOEL and LOEL values
  hawc_pfas$NOEL_values <- noel_values$dose[match(paste(hawc_pfas$animal_group.dosing_regime.id,hawc_pfas$NOEL),
                                                  paste(noel_values$dose_regime,noel_values$NOEL))]
  hawc_pfas$NOEL_units <-  noel_values$dose_units.name[match(paste(hawc_pfas$animal_group.dosing_regime.id,hawc_pfas$NOEL),
                                                             paste(noel_values$dose_regime,noel_values$NOEL))]
  hawc_pfas$LOEL_values <- loel_values$dose[match(paste(hawc_pfas$animal_group.dosing_regime.id,hawc_pfas$LOEL),
                                                  paste(loel_values$dose_regime,loel_values$LOEL))]
  hawc_pfas$LOEL_units <-  loel_values$dose_units.name[match(paste(hawc_pfas$animal_group.dosing_regime.id,hawc_pfas$LOEL),
                                                             paste(loel_values$dose_regime,loel_values$LOEL))]
  hawc_pfas$FEL_values <- fel_values$dose[match(paste(hawc_pfas$animal_group.dosing_regime.id,hawc_pfas$FEL),
                                                paste(fel_values$dose_regime,fel_values$FEL))]
  hawc_pfas$FEL_units <-  fel_values$dose_units.name[match(paste(hawc_pfas$animal_group.dosing_regime.id,hawc_pfas$FEL),
                                                           paste(fel_values$dose_regime,fel_values$FEL))]

  num_unit_cols = c("NOEL_values", "NOEL_units", "LOEL_values", "LOEL_units", "FEL_values", "FEL_units")
  # Fill blank columns
  hawc_pfas[, num_unit_cols[!num_unit_cols %in% names(hawc_pfas)]] <- NA

  fac_cols <- sapply(hawc_pfas, is.factor)                          # Identify all factor columns
  hawc_pfas[fac_cols] <- lapply(hawc_pfas[fac_cols], as.character)  # Convert all factors to characters

  hawc_pfas = hawc_pfas %>%
    # Add url information
    dplyr::mutate(
      endpoint_url = paste("https://hawcprd.epa.gov", url, sep = ""),
      assessment_url = paste("https://hawcprd.epa.gov/assessment/", assessment, "/", sep = "")
    )

  # Rename columns
  rename_list <- c(
    "assessment" = "assessment",
    "critical_effect" = "name",
    "target_organ" = "organ",
    "NOEL_original" = "NOEL",
    "LOEL_original" = "LOEL",
    "FEL_original" = "FEL",
    "endpoint_url_original" = "url",
    "data_location" = "data_location",
    "bmds" = "bmds",
    "study_id" = "animal_group.experiment.study.id",
    "title" = "animal_group.experiment.study.title",
    "authors_short" = "animal_group.experiment.study.authors_short",
    "author" = "animal_group.experiment.study.authors",
    "year" = "animal_group.experiment.study.year",
    "journal" = "animal_group.experiment.study.journal",
    "full_text_url" = "animal_group.experiment.study.full_text_url",
    "short_ref" = "animal_group.experiment.study.short_citation",
    "long_ref" = "animal_group.experiment.study.full_citation",
    "study_url_original" = "animal_group.experiment.study.url",
    "experiment_name" = "animal_group.experiment.name",
    "experiment_type" = "animal_group.experiment.type",
    "name" = "animal_group.experiment.chemical",
    "casrn" = "animal_group.experiment.cas",
    "chemical_source" = "animal_group.experiment.chemical_source",
    "media" = "animal_group.experiment.vehicle",
    "guideline_compliance" = "animal_group.experiment.guideline_compliance",
    "dosing_regime_id" = "animal_group.dosing_regime.id",
    "route_of_exposure" = "animal_group.dosing_regime.route_of_exposure",
    "exposure_duration_value" = "animal_group.dosing_regime.duration_exposure",
    "exposure_duration_text" = "animal_group.dosing_regime.duration_exposure_text",
    "species" = "animal_group.species",
    "strain" = "animal_group.strain",
    "sex" = "animal_group.sex",
    "population" = "animal_group.name",
    "generation" = "animal_group.generation",
    "noel_names" = "noel_names.noel",
    "loel_names" = "noel_names.loel",
    "doses" = "doses",
    "doses_units" = "doses_units",
    "NOEL_values" = "NOEL_values",
    "NOEL_units" = "NOEL_units",
    "LOEL_values" = "LOEL_values",
    "LOEL_units" = "LOEL_units",
    "FEL_values" = "FEL_values",
    "FEL_units" =  "FEL_units",
    "record_url" = "endpoint_url",
    "source_url" = "assessment_url"
  )

  hawc_pfas = hawc_pfas %>%
    dplyr::rename(dplyr::all_of(rename_list))

  # Prep final data
  res = hawc_pfas %>%
    # Drop assessment column
    dplyr::select(-assessment) %>%

    # Add exposure and study_duration
    dplyr::mutate(
      exposure_route = route_of_exposure,
      exposure_method = route_of_exposure,
      study_duration_value = exposure_duration_value,
      study_duration_units = exposure_duration_text
    ) %>%

    # Rename columns as appropriate
    dplyr::rename(
      exposure_duration_value_original = exposure_duration_value,
      study_type = experiment_type
    ) %>%

    # Add final values
    dplyr::mutate(
      fel_names = "FEL"
    ) %>%

    # Pivot the NOEL, LOEL, and FEL fields to long form
    tidyr::unite(noel_names, NOEL_values, NOEL_units, NOEL_original,
                 col="NOEL", sep="|") %>%
    tidyr::unite(loel_names, LOEL_values, LOEL_units, LOEL_original,
                 col="LOEL", sep="|") %>%
    tidyr::unite(fel_names, FEL_values, FEL_units, FEL_original,
                 col="FEL", sep="|") %>%
    tidyr::pivot_longer(cols = c("NOEL", "LOEL", "FEL"),
                        names_to = NULL,
                        values_to = "toxval_transform") %>%
    # Preserve the toxval_numeric dose index
    tidyr::separate(col = toxval_transform,
                    into = c("toxval_type", "toxval_numeric", "toxval_units", "toxval_numeric_dose_index"),
                    sep = "\\|") %>%
    dplyr::mutate(toxval_type = gsub("NA", NA, toxval_type),
                  toxval_numeric = gsub("NA", NA, toxval_numeric),
                  toxval_units = gsub("NA", NA, toxval_units)) %>%

    # Drop columns without toxval_numeric, toxval_type, or toxval_units
    tidyr::drop_na(toxval_numeric, toxval_type, toxval_units) %>%
    dplyr::filter(toxval_numeric != "-999",
                  toxval_units != "Normal") %>%
    dplyr::mutate(
      # Replace critical_effect field "-" with NA
      critical_effect = critical_effect %>% as.character() %>% dplyr::na_if("-"),
      target_organ = target_organ %>%
        as.character() %>%
        dplyr::na_if("-") %>%
        dplyr::na_if("[Organ]"),
      generation = generation %>% as.character() %>% dplyr::na_if("-")
    ) %>%
    # Build critical_effect value in form generation: target_organ: critical_effect
    tidyr::unite("critical_effect", generation, target_organ, critical_effect,
                 sep = ": ",
                 na.rm=TRUE,
                 remove=FALSE)

  # strip begining and ending quotation marks
  # Setting simplify to FALSE for cases where res is only 1 row. Unexpected behavior with just 1 row...
  res <- as.data.frame(sapply(res, function(x) gsub("\"", "", x), simplify = FALSE))

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  # Handle relationship linkages
  relationship <- res %>%
    dplyr::filter(grepl(";", toxval_numeric),
                  grepl(";", toxval_units),
                  grepl(";", doses),
                  grepl(";", doses_units))
  # Check if any available
  if(nrow(relationship)){
    relationship = relationship %>%
      dplyr::mutate(relationship_id = 1:dplyr::n())
  } else {
    # Empty dataframe with res cols to bind_rows()
    relationship = res[0,]
  }

  # Join back the relationship split rows
  res <- res %>%
    dplyr::filter(!grepl(";", toxval_numeric),
                  !grepl(";", toxval_units),
                  !grepl(";", doses),
                  !grepl(";", doses_units)) %>%
    dplyr::bind_rows(relationship)

  # Final transformations moved from load script
  res = res %>%
    # Separate entries with multiple corresponding toxval_numeric, toxval_units, and dose values
    tidyr::separate_rows(
      c(doses, doses_units, toxval_numeric, toxval_units),
      sep = ";"
    ) %>%

    dplyr::mutate(
      dplyr::across(c(doses, doses_units, toxval_numeric, toxval_units), stringr::str_squish),
      dplyr::across(tidyselect::where(is.character), fix.replace.unicode),
      species = tolower(species),
      toxval_numeric = as.numeric(toxval_numeric),
      toxval_units = toxval_units %>% stringr::str_squish(),
      casrn = gsub("([a-zA-Z]+\\s+[a-zA-Z]*\\:*\\s*)(.*)", "\\2", casrn),

      # Remove "Not Specified" strain
      strain = strain %>%
        dplyr::na_if("Not Specified"),

      # Clean sex values
      sex = sex %>%
        gsub("Combined", "male/female", .) %>%
        tolower(),

      # Fix exposure details
      exposure_route = gsub("(^[a-zA-Z]+)(\\s*.*)", "\\1", route_of_exposure) %>%
        tolower(),
      exposure_method = route_of_exposure %>%
        gsub("(^[a-zA-Z]+\\s*)(.*)", "\\2", .) %>%
        gsub("^\\-\\s+", "", .) %>%
        dplyr::na_if(., ""),

      study_duration = exposure_duration_text %>%
        gsub("([0-9])([A-Za-z])", "\\1 \\2", .) %>%
        gsub("(PND|GD|LD)s", "\\1", .) %>%
        gsub("D([0-9])", "D \\1", .) %>%
        gsub("\\s*\\(.+", "", .) %>%
        gsub("\\s*\\-\\s*", "-", .) %>%
        gsub("[0-9\\.]+\\s*[a-z]\\/[a-z]", "", .) %>%
        gsub("1 hr 20 min to 4 h", "1.33-4 h", .) %>%
        gsub("\\s*via.+", "", .),
      study_duration_value = dplyr::case_when(
        grepl("dose", study_duration) ~ as.character(NA),
        grepl("[0-9\\.]+(?:\\-[0-9\\.]+)?\\s*(?:d|wk|yr|min|h)", study_duration) ~ stringr::str_extract(study_duration,
                                                                                                        "([0-9\\.]+(?:\\-[0-9\\.]+)?)\\s*(?:d|wk|yr|min|h)",
                                                                                                        group=1),
        grepl("GD [0-9]+\\-[0-9]+", study_duration) ~ stringr::str_extract(study_duration,
                                                                           "GD ([0-9]+\\-[0-9]+)",
                                                                           group=1),
        grepl("(?:GD|PND|LD|PNW) [0-9]+\\-(?:GD|PND|LD|PNW) [0-9]+", study_duration) ~ stringr::str_extract(study_duration,
                                                                                                            "((?:GD|PND|LD|PNW) [0-9]+\\-(?:GD|PND|LD|PNW) [0-9]+)",
                                                                                                            group=1),
        grepl("premating-LD 4", study_duration) ~ "4",
        TRUE ~ as.character(NA)
      ) %>%
        gsub("GD ([0-9]+\\-)GD ([0-9]+)", "\\1\\2", .),
      study_duration_units = dplyr::case_when(
        is.na(study_duration_value) ~ as.character(NA),
        grepl("[0-9\\.]+(?:\\-[0-9\\.]+)?\\s*(?:d|wk|yr|min|h)", study_duration) ~ stringr::str_extract(study_duration,
                                                                                                        "[0-9\\.]+(?:\\-[0-9\\.]+)?\\s*(d|wk|yr|min|h)",
                                                                                                        group=1),
        grepl("GD [0-9]+\\-[0-9]+", study_duration) ~ "GD",
        grepl("(?:GD|PND|LD|PNW) [0-9]+\\-(?:GD|PND|LD|PNW) [0-9]+", study_duration) ~ gsub("(GD|PND|LD|PNW) [0-9]+\\-(GD|PND|LD|PNW) [0-9]+",
                                                                                            "\\1,\\2",
                                                                                            study_duration),
        grepl("premating-LD 4", study_duration) ~ "LD",
        TRUE ~ as.character(NA)
      ) %>%
        gsub("d", "days", .) %>%
        gsub("wk", "weeks", .) %>%
        gsub("yr", "years", .) %>%
        gsub("min", "minutes", .) %>%
        gsub("h", "hours", .) %>%
        gsub("(?:(GD),GD)|(?:(PND),PND)|(?:(LD),LD)|(?:(PNW),PNW)", "\\1", .),

      study_type = gsub("(^\\w+\\-*\\w*)(\\s*.*)", "\\1", study_type),

      # Extract exposure_form from media field
      exposure_form = dplyr::case_when(
        grepl("not reported|not large enough|\\bother|none|no vehicle", media, ignore.case=TRUE) ~ as.character(NA),
        TRUE ~ media
      )
    ) %>%
    # Drop unused study_duration field
    dplyr::select(-study_duration) %>%
    # Filter out invalid entries
    dplyr::filter(!(toxval_numeric %in% c(0, NA))) %>%
    dplyr::distinct()

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"

  hashing_cols = c(toxval.config()$hashing_cols[!(toxval.config()$hashing_cols %in% c("critical_effect"))]# ,
                   # "dosing_regime_id",
                   # "experiment_url"
  )

  # Perform deduping
  res = toxval.source.import.dedup(res, hashing_cols=hashing_cols) %>%
    # Replace "|::|" in critical_effect with "|" delimiter
    dplyr::mutate(
      critical_effect = critical_effect %>%
        gsub(" \\|::\\| ", "|", .) %>%
        stringr::str_squish()
    )

  return(res)
}
