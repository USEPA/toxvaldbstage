#--------------------------------------------------------------------------------------
#' A generic template for adding data to toxval_source for a new source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param subf The subfolder containing the IUCLID subsource
#--------------------------------------------------------------------------------------
import_source_iuclid <- function(db, subf, chem.check.halt=F) {
  printCurrentFunction(db)
  source = paste0("IUCLID_", subf)
  source_table = paste0("source_iuclid_", subf) %>% tolower()
  dir = paste0(toxval.config()$datapath,"iuclid/",subf,"/",subf,"_files/")
  file = list.files(dir, pattern=".xlsx", full.names = TRUE)
  if(length(file) > 1) stop("More than 1 IUCLID file stored in '", dir, "'")
  # guess_max used due to large file with some columns guessed as NA/logical when not
  res0 = readxl::read_xlsx(file, guess_max=Inf)

#import_efsa_source <- function(db,chem.check.halt=F) {
#  printCurrentFunction(db)
#  source = "IUCLID"
#  source_table = "source_iuclid"
#  dir = paste0(toxval.config()$datapath,"iuclid/")
#  file = paste0(dir,"RepeatedDoseToxicityOral.xlsx")
# res0 = readxl::read_xlsx(file, guess_max=Inf)

  if(!nrow(res0)){
    return("...No rows found in file...skipping")
  }

  res <- res0 %>%
  # Copy columns and rename new columns
    mutate(name = reference_substance,
           casrn = reference_substance_CASnumber,
           ec_number = reference_substance_ECnumber,
           source_url = ECHA_url,
           toxval_type = ResultsAndDiscussion.EffectLevels.Efflevel..Endpoint.code,
           toxval_type_other = ResultsAndDiscussion.EffectLevels.Efflevel..Endpoint.other,
           toxval_units = ResultsAndDiscussion.EffectLevels.Efflevel..EffectLevel.unit.code,
           toxval_units_other = ResultsAndDiscussion.EffectLevels.Efflevel..EffectLevel.unit.other,
           sex = ResultsAndDiscussion.EffectLevels.Efflevel..Sex.code,
           toxval_numeric_lower = ResultsAndDiscussion.EffectLevels.Efflevel..EffectLevel.lowerValue,
           toxval_numeric_upper = ResultsAndDiscussion.EffectLevels.Efflevel..EffectLevel.upperValue,
           toxval_qualifier_lower = ResultsAndDiscussion.EffectLevels.Efflevel..EffectLevel.lowerQualifier,
           toxval_qualifier_upper = ResultsAndDiscussion.EffectLevels.Efflevel..EffectLevel.upperQualifier,
           strain = MaterialsAndMethods.TestAnimals.Strain.code,
           strain_other = MaterialsAndMethods.TestAnimals.Strain.other,
           species = MaterialsAndMethods.TestAnimals.Species.code,
           species_other = MaterialsAndMethods.TestAnimals.Species.other,
           guideline = MaterialsAndMethods.Guideline.0.Guideline.code,
           guideline_other = MaterialsAndMethods.Guideline.0.Guideline.other,
           # Handled by helper function below
           # study_duration_value = MaterialsAndMethods.AdministrationExposure.DurationOfTreatmentExposure,
           # study_duration_units = MaterialsAndMethods.AdministrationExposure.DurationOfTreatmentExposure,
           study_type = AdministrativeData.Endpoint.code,
           exposure = MaterialsAndMethods.AdministrationExposure.RouteOfAdministration.code,
           exposure_other = MaterialsAndMethods.AdministrationExposure.RouteOfAdministration.other,
           reference_title = literatureTitle,
           reference_type = literatureType,
           reference_year = literature_referenceYear,
           effect_level_basis = ResultsAndDiscussion.EffectLevels.Efflevel..Basis..code,
           media = ResultsAndDiscussion.TargetSystemOrganToxicity.TargetSystemOrganToxicity..Organ..code,
           dose_units = ResultsAndDiscussion.TargetSystemOrganToxicity.TargetSystemOrganToxicity..LowestEffectiveDoseConc.unit.code,
           dose = ResultsAndDiscussion.TargetSystemOrganToxicity.TargetSystemOrganToxicity..LowestEffectiveDoseConc.value) %>%
    # Split columns and name them
    tidyr::separate(., study_type, c("study_type","exposure_route"), sep=": ", fill="right", remove=FALSE) %>%
    tidyr::separate(., exposure, c(NA,"exposure_method"), sep=": ", fill="right", remove=FALSE) %>%
    # Combine columns and name them
    unite(toxval_numeric, toxval_numeric_lower, toxval_numeric_upper, na.rm = TRUE, sep='-') %>%
    unite(toxval_qualifier, toxval_qualifier_lower, toxval_qualifier_upper, na.rm = TRUE, sep=' ')

    # Replace column value with another column value based on a condition ("other:")
    res$toxval_units[res$toxval_units == 'other:' & !is.na(res$toxval_units)] <- res$toxval_units_other[res$toxval_units == 'other:' & !is.na(res$toxval_units)]
    res$toxval_type[res$toxval_type == 'other:' & !is.na(res$toxval_type)] <- res$toxval_type_other[res$toxval_type == 'other:' & !is.na(res$toxval_type)]
    res$species[res$species == 'other:' & !is.na(res$species)] <- res$species_other[res$species == 'other:' & !is.na(res$species)]
    res$strain[res$strain == 'other:' & !is.na(res$strain)] <- res$strain_other[res$strain == 'other:' & !is.na(res$strain)]
    res$guideline[res$guideline == 'other:' & !is.na(res$guideline)] <- res$guideline_other[res$guideline == 'other:' & !is.na(res$guideline)]
    res$exposure[res$exposure == 'other:' & !is.na(res$exposure)] <- res$exposure_other[res$exposure == 'other:' & !is.na(res$exposure)]
    # Fix: effect_level_basis TBD
    # Fix: media TBD
    # Fix: reference_type TBD
    # Fix: dose_units TBD

    # Fix study duration with various regex
    res = res %>%
      dplyr::mutate(temp_id = 1:n())

    tmp = fix_source_iuclid_study_duration(df = res %>%
                                             select(temp_id, raw_dur = MaterialsAndMethods.AdministrationExposure.DurationOfTreatmentExposure))

    res = res %>%
      left_join(tmp, by = "temp_id")

    res = res %>%
      select(-temp_id)

  # Standardize the names
  names(res) <- names(res) %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]|[\\(]|[\\)]", "_", .) %>%
    stringr::str_squish() %>%
    tolower()

  #####################################################################
  cat("Load the data\n")
  #####################################################################
  source_prep_and_load(db,source=source,table=source_table,res=res,
                       do.reset=FALSE,do.insert=TRUE,chem.check.halt=TRUE)
}

#--------------------------------------------------------------------------------------
#' Load the various IUCLID subsources into ToxVal
#' @param dir directory containing the various IUCLID subsource subdirectories
#' @param db The version of toxval_source into which the source is loaded.
#' @param do.insert If TRUE, insert data into the database, default TRUE
#' @param chem.check.halt If TRUE, stop the execution if there are errors in the
#' @return None, subsources loaded
#--------------------------------------------------------------------------------------

orchestrate_import_source_iuclid <- function(dir="Repo/iuclid") {
  # Loop through all subdirectories of current wd and load the source files within into ToxVal
  subdirs <- list.files(dir)
  for (subf in subdirs) {
    import_source_iuclid(db, subf, chem.check.halt = FALSE)
  }
}

#' fix_source_iuclid_study_duration
#' @description Helper function to parse IUCLID study_duration
#' @param df Input dataframe to process
fix_source_iuclid_study_duration <- function(df){
  # Quick normalization
  df = df %>%
    mutate(raw_dur = stringr::str_squish(tolower(raw_dur)))

  # Filter to rows without a duration
  out = df %>%
    filter(is.na(raw_dur)) %>%
    mutate(study_duration_value = NA,
           study_duration_units = NA) %>%
    select(-raw_dur)
  # Filter out matches
  df = df %>% filter(!temp_id %in% out$temp_id)
  # Case of integers with units, optional end in "." (e.g. 13 weeks.)
  out = df %>%
    # https://stackoverflow.com/questions/12117024/decimal-number-regular-expression-where-digit-after-decimal-is-optional
    filter(grepl("^[0-9]+\\s+[A-Za-z|.]+$", raw_dur)) %>%
    tidyr::separate(raw_dur, c("study_duration_value", "study_duration_units"), sep="\\s", extra="merge") %>%
    dplyr::mutate(across(c("study_duration_value", "study_duration_units"), ~stringr::str_squish(tolower(.)))) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% filter(!temp_id %in% out$temp_id)
  # Case of decimal numbers (ex. 7.5 years)
  out = df %>%
    # https://stackoverflow.com/questions/12117024/decimal-number-regular-expression-where-digit-after-decimal-is-optional
    filter(grepl("^[0-9]+\\.?[0-9]+\\s+[A-Za-z|.]+$", raw_dur)) %>%
    tidyr::separate(raw_dur, c("study_duration_value", "study_duration_units"), sep="\\s", extra="merge") %>%
    dplyr::mutate(across(c("study_duration_value", "study_duration_units"), ~stringr::str_squish(tolower(.)))) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% filter(!temp_id %in% out$temp_id)
  # Case like 14-days
  out = df %>%
    filter(grepl("^[0-9]+-[A-Za-z]+$", raw_dur)) %>%
    tidyr::separate(raw_dur, c("study_duration_value", "study_duration_units"), sep="-", extra="merge") %>%
    dplyr::mutate(across(c("study_duration_value", "study_duration_units"), ~stringr::str_squish(.))) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% filter(!temp_id %in% out$temp_id)
  # Case like 90 to 94 days
  out = df %>%
    filter(grepl("^[0-9]+\\s+to\\s+[0-9]+\\s+[A-Za-z]+$", raw_dur)) %>%
    dplyr::mutate(study_duration_value = stringr::str_extract(raw_dur, "^[0-9]+\\s+to\\s+[0-9]+\\s+") %>%
                    gsub("to", "-", .),
           study_duration_units = gsub("to", "", raw_dur) %>%
             stringr::str_replace_all("[:digit:]+", "")) %>%
    dplyr::mutate(across(c("study_duration_value", "study_duration_units"), ~stringr::str_squish(tolower(.)))) %>%
    select(-raw_dur) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% filter(!temp_id %in% out$temp_id)
  # TODO Case like 7 days or 28 days; 40 days to 55 days
  out = df %>%
    filter(grepl("^[0-9]+\\s[A-Za-z]+\\s[or|to]+\\s[0-9]+\\s[A-Za-z]+$", raw_dur)) %>%
    # Uncertain how to handle, setting aside for now
    mutate(study_duration_value = raw_dur,
           study_duration_units = raw_dur) %>%
    select(-raw_dur) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% filter(!temp_id %in% out$temp_id)
  # Case like 90 or 91 days
  out = df %>%
    filter(grepl("^[0-9]+\\s[or]+\\s[0-9]+\\s[A-Za-z]+$", raw_dur)) %>%
    # Uncertain how to handle, setting aside for now
    dplyr::mutate(study_duration_value = stringr::str_extract(raw_dur, "^[0-9]+\\s[or]+\\s[0-9]+"),
                  study_duration_units = gsub("or", "", raw_dur) %>%
                    stringr::str_replace_all("[:digit:]+", "")) %>%
    dplyr::mutate(across(c("study_duration_value", "study_duration_units"), ~stringr::str_squish(tolower(.)))) %>%
    select(-raw_dur) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% filter(!temp_id %in% out$temp_id)
  # Case like At least 90 days
  out = df %>%
    filter(grepl("^[Aa]t least [0-9]+ [A-Za-z]+$", raw_dur)) %>%
    mutate(raw_dur = gsub("at least", "", raw_dur, ignore.case=TRUE) %>%
             stringr::str_squish(.)) %>%
    tidyr::separate(raw_dur, c("study_duration_value", "study_duration_units"), sep="\\s", extra="merge") %>%
    dplyr::mutate(across(c("study_duration_value", "study_duration_units"), ~stringr::str_squish(tolower(.)))) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% filter(!temp_id %in% out$temp_id)
  # Case like - 12 weeks
  out = df %>%
    filter(grepl("^- [0-9]+ [A-Za-z]+$", raw_dur)) %>%
    mutate(raw_dur = gsub("-", "", raw_dur, ignore.case=TRUE) %>%
             stringr::str_squish(.)) %>%
    tidyr::separate(raw_dur, c("study_duration_value", "study_duration_units"), sep="\\s", extra="merge") %>%
    dplyr::mutate(across(c("study_duration_value", "study_duration_units"), ~stringr::str_squish(tolower(.)))) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% filter(!temp_id %in% out$temp_id)
  # TODO Case - Males: 14 days ... - Females: 14 days
  out = df %>%
    filter(grepl("^- Males: [0-9]+ [A-Za-z]+|- Females: [0-9]+ [A-Za-z]+", raw_dur)) %>%
    mutate(study_duration_value = raw_dur,
           study_duration_units = raw_dur) %>%
    select(-raw_dur) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% filter(!temp_id %in% out$temp_id)
  # Case like Up to 10 months
  out = df %>%
    filter(grepl("^[Uu]p to [0-9]+ [A-Za-z|.]+$", raw_dur)) %>%
    mutate(raw_dur = gsub("up to", "", raw_dur, ignore.case=TRUE) %>%
             stringr::str_squish(.)) %>%
    tidyr::separate(raw_dur, c("study_duration_value", "study_duration_units"), sep="\\s", extra="merge") %>%
    dplyr::mutate(across(c("study_duration_value", "study_duration_units"), ~stringr::str_squish(tolower(.)))) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% filter(!temp_id %in% out$temp_id)
  # TODO Case 10-11 days; 104-105 weeks; 100 - 110 days
  out = df %>%
    filter(grepl("^[0-9]+-[0-9]+ [A-Za-z|.]+$|^[0-9]+ - [0-9]+ [A-Za-z|.]+$", raw_dur)) %>%
    mutate(raw_dur = gsub(" - ", "-", raw_dur)) %>%
    tidyr::separate(raw_dur, c("study_duration_value", "study_duration_units"), sep="\\s", extra="merge") %>%
    dplyr::mutate(across(c("study_duration_value", "study_duration_units"), ~stringr::str_squish(.))) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% filter(!temp_id %in% out$temp_id)
  # Case > 600 d
  out = df %>%
    filter(grepl("^>[0-9]+|^> [0-9]+", raw_dur)) %>%
    mutate(raw_dur = gsub(">", "", raw_dur) %>%
             stringr::str_squish(.)) %>%
    tidyr::separate(raw_dur, c("study_duration_value", "study_duration_units"), sep="\\s", extra="merge") %>%
    dplyr::mutate(across(c("study_duration_value", "study_duration_units"), ~stringr::str_squish(.))) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% filter(!temp_id %in% out$temp_id)
  # Case 102 weeks (2 years)
  out = df %>%
    filter(grepl("^[0-9]+ [A-Za-z|.]+ \\([0-9]+ [A-Za-z]+\\)$", raw_dur)) %>%
    mutate(raw_dur = stringr::str_extract(raw_dur, "^[0-9]+ [A-Za-z|.]+ \\([0-9]+ [A-Za-z]+\\)") %>%
             sub('\\(.*', '', .)) %>%
    tidyr::separate(raw_dur, c("study_duration_value", "study_duration_units"), sep="\\s", extra="merge") %>%
    dplyr::mutate(across(c("study_duration_value", "study_duration_units"), ~stringr::str_squish(.))) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% filter(!temp_id %in% out$temp_id)
  # TODO Case of spelled out numbers (e.g., twenty weeks)
  # Generate spelled out number lists to check
  numbers = list(base = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine"),
                 teens = c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"),
                 tens = c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"))

  numbers = c(numbers %>% c(),
              sapply(numbers$tens, function(n){ paste0(n, "", numbers$base)}, USE.NAMES = FALSE) %>% c(),
              sapply(numbers$tens, function(n){ paste0(n, "-", numbers$base)}, USE.NAMES = FALSE) %>% c(),
              sapply(numbers$tens, function(n){ paste0(n, " ", numbers$base)}, USE.NAMES = FALSE) %>% c()
  ) %>% unlist()
  # Need to refine the splitting of the column
  tmp = df %>%
    filter(grepl(paste0("^", numbers, " [A-Za-z|.]+$", collapse="|"),
                 raw_dur)) %>%
    mutate(study_duration_value = raw_dur,
           study_duration_units = stringr::str_extract(raw_dur, "[A-Za-z|.]+$"))
  # Remove units from values (order by character count so smaller abbreviations don't mess up other replacements)
  tmp_reg = unique(tmp$study_duration_units)
  tmp$study_duration_value = gsub(paste0(tmp_reg[order(nchar(tmp_reg), tmp_reg, decreasing = TRUE)], collapse="|"),
                                  "",
                                  tmp$study_duration_value) %>%
    stringr::str_squish()
  out = tmp %>%
    # https://rdrr.io/cran/doseminer/man/words2number.html
    # TODO Convert spelled out numbers to numerics
    dplyr::mutate(across(c("study_duration_value", "study_duration_units"), ~stringr::str_squish(.))) %>%
    select(-raw_dur) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% filter(!temp_id %in% out$temp_id)

  # Helper to see what's left to fix
  # df %>% select(raw_dur) %>% unique() %>% View()

  # Percent coverage
  # round(nrow(out) / (nrow(out) + nrow(df)) * 100, 3)

  # Return processed fields
  out = df %>%
    mutate(study_duration_value=raw_dur,
           study_duration_units=raw_dur) %>%
    select(-raw_dur) %>%
    # Remove extraneous whitespace
    dplyr::mutate(across(c("study_duration_value", "study_duration_units"), ~stringr::str_squish(.))) %>%
    rbind(out, .)
  return(out)
}
