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
  res0 = readxl::read_xlsx(file, guess_max=21474836)

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
    unite(toxval_qualifier, toxval_qualifier_lower, toxval_qualifier_upper, na.rm = TRUE, sep=' ') %>%
    #select(-matches("CrossReference.*.uuid")) %>%
    select(-matches("CrossReference.*.uuid|CrossReference.*.RelatedInformation"))

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
    res = fix_numeric_units_split(df = res,
                                  to_split = "MaterialsAndMethods.AdministrationExposure.DurationOfTreatmentExposure",
                                  value_to = "study_duration_value",
                                  units_to = "study_duration_units")

  # Standardize the names
  names(res) <- names(res) %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]|[\\(]|[\\)]", "_", .) %>%
    stringr::str_squish() %>%
    tolower() %>%
    # Truncate field names to abbreviated strings
    textclean::mgsub(.,
                     pattern = c("__", "administrativedata", "materialsandmethods", "administrationexposure", "administration",
                                 "materials", "resultsanddiscussion", "effectlevels", "system", "toxicity"
                     ),
                     replace = c("_", "admindata", "matnmet", "adminexposure", "admin",
                                 "mat", "resndisc", "efflvs", "sys", "tox")) %>%
    gsub("targetsysorgantox_targetsysorgantox", "targetsysorgantox", .)
  # Halt if field names are still too long
  if(any(nchar(names(res)) > 65)){
    message("Error: fieldnames too long: ", names(res)[nchar(names(res)) > 65] %>% toString())
    browser()
  }

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

orchestrate_import_source_iuclid <- function(dir=paste0(toxval.config()$datapath, "iuclid")) {
  # Loop through all subdirectories of current wd and load the source files within into ToxVal
  subdirs <- list.files(dir)
  for (subf in subdirs) {
    import_source_iuclid(db, subf, chem.check.halt = FALSE)
  }
}
