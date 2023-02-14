#--------------------------------------------------------------------------------------
#' A generic template for adding data to toxval_source for a new source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param subf The subfolder containing the IUCLID subsource
#--------------------------------------------------------------------------------------
#import_source_iuclid <- function(db, subf, chem.check.halt=F) {
#  printCurrentFunction(db)
#  source = paste0("IUCLID_", subf)
#  source_table = paste0("source_iuclid_", subf) %>% tolower()
#  dir = paste0(toxval.config()$datapath,"iuclid/",subf,"/",subf,"_files/")
#  file = list.files(dir, pattern=".xlsx", full.names = TRUE)
#  if(length(file) > 1) stop("More than 1 IUCLID file stored in '", dir, "'")
#  res = readr::read_xlsx(file)

import_efsa_source <- function(db,chem.check.halt=F) {
  printCurrentFunction(db)
  source = "IUCLID"
  source_table = "source_iuclid"
  dir = paste0(toxval.config()$datapath,"iuclid/")
  file = paste0(dir,"RepeatedDoseToxicityOral.xlsx")
  res0 = readxl::read_xlsx(file, guess_max=Inf)

  if(!nrow(res)){
    return("...No rows found in file...skipping")
  }

  res <- res0 %>%
  # Copy columns and rename new columns
    mutate(name = reference_substance,
           casrn = reference_substance_CASnumber,
           ec_number = reference_substance_ECnumber,
           url = ECHA_url,
           toxval_type = ResultsAndDiscussion.EffectLevels.Efflevel..Endpoint.code,
           toxval_units = ResultsAndDiscussion.EffectLevels.Efflevel..EffectLevel.unit.code,
           sex = ResultsAndDiscussion.EffectLevels.Efflevel..Sex.code,
           toxval_numeric_lower = ResultsAndDiscussion.EffectLevels.Efflevel..EffectLevel.lowerValue,
           toxval_numeric_upper = ResultsAndDiscussion.EffectLevels.Efflevel..EffectLevel.upperValue,
           toxval_qualifier_lower = ResultsAndDiscussion.EffectLevels.Efflevel..EffectLevel.lowerQualifier,
           toxval_qualifier_upper = ResultsAndDiscussion.EffectLevels.Efflevel..EffectLevel.upperQualifier,
           strain = MaterialsAndMethods.TestAnimals.Strain.code,
           species = MaterialsAndMethods.TestAnimals.Species.code,
           guideline = MaterialsAndMethods.Guideline.0.Guideline.code,
           study_duration_value = MaterialsAndMethods.AdministrationExposure.DurationOfTreatmentExposure,
           study_duration_units = MaterialsAndMethods.AdministrationExposure.DurationOfTreatmentExposure,
           study_type = AdministrativeData.Endpoint.code,
           exposure = MaterialsAndMethods.AdministrationExposure.RouteOfAdministration.code,
           reference_title = literatureTitle,
           reference_type = literatureType,
           reference_year = literature_referenceYear,
           effect_level_basis = ResultsAndDiscussion.EffectLevels.Efflevel..Basis..code,
           toxval_units_other = ResultsAndDiscussion.EffectLevels.Efflevel..EffectLevel.unit.other,
           toxval_type_other = ResultsAndDiscussion.EffectLevels.Efflevel..Endpoint.other,
           species_other = MaterialsAndMethods.TestAnimals.Species.other,
           strain_other = MaterialsAndMethods.TestAnimals.Strain.other,
           media = ResultsAndDiscussion.TargetSystemOrganToxicity.TargetSystemOrganToxicity..Organ..code,
           dose_units = ResultsAndDiscussion.TargetSystemOrganToxicity.TargetSystemOrganToxicity..LowestEffectiveDoseConc.unit.code,
           dose = ResultsAndDiscussion.TargetSystemOrganToxicity.TargetSystemOrganToxicity..LowestEffectiveDoseConc.value) %>%
    # Split columns and name them
    tidyr::separate(., study_type, c("study_type","exposure_route"), sep=": ", fill="right", remove=FALSE) %>%
    tidyr::separate(., exposure, c(NA,"exposure_method"), sep=": ", fill="right", remove=FALSE) %>%
    # Combine columns and name them
    unite(toxval_numeric, toxval_numeric_lower, toxval_numeric_upper, na.rm = TRUE, sep='-') %>%
    unite(toxval_qualifier, toxval_qualifier_lower, toxval_qualifier_upper, na.rm = TRUE, sep=' ')
  
    # Replace column value with another column value based on a condition
    res$toxval_units[res$toxval_units == 'other:' & !is.na(res$toxval_units)] <- res$toxval_units_other[res$toxval_units == 'other:' & !is.na(res$toxval_units)]
    res$toxval_type[res$toxval_type == 'other:' & !is.na(res$toxval_type)] <- res$toxval_type_other[res$toxval_type == 'other:' & !is.na(res$toxval_type)]
    res$species[res$species == 'other:' & !is.na(res$species)] <- res$species_other[res$species == 'other:' & !is.na(res$species)]
    res$strain[res$strain == 'other:' & !is.na(res$strain)] <- res$strain_other[res$strain == 'other:' & !is.na(res$strain)]
    
  # Standardize the names
  names(res) <- names(res) %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
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
