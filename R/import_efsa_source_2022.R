#--------------------------------------------------------------------------------------
#' Import of EFSA OpenFoodTox 2022 source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#--------------------------------------------------------------------------------------
import_efsa_source <- function(db,chem.check.halt=F) {
  printCurrentFunction(db)
  source = "EFSA"
  source_table = "source_efsa"
  dir = paste0(toxval.config()$datapath,"efsa/efsa_files/")
  file = paste0(dir,"efsa_openfoodtox_raw_2022.xlsx")
  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  #
  # the final file should have column names that include "name" and "casrn"
  # additionally, the names in res need to match names in the source
  # database table. You do not need to add any of the generic columns
  # described in the SOP - they will get added in source_prep_and_load
  #
  
  # Standardize the names
  names(res0) <- names(res0) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    gsub("___", "_", .) %>%
    tolower()
  
  #res = source.specific.transformations(res0)
  
  res <- res0 %>%
    # Renaming columns
    dplyr::rename(record_url=url,
                  toxval_type=endpoint,
                  toxval_numeric=value,
                  toxval_numeric_qualifier=qualifier_x,
                  toxval_unit=doseunit,
                  critical_effect=basis,
                  study_type=testtype,
                  study_duration_value=exp_duration_days,
                  species_original=species,
                  strain=strain,
                  sex=sex,
                  human_eco=study_category,
                  year=publicationyear,
                  title=title,
                  record_source_type=doctype,
                  casrn=com_casnumber,
                  name = com_name,
                  chemical=comparamname,
                  source=owner,
                  subsource=author) %>%
    # Recoding/fixing entries in study_type
    mutate(study_type=recode(study_type,
                             "acute toxicity" = "acute",
                             "chronic/long term toxicity" = "chronic/long-term",
                             "reproduction toxicity" = "reproductive",
                             "short-term toxicity" = "short-term",
                             "study with volunteers" = "human"),
           human_eco=recode(human_eco,
                            "Animal (non-target species) health" = "human health",
                            "Animal (target species) health" = "human health",
                            "Ecotox (soil compartment)" = "eco",
                            "Ecotox (water compartment)" = "eco",
                            "Human health" = "human health"),
           # Fill in source_url, source_file, and study_duration_units columns
           source_url = "https://zenodo.org/record/5076033#.Y9fEoXbMI2z",
           source_download = "OpenFoodToxTX22784_2022.xlsx",
           study_duration_units = "days") %>%
    # splitting ROUTE into exposure_route and exposure_method columns
    tidyr::separate(., route, c("exposure_route","exposure_method"), sep=": ", fill="right", remove=FALSE) %>%
    mutate(toxval_unit = gsub("µ", "u", toxval_unit)) %>%
    mutate(toxval_unit = gsub("³", "3", toxval_unit))
  
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source=source,table=source_table,res=res,F,T,T)
}


