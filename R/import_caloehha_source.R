library("openxlsx")
library('stringr')
library('tibble')
library('janitor')
library('tidyr')
#--------------------------------------------------------------------------------------
#' Load caloehha Source file into dev_toxval_source_v4.
#' The raw data can be exported as an Excel sheet from the web site
#' https://oehha.ca.gov/chemicals, selecting the link "Export database as .CSV file"
#'
#' This method parses that file and prepares for loading into toxval source
#'
#' @param db The version of toxval into which the source is loaded.
#' @param infile The input file ./caloehha/caloehha_files/OEHHA-chemicals_2018-10-30T08-50-47.xlsx
#--------------------------------------------------------------------------------------
import_caloehha_source <- function(db,
                                   infile="../caloehha/caloehha_files/OEHHA-chemicals_2018-10-30T08-50-47.xlsx",
                                   indir="../caloehha/caloehha_files/",
                                   chem.check.halt=F) {
  printCurrentFunction(db)

  #####################################################################
  cat("Build original_caloehha table from source file\n")
  #####################################################################
  mat <- openxlsx::read.xlsx(infile)
  #print(names(mat))
  name.list <- c(
    "name",
    "casrn",
    "use",
    "synonym",
    "latest_criteria",
    "inhalation_unit_risk_(ug/m3)-1",
    "inhalation_slope_factor_(mg/kg-day)-1",
    "oral_slope_factor_(mg/kg-day)-1",
    "cancer_potency_year",
    "acute_rel_ug/m3",
    "acute_rel_species",
    "acute_rel_critical_effect",
    "acute_rel_target_organ",
    "acute_rel_severity",
    "acute_rel_year",
    "inhalation_rel_8_hour_ug/m3",
    "inhalation_rel_year",
    "chronic_inhalation_rel_ug/m3",
    "chronic_inhalation_critical_effect",
    "chronic_inhalation_target_organ",
    "human_data",
    "human_data_critical_effect",
    "cancer_risk_at_phg",
    "mcl_mg/L",
    "cancer_risk_at_mcl",
    "notification_level_ug/L",
    "phg_mg/L",
    "phg_year",
    "nsrl_inhalation",
    "nsrl_oral",
    "madl_inhlation_reprotox",
    "madl_oral_reprotox",
    "madl_nsrl_year",
    "cancer_listing",
    "cancer_listing_mechanism",
    "reprotox_listing",
    "prop65_class",
    "prop65_devtox_year",
    "prop65_devtox_listing_mechanism",
    "female_reprotox_year",
    "female_reprotox_listing_mechanism",
    "male_reprotox_year",
    "male_reprotox_listing_mechanism",
    "chrd_mg/kg-day",
    "chrd_year",
    "CHHSL_Commercial_Non-volatile_mg/kg_soil",
    "CHHSL_Commercial_Volatile_engineered_fill_mcg/l_soil_gas",
    "CHHSL_Commercial_Volatile_no_engineered_fill_mcg/l_soil_gas",
    "CHHSL_Residential_Volatile_engineered_fill_mcg/l_soil_gas",
    "CHHSL_Residential_Volatile_no_engineered_fill_mcg/l_soil_gas",
    "Soil/soil-gas_screening_num_CHHSL_Residential_Non-vol_mg/kg")
  names(mat) <- name.list
  mat = mat[!is.element(mat$casrn,"n/a"),]

  clist = mat$casrn
  for(i in 1:length(clist)) {
    casrn1 = clist[i]
    temp = str_split(casrn1,";")[[1]]
    casrn2 = temp[1]
    if(casrn2!=casrn1) {
      cat(casrn1,":",casrn2,"\n")
      clist[i] = casrn2
    }
  }
  mat$casrn = clist
  names(mat) = c("name","casrn","use","synonym"    ,
                 "latest_criteria","inhalation_unit_risk"  ,
                 "inhalation_slope_factor","oral_slope_factor",
                 "cancer_potency_year","acute_rel",
                 "acute_rel_species","acute_rel_critical_effect",
                 "acute_rel_target_organ","acute_rel_severity",
                 "acute_rel_year","inhalation_rel_8_hour",
                 "inhalation_rel_year","chronic_inhalation_rel",
                 "chronic_inhalation_critical_effect","chronic_inhalation_target_organ",
                 "human_data","human_data_critical_effect",
                 "cancer_risk_at_phg","mcl",
                 "cancer_risk_at_mcl","notification_level",
                 "phg","phg_year",
                 "nsrl_inhalation","nsrl_oral" ,
                 "madl_inhlation_reprotox","madl_oral_reprotox",
                 "madl_nsrl_year","cancer_listing",
                 "cancer_listing_mechanism","reprotox_listing" ,
                 "prop65_class","prop65_devtox_year",
                 "prop65_devtox_listing_mechanism","female_reprotox_year",
                 "female_reprotox_listing_mechanism","male_reprotox_year",
                 "male_reprotox_listing_mechanism","chrd" ,
                 "chrd_year",
                 "chhsl_commercial_nonvolatile_soil",
                 "chhsl_commercial_volatile_engineered_fill_soil_gas",
                 "chhsl_commercial_volatile_no_engineered_fill_soil_gas",
                 "chhsl_residential_volatile_engineered_fill_soil_gas",
                 "chhsl_residential_volatile_no_engineered_fill_soil_gas",
                 "soil_soil_gas_screening_num_chhsl_residential_nonvolatile")
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="Cal OEHHA",table="source_caloehha",res=mat,F,T,T)
}
