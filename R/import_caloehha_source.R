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

  #####################################################################
  cat("Do the chemical checking\n")
  #####################################################################
  source = "Cal OEHHA"
  res = as.data.frame(mat)
  res$clowder_id = "-"
  res = fix.non_ascii.v2(res,source)
  res = source_chemical.process(db,res,source,chem.check.halt,casrn.col="casrn",name.col="name")
  #####################################################################
  cat("Build the hash key and load the data \n")
  #####################################################################
  res = subset(res,select=-c(chemical_index))
  toxval_source.hash.and.load(db,source,"original_caloehha_table",F,F,res)
  return(1)


  ###runInsertTable(mat,"original_caloehha_table",db,do.halt=T,verbose=F)

  #####################################################################
  cat("Build whole_caloehha table from source file\n")
  #####################################################################

  mat[is.null(mat)] <- NA
  name.list <- c("casrn","name","toxval_type","toxval_numeric","toxval_units",
               "species_original","critical_effect",
               "risk_assessment_class","year","exposure_route",
               "study_type","study_duration_value","study_duration_units","record_url")

  row <- as.data.frame(matrix(nrow=1,ncol=length(name.list)))
  names(row) <- name.list
  res <- NULL
  for(i in 1:nrow(mat)) {

    casrn <- fix.casrn(mat[i,"casrn"])
    name <- mat[i,"name"]
    cname <- tolower(name)
    cname <- str_replace_all(cname," ","-")
    if(!is.na(mat[i,"inhalation_unit_risk_(ug/m3)-1"])) {
      row[] <- NA
      row[1,"casrn"] <- casrn
      row[1,"name"] <- name
      row[1,"toxval_numeric"] <- mat[i,"inhalation_unit_risk_(ug/m3)-1"]
      row[1,"toxval_units"] <- "(ug/m3)-1"
      row[1,"toxval_type"] <- "inhalation unit risk"
      row[1,"exposure_route"] <- "inhalation"
      row[1,"risk_assessment_class"] <- "chronic"
      row[1,"study_type"] <- "cancer"
      row[1,"year"] <- mat[i,"cancer_potency_year"]
      row[1,"record_url"] <- paste0("https://oehha.ca.gov/chemicals/",cname)
      res <- rbind(res,row)
    }

    if(!is.na(mat[i,"inhalation_slope_factor_(mg/kg-day)-1"])) {
      row[] <- NA
      row[1,"casrn"] <- casrn
      row[1,"name"] <- name
      row[1,"toxval_numeric"] <- mat[i,"inhalation_slope_factor_(mg/kg-day)-1"]
      row[1,"toxval_units"] <- "(mg/kg-day)-1"
      row[1,"toxval_type"] <- "inhalation slope factor"
      row[1,"exposure_route"] <- "inhalation"
      row[1,"risk_assessment_class"] <- "chronic"
      row[1,"study_type"] <- "cancer"
      row[1,"year"] <- mat[i,"cancer_potency_year"]
      row[1,"record_url"] <- paste0("https://oehha.ca.gov/chemicals/",cname)
      res <- rbind(res,row)
    }

    if(!is.na(mat[i,"oral_slope_factor_(mg/kg-day)-1"])) {
      row[] <- NA
      row[1,"casrn"] <- casrn
      row[1,"name"] <- name
      row[1,"toxval_numeric"] <- mat[i,"oral_slope_factor_(mg/kg-day)-1"]
      row[1,"toxval_units"] <- "(mg/kg-day)-1"
      row[1,"toxval_type"] <- "oral unit risk"
      row[1,"exposure_route"] <- "oral"
      row[1,"risk_assessment_class"] <- "chronic"
      row[1,"study_type"] <- "cancer"
      row[1,"year"] <- mat[i,"cancer_potency_year"]
      row[1,"record_url"] <- paste0("https://oehha.ca.gov/chemicals/",cname)
      res <- rbind(res,row)
    }

    if(!is.na(mat[i,"acute_rel_ug/m3"])) {
      row[] <- NA
      row[1,"casrn"] <- casrn
      row[1,"name"] <- name
      row[1,"toxval_numeric"] <- mat[i,"acute_rel_ug/m3"]
      row[1,"toxval_units"] <- "ug/m3"
      row[1,"toxval_type"] <- "REL"
      row[1,"exposure_route"] <- "oral"
      row[1,"risk_assessment_class"] <- "acute"
      row[1,"study_type"] <- "acute"
      row[1,"year"] <- mat[i,"acute_rel_year"]
      row[1,"species_original"] <- tolower(mat[i,"acute_rel_species"])
      row[1,"critical_effect"] <- paste(mat[i,"acute_rel_critical_effect"],":",mat[i,"acute_rel_target_organ"],":",mat[i,"acute_rel_severity"])
      row[1,"record_url"] <- paste0("https://oehha.ca.gov/chemicals/",cname)
      res <- rbind(res,row)
    }

    if(!is.na(mat[i,"inhalation_rel_8_hour_ug/m3"])) {
      row[] <- NA
      row[1,"casrn"] <- casrn
      row[1,"name"] <- name
      row[1,"toxval_numeric"] <- mat[i,"inhalation_rel_8_hour_ug/m3"]
      row[1,"toxval_units"] <- "ug/m3"
      row[1,"toxval_type"] <- "REL"
      row[1,"exposure_route"] <- "inhalation"
      row[1,"risk_assessment_class"] <- "acute"
      row[1,"study_type"] <- "acute"
      row[1,"year"] <- mat[i,"inhalation_rel_year"]
      row[1,"study_duration_value"] <- 8
      row[1,"study_duration_units"] <- "hour"
      row[1,"record_url"] <- paste0("https://oehha.ca.gov/chemicals/",cname)
      res <- rbind(res,row)
    }

    if(!is.na(mat[i,"chronic_inhalation_rel_ug/m3"])) {
      row[] <- NA
      row[1,"casrn"] <- casrn
      row[1,"name"] <- name
      row[1,"toxval_numeric"] <- mat[i,"chronic_inhalation_rel_ug/m3"]
      row[1,"toxval_units"] <- "ug/m3"
      row[1,"toxval_type"] <- "REL"
      row[1,"exposure_route"] <- "inhalation"
      row[1,"risk_assessment_class"] <- "chronic"
      row[1,"study_type"] <- "chronic"
      row[1,"year"] <- mat[i,"inhalation_rel_year"]
      row[1,"critical_effect"] <- paste( mat[i,"chronic_inhalation_critical_effect"],":", mat[i,"chronic_inhalation_target_organ"])
      row[1,"record_url"] <- paste0("https://oehha.ca.gov/chemicals/",cname)
      res <- rbind(res,row)
    }

    if(!is.na(mat[i,"mcl_mg/L"])) {
      row[] <- NA
      row[1,"casrn"] <- casrn
      row[1,"name"] <- name
      row[1,"toxval_numeric"] <- mat[i,"mcl_mg/L"]
      row[1,"toxval_units"] <- "mg/L"
      row[1,"toxval_type"] <- "MCL"
      row[1,"exposure_route"] <- "oral"
      row[1,"risk_assessment_class"] <- "chronic"
      row[1,"study_type"] <- "chronic"
      row[1,"record_url"] <- paste0("https://oehha.ca.gov/chemicals/",cname)
      res <- rbind(res,row)
    }

  }


  ###runInsertTable(res,"whole_caloehha_table",db,do.halt=T,verbose=F)

  ###########################################################
  cat("Convert multiple date formats present in year field to the corresponding year value,
      then change the data type from character to integer\n")
  ###########################################################
  res1 <- res
  date_fix <- excel_numeric_to_date(as.numeric(as.character(res1[grep("[0-9]{5}", res1$year),'year'])), date_system = "modern")
  date_fix <- format(date_fix, format = "%Y")

  res1[grep("[0-9]{5}", res1$year),'year'] <- date_fix
  res1[grep("[a-zA-Z]+", res1$year),'year'] <- gsub(".*\\,\\s+(\\d{4})", "\\1", grep("[a-zA-Z]+", res1$year,value= T))
  res1$year <- as.integer(res1$year)

  ###########################################################
  cat("Seperate multiple casrns \n")
  ###########################################################

  res1 <- separate_rows(res1, casrn, sep = ";")
  res1$casrn <- gsub("^\\s+","", res1$casrn)

  ###########################################################
  cat("Seperate multiple toxval_numeric entries, assign toxval units and exposure routes associated with certain toxval_numeric entries
      to their corresponding fields, convert character data type of toxval_numeric to numeric data type\n")
  ###########################################################

  tox_num <- grep("E",res1$toxval_numeric)
  res1[tox_num, "toxval_numeric"] <- gsub("\\s+","", tolower(grep("E",res1[tox_num, "toxval_numeric"], value = T)))

  res1 <- separate_rows(res1, toxval_numeric, sep = ";")
  res1$toxval_numeric <- gsub("^\\s+","", res1$toxval_numeric)
  res1[grep("\\(.*\\/.*\\)", res1$toxval_numeric),'toxval_units']<- gsub(".*\\((.*\\/.*)\\)", "\\1",grep("\\(.*\\/.*\\)", res1$toxval_numeric, value= T))
  res1[grep("\\(.*\\/.*\\)", res1$toxval_numeric),'toxval_numeric'] <- gsub("\\((.*\\/.*)\\).*", "", grep("\\(.*\\/.*\\)", res1$toxval_numeric, value= T))
  res1[grep("\\(.*\\)", res1$toxval_numeric),'exposure_route']  <- gsub(".*\\((.*)\\)", "\\1",grep("\\(.*\\)", res1$toxval_numeric, value= T))
  res1[grep("\\(.*\\)", res1$toxval_numeric),'toxval_numeric']  <- gsub("\\s*\\([^\\)]+\\)|\\s*\\([^\\)]+\\).*", "",res1[grep("\\(.*\\)", res1$toxval_numeric),'toxval_numeric'])
  tox_units <- grep("\\s+",res1$toxval_numeric)
  new_tox_units <- gsub("Î¼", "u", grep("\\s+",res1$toxval_numeric, value = T))
  res1[tox_units,'toxval_units'] <- gsub("^\\d+ |^\\d+\\,\\d+ |^\\d+\\.\\d+ ", "", new_tox_units)
  res1$toxval_numeric <-  gsub("Î¼", "u", res1$toxval_numeric)
  res1$toxval_numeric <- gsub("(^\\d+ |^\\d+\\,\\d+ |^\\d+\\.\\d+ ).*","\\1", res1$toxval_numeric)
  res1$toxval_numeric <-  gsub("^\\s+|\\s+$", "", res1$toxval_numeric)
  res1$toxval_numeric <- gsub(",", "", res1$toxval_numeric)
  #browser()
  #res1[grep("[0-9]+",res1$toxval_numeric, invert = T),"toxval_numeric"] <- gsub(paste0(grep("[0-9]+",res1$toxval_numeric, invert = T, value = T), "|", collapse = ""), "", grep("[0-9]+",res1$toxval_numeric, invert = T, value = T))
  res1$toxval_numeric <- as.numeric(res1$toxval_numeric)

  res1["caloehha_id"] <- c(1:length(res1[,1]))


  # #####################################################################
  # cat("Build new_caloehha from res1 \n")
  # #####################################################################
  # runInsertTable(res1,"new_caloehha",db,do.halt=T,verbose=F)
  #
  # #####################################################################
  # cat("Build caloehha_chemical_information table from res1\n")
  # #####################################################################
  # chemical_information <- res1[,c("name","casrn")]
  # chemical_information <- unique(chemical_information[,1:2])
  # chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  # chemical_information <- chemical_information[c('chemical_id','name','casrn')]
  #
  # runInsertTable(chemical_information,"caloehha_chemical_information",db,do.halt=T,verbose=F)

}



