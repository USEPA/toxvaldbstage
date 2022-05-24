library("openxlsx")
library("tibble")
library("janitor")
#--------------------------------------------------------------------------------------
#' Load atsdr Source into dev_toxval_source_v3. 
#' @param toxval.db The version of toxval into which the source is loaded.
#' @param infile The input file ./atsdr/atsdr_files/ATSDR_MRLs_2020_Sept2020_Temp.xlsx

#--------------------------------------------------------------------------------------

import_atsdr_source <- function(toxval.db,infile) {
  printCurrentFunction(toxval.db)

  #####################################################################
  cat("Build original_atsdr_table from new source file and fix date conversions \n")
  #####################################################################
  res <- read.xlsx(infile)
  names.list <- c("source_name_sid", "casrn", "name","source_url", "data_collection", "source_name_cid", "route","duration","mrl","total_factors","endpoint", "status","date")
  res <- res[3:468,]
  names(res) <- names.list
  res$date <- excel_numeric_to_date(as.numeric(as.character(res$date)), date_system = "modern")
  res$date <- format(res$date, format = "%d-%b-%Y")

  runInsertTable(res,"original_atsdr_table",toxval.db,do.halt=T,verbose=F)
  #####################################################################
  cat("Build new_atsdr table from res \n")
  #####################################################################
  
  #subset required fields
  names.list <- c("source_name_sid", "casrn", "name","source_url", "subsource", "source_name_cid", "exposure_route","duration","mrl","uncertainty_factor","critical_effect", "status","date")
  names(res) <- names.list
  res$study_type <- res$duration
  res[grep("15 - 364 days\\.|15 - 364 daysermediate",res$study_type),"study_type"] <- "intermediate"
  res[grep("1 - 14 days",res$study_type),"study_type"] <- "acute"
  res[grep("1 year or.*",res$study_type),"study_type"] <- "chronic"
  
  res[grep("Inh.*",res$exposure_route),"exposure_route"] <- "Inhalation"
  res[grep("Rad.*",res$exposure_route),"exposure_route"] <- "External Radiation MRLs"
  
  res$toxval_numeric <-  res$mrl
  res$toxval_numeric <-  gsub("(\\d+\\.*\\d*)(\\s*.*)","\\1",res$toxval_numeric)
  res$toxval_numeric <- as.numeric(res$toxval_numeric)
  res$toxval_units <- gsub("(\\d+\\.*\\d*\\s*)(.*)","\\2",res$mrl)
  
  res$study_duration_value <- res$duration
  res$study_duration_value <- gsub("(\\d+\\s*\\-*\\s*\\d*)(\\s*.*)","\\1",res$study_duration_value)
  res$study_duration_value <- gsub("^\\s+|\\s+$","",res$study_duration_value)
  
  res$study_duration_units <- res$duration
  res[grep("chronic",res$study_type),"study_duration_units"] <- "year"
  res[grep("acute",res$study_type),"study_duration_units"] <- "day"
  res[grep("intermediate",res$study_type),"study_duration_units"] <- "day"
  
  
  res[grep("15 - 364",res$study_duration_value),"study_duration_value"] <- "364"
  res[grep("1 - 14",res$study_duration_value),"study_duration_value"] <- "14"
  res$study_duration_value <-  as.numeric(res$study_duration_value)
  
  res$year <- res$date
  res$year <-  gsub("(\\d+\\-\\w+\\-)(\\d{4})", "\\2", res$year)
  
  name.list <- c("casrn","name","source_url","subsource","exposure_route","study_type","toxval_numeric","toxval_units","uncertainty_factor","critical_effect","year", "study_duration_value","study_duration_units")
  res <- res[,name.list]
  #create copies of res
  res1 <- res
  res2 <- res
  # update res2 toxval_numeric field by multiplying values from existing toxval_numeric with uncertainity factors
  for(i in 1:nrow(res2)) res2[i,"toxval_numeric"] <- res2[i,"toxval_numeric"]*res2[i,"uncertainty_factor"]
  #assign new column toxval_type 
  res1$toxval_type <- "ATSDR MRL"
  res2$toxval_type <- "NOAEL"
  #combine res1 & res2 to create res and select required fields
  res <- rbind(res1,res2)
  res <- res[,c("casrn","name","source_url","exposure_route","study_type","toxval_numeric","toxval_units","toxval_type","critical_effect","year", "subsource", "study_duration_value","study_duration_units")]
  
  res$source_url <- gsub("#", "",res$source_url)
  
  res[grep("Neurol.",res$critical_effect),"critical_effect"] <- "neurologic"
  res[grep("Hemato.",res$critical_effect),"critical_effect"] <- "hematological"
  res[grep("Resp.",res$critical_effect),"critical_effect"] <- "respiratory"
  res[grep("Gastro.",res$critical_effect),"critical_effect"] <- "gastrointestinal"
  res[grep("Repro.",res$critical_effect),"critical_effect"] <- "reproductive"
  res[grep("Develop.",res$critical_effect),"critical_effect"] <- "developmental"
  res[grep("Metab.",res$critical_effect),"critical_effect"] <- "metabolic"
  res[grep("Body Wt.",res$critical_effect),"critical_effect"] <- "body weight"
  res[grep("Immuno.",res$critical_effect),"critical_effect"] <- "immunological"
  res[grep("Musculo.",res$critical_effect),"critical_effect"] <- "musculoskeletal"
  res[grep("Reprod.",res$critical_effect),"critical_effect"] <- "reproductive"
  res[grep("Endocr.",res$critical_effect),"critical_effect"] <- "endocrine"
  res[grep("Lymphor.",res$critical_effect),"critical_effect"] <- "lymphatic"
  
  res$toxval_units <- gsub("µ","u", res$toxval_units)
  res$toxval_units <- gsub("\\/day","-day", res$toxval_units)
  res$exposure_route <- tolower(res$exposure_route)
  res[grep("external.*",res$exposure_route),"exposure_route"] <- "radiation"
  
  
  # assign appropriate data types
  res <- lapply(res, function(x) type.convert(as.character(x), as.is = T))
  res <- data.frame(res, stringsAsFactors = F)
  
  # fix casrn
  for(i in 1:nrow(res)) res[i,"casrn"] <- fix.casrn(res[i,"casrn"])
  res["atsdr_id"] <- c(1:length(res[,1]))
  runInsertTable(res,"new_atsdr",toxval.db,do.halt=T,verbose=F)
  #####################################################################
  cat("Build atsdr_chemical_information table from res\n")
  #####################################################################
  chemical_information <- res[,c("name","casrn")]
  chemical_information <- unique(chemical_information[,1:2])
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  chemical_information <- chemical_information[c('chemical_id','name','casrn')]
  
  runInsertTable(chemical_information,"atsdr_chemical_information",toxval.db,do.halt=T,verbose=F)

  
}

