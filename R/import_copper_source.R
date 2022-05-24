library("openxlsx")
library("tibble")
library("janitor")
#--------------------------------------------------------------------------------------
#' Load copper manufacturers Source into dev_toxval_source_v4. 
#' @param toxval.db The version of toxval into which the source is loaded.
#' @param infile The input file ./copper/copper_files/Copper Data Entry - Final.xlsx

#--------------------------------------------------------------------------------------

import_copper_source <- function(toxval.db,infile) {
  printCurrentFunction(toxval.db)
  
  #####################################################################
  cat("create original_copper_table from source file\n")
  #####################################################################
  res <- read.xlsx(infile)
  names(res)[c(7,34)]<- c("toxval_subtype1","year1")
  res$toxval_numeric_qualifier <- as.character(res$toxval_numeric_qualifier)
  runInsertTable(res,"original_copper_table",toxval.db,do.halt=T,verbose=F)
  
  
  #####################################################################
  cat("Build new_copper_table from res1 \n")
  #####################################################################
  res1 <- res
  res1[which(!is.na(res1[,"toxval_subtype1"])),"toxval_subtype"] <- res1[which(!is.na(res1[,"toxval_subtype1"])),"toxval_subtype1"]
  res1 <- res1[ , !(names(res1) %in% c("toxval_id","toxval_subtype1","year1"))]
  res1[grep("^[^[:alnum:]]",res1$toxval_numeric), "toxval_numeric_qualifier"] <- res1[grep("^[^[:alnum:]]",res1$toxval_numeric), "toxval_numeric"]
  res1[grep("^[^[:alnum:]]",res1$toxval_numeric), "toxval_numeric_qualifier"] <- gsub("(^[^[:alnum:]])(.*)","\\1",res1[grep("^[^[:alnum:]]",res1$toxval_numeric), "toxval_numeric_qualifier"])
  res1[grep("^[^[:alnum:]]",res1$toxval_numeric), "toxval_numeric"] <- gsub("(^[^[:alnum:]])(.*)","\\2",res1[grep("^[^[:alnum:]]",res1$toxval_numeric), "toxval_numeric"])
  res1[grep("\\-",res1$toxval_numeric), "toxval_numeric"] <- gsub("(.*)(\\s*\\-\\s*)(.*)","\\1",res1[grep("\\-",res1$toxval_numeric), "toxval_numeric"])
  
  res1[grep("^[a-zA-Z]+",res1$study_duration_value), "study_duration_value"] <- gsub("(^[a-zA-Z]+\\s*[a-zA-Z]+\\s*)(.*)","\\2",res1[grep("^[a-zA-Z]+",res1$study_duration_value), "study_duration_value"])
  res1[grep("\\-",res1$study_duration_value), "study_duration_value"] <- gsub("(.*)(\\s*\\-\\s*)(.*)","\\3",res1[grep("\\-",res1$study_duration_value), "study_duration_value"])
  res1[which(res1$volume == "-"),"volume"] <- ""
  
  res1["copper_id"] <- c(1:length(res1[,1]))
  res1 <- res1[c("copper_id",names(res1[-34]))]
  
  res1 <- lapply(res1, function(x) type.convert(as.character(x), as.is = T))
  res1 <- data.frame(res1, stringsAsFactors = F)
  
  runInsertTable(res1,"new_copper_table",toxval.db,do.halt=T,verbose=F)
  #####################################################################
  cat("Build copper_chemical_information table from res1\n")
  #####################################################################
  chemical_information <- res1[,c("name","casrn")]
  chemical_information <- unique(chemical_information[,1:2])
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  chemical_information <- chemical_information[c('chemical_id','name','casrn')]
  
  runInsertTable(chemical_information,"copper_chemical_information",toxval.db,do.halt=T,verbose=F)
  
  
}
