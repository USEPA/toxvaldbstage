library("openxlsx")
library("stringr")
#--------------------------------------------------------------------------------------
#' Load health_canada Source Info into dev_toxval_source_v2. 
#' @param toxval.db The version of toxval into which the source info is loaded.
#' @param infile The input file ./health_canada/health_canada_files/HealthCanada_TRVs_2010_AppendixA v2.xlsx


#--------------------------------------------------------------------------------------

import_health_canada_source <- function(toxval.db,infile) {
  printCurrentFunction(toxval.db)

  #####################################################################
  cat("Build original_health_canada_table \n")
  #####################################################################
  res <- read.xlsx(infile, 1, colNames = T)
  runInsertTable(res,"original_health_canada_table",toxval.db,do.halt=T,verbose=F)
  
  #####################################################################
  cat("Build health_canada_chemical_information table from res\n")
  #####################################################################
  chemical_information <- res[,3:4]
  chemical_information <- unique(chemical_information[,1:2])
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  names(chemical_information)[1] <- tolower(names(chemical_information)[1])
  chemical_information <- chemical_information[c('chemical_id','name','casrn')] 
  runInsertTable(chemical_information,"health_canada_chemical_information",toxval.db,do.halt=T,verbose=F)
  
  #####################################################################
  cat("create dataframe res1 and build health_canada_duration_replacement_table\n")
  #####################################################################
  res1 <- res
  dur_sym_val <- grep("^[^[:alnum:] ]", res1$duration, value = T )
  dur_sym_new_val1 <- gsub(dur_sym_val[1], "<= 60 years", dur_sym_val[1])
  dur_sym_new_val2 <- gsub(dur_sym_val[2], ">= 12 months occupational exposure",dur_sym_val[2])
  dur_sym_new_val3 <- gsub(dur_sym_val[3], ">= 12 months occupational exposure",dur_sym_val[3])
  res1$duration[res1$duration %in% dur_sym_val[1:3]] <- c(dur_sym_new_val1,dur_sym_new_val2,dur_sym_new_val3)
  res1 <- as.data.frame(apply(res1, 2, function(y) as.character(gsub("µ", "u", y))),stringsAsFactors = F)
  res1 <- as.data.frame(apply(res1, 2, function(y) as.character(gsub("[³]", "3", y))),stringsAsFactors = F)
  res1$study_duration_value <- res1$duration 
  replacement_value <- c("days 1-24 (rabbits) and 1-19 (rats) of gestational period","Duration and Dosing Regime: single bolus dose (0, 12.5, 50, 200, or 800 ng 2,3,7,8-TCDD)/kg-bw) on day 15 of gestation (Oshako et al., 2001); subcutaneous loading dose 25, 60, or 300 ng TCDD/kgbw) followed by weekly maintenance doses (5, 12, or 60 ng TCDD/kgbw) beginning 2 weeks prior to mating, and continuing through mating, gestation and lactation (Faqi et al., 1998)",
                         "F0: prior to and during mating (males and females) and throughout gestation lactation; F1: from weaning through reproduction until weaning of F2 pups","gestational days 0-17","nd","chronic","1, 2, 6, 8 weeks (various groups)","4 and 8 months","105 to 107 weeks","1 gestational period, 48 d post-natal exposure","3 dosing regimes: for 3 months before pregnancy, for 2 months before and 21 d during pregnancy, or for 21 d during pregnancy only")
  new_replacement_value <- c(19,15,2,17,0,0,8,8,107,48,3)
  new_replacement_unit <- c("days","days","generations","days","-","-", "weeks","months","weeks","days","months")
  replacement_table <- data.frame(replacement_value, new_replacement_value, new_replacement_unit, stringsAsFactors = F)
  runInsertTable(replacement_table,"health_canada_duration_replacement_table",toxval.db,do.halt=T,verbose=F)
  
  #####################################################################
  cat("Build new_health_canada_table with new_study_duration_values, new_study_duration_units and new_study_duration_qualifiers\n")
  #####################################################################
  
  for (i in 1:length(replacement_table$replacement_value)){
    for (k in 1:nrow(res1)){
      res1$study_duration_value[k][res1$duration[k] %in% replacement_table$replacement_value[[i]]] <- replacement_table$new_replacement_value[i]
    }
  }
  
  
  res1$study_duration_value <-  gsub("^\\D+(\\d.*)", "\\1", res1$study_duration_value)
  res1$study_duration_units <- word(res1$study_duration_value,2)
  res1$study_duration_value <- word(res1$study_duration_value,1)
  num_alpha_val <- grep("[0-9]+[a-zA-Z]+", res1$study_duration_value, value = T)
  num_alpha_unit <- gsub("\\d","",num_alpha_val)
  num_alpha_vals <- gsub("[a-zA-Z]","",num_alpha_val)
  res1$study_duration_value[res1$duration %in% num_alpha_val] <- num_alpha_vals
  res1$study_duration_units[res1$duration %in% num_alpha_val] <- num_alpha_unit
  
  for (i in 1:length(replacement_table$replacement_value)){
    for (k in 1:nrow(res1)){
      res1$study_duration_units[k][res1$duration[k] %in% replacement_table$replacement_value[[i]]] <- replacement_table$new_replacement_unit[i]
    }
  }
  
  
  res1$study_duration_value <- gsub(".*-", "", res1$study_duration_value)
  res1$study_duration_units <- gsub("\\bd\\b","days", res1$study_duration_units)
  res1$study_duration_units <- gsub("\\bh\\b","hours", res1$study_duration_units)
  res1$study_duration_units <- gsub("\\bwk\\b","weeks", res1$study_duration_units)
  res1$study_duration_units <- gsub("\\byear\\b","years", res1$study_duration_units)
  res1$study_duration_units <- gsub("\\,|\\;|\\s+|\\)", "",res1$study_duration_units)
  res1$study_duration_value <-  as.numeric(res1$study_duration_value)
  
  res1$study_duration_qualifier <- res1$duration
  res1$study_duration_qualifier <- gsub("[a-zA-Z0-9.,\\+() ;:/-]", "",res1$study_duration_qualifier)
  res1["healthcanada_id"] <- c(1:length(res1[,1]))
  res1 <- res1[c("healthcanada_id",names(res1[1:14]), names(res1[22:24]), names(res1[15:21]))]
  names(res1) <- tolower(names(res1))
  res1$strain <- "-"
  res1$sex <- "-"
  res1$exposure_method <- gsub(".*\\:","",res1$exposure_route)
  res1$exposure_method <- gsub("^\\s+","",res1$exposure_method)
  res1$exposure_route <- gsub("\\:.*","",res1$exposure_route)
  res1$exposure_method[res1$exposure_method == res1$exposure_route] <- "nd"  
  
  open_paranthesis_effect <- which(str_count(res1$critical_effect,"\\)") == 0 & str_count(res1$critical_effect,"\\(") == 1)
  open_paranthesis_effect2 <- grep("[^\\)]$",res1[which(str_count(res1$critical_effect,"\\)") == 0 & str_count(res1$critical_effect,"\\(") == 1),"critical_effect"])
  
  critical_effect_to_clean <- open_paranthesis_effect[open_paranthesis_effect2]
  res1[critical_effect_to_clean,"critical_effect"] <- gsub("\\(","",res1[critical_effect_to_clean,"critical_effect"])
  
  open_paranthesis_effect <-which(str_count(res1$critical_effect,"\\(") == 6)
  res1[open_paranthesis_effect,"critical_effect"] <- gsub("(.*\\(.*\\)\\;\\s+)(\\()(.*)","\\1\\3",res1[open_paranthesis_effect,"critical_effect"])
  
  
  runInsertTable(res1,"new_health_canada_table",toxval.db,do.halt=T,verbose=F)
  
 
}