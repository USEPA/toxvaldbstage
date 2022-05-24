library("openxlsx")
#--------------------------------------------------------------------------------------
#' Load ECHA echemportal 2020 Source into dev_toxval_source_v4. 
#' @param toxval.db The version of toxval into which the source is loaded.
#' @param infile The input file ./echa_echemportal/echa_echemportal_files/eChemPortal mammalian data 2020 step 3.xlsx ,build from echemportal.prep.v2.step3.R

#--------------------------------------------------------------------------------------
import_echa_echemportal_source <- function(toxval.db,infile) {
  printCurrentFunction(toxval.db)
  
  #####################################################################
  cat("Build new_echa table\n")
  #####################################################################
  
  echa_table <- read.xlsx(infile,1)
  
  # assign generation info from critical effect to generation
  echa_table$generation_1 <- echa_table$generation
  gen_info <- c("p0","p1","f1","f2","\\bp\\b","\\bmaternal\\b","\\bfetal\\b","f3a")
  echa_table$generation <- str_extract_all(echa_table$critical_effect, regex(paste(gen_info, collapse = "|"), ignore_case = T)) %>% sapply(., paste, collapse = ", ")
  
  echa_table[which(echa_table$generation == ""),"generation"] <- echa_table[which(echa_table$generation == ""),"generation_1"]
  echa_table$generation <- tolower(echa_table$generation)
  echa_table[which(echa_table$generation == "fetus"),"generation"] <-"fetal"
  echa_table[which(is.na(echa_table$generation)|(echa_table$generation == "")), "generation"] <- "-"
  
  
  echa_table <- echa_table[,names(echa_table)[(names(echa_table)!= "generation_1")]]
  
  runInsertTable(echa_table,"new_echa",toxval.db,do.halt=T,verbose=F)
  
  #####################################################################
  cat("Build echa_chemical_information table from echa_table\n")
  #####################################################################
  chemical_information <- echa_table[,c("casrn","name")]
  chemical_information <- unique(chemical_information[,1:2])
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  chemical_information <- chemical_information[c('chemical_id','name','casrn')]
  
  runInsertTable(chemical_information,"echa_chemical_information",toxval.db,do.halt=T,verbose=F)
  
}
  