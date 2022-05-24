library("openxlsx")

#--------------------------------------------------------------------------------------
#' Load wignall Source data into dev_toxval_source_v2. 
#' @param toxval.db The version of toxval into which the source info is loaded.
#' @param infile The input file ./wignall/wignall_files/BMD_Results_2014-06-17_reviewed Mar 2018.xlsx


#--------------------------------------------------------------------------------------
import_wignall_source <- function(toxval.db,infile) {
  printCurrentFunction(toxval.db)

  #####################################################################
  cat("Build original_wignall_table and new_wignall_table \n")
  #####################################################################
  res1 <- read.xlsx(infile,1,startRow = 1)
  runInsertTable(res1,"original_wignall_table",toxval.db,do.halt=T,verbose=F)
  res1["new_toxval_units"] <- gsub(".*\\(|\\)", "",res1$Toxicity.value.type )
  res1["new_toxval_type"] <- gsub("\\(.*|\\)", "",res1$Toxicity.value.type )
  res1["UF"] <- res1$POD / res1$Toxicity.Value
  
  colnames(res1) <- c("wignall_id", "casrn","name","original_toxval_type","original_toxval_units", "subsource", 
                      "toxval_numeric", "POD_numeric", "POD_units","POD_type","organ","effect",
                      "effect_description","dose_number","dose_values","dose_units","dose_converted",
                      "DR_type","mean_response","response_units","SD_of_response",
                      "total_number_of_animals","incidence_in_number_of_animals","BMR",
                      "BMD","BMDL","BMD/L_WIZARD_notes","action_taken","BMD'","BMDL'","BMD/L'_WIZARD_notes",
                      "comments","hyperlink","reference","toxval_units","toxval_type","UF")
  
  res1 <- res1[,c(1,2,3,7,35,36,4,5,6,8:10,37,11:34)]
  for(i in 1:length(res1$casrn)){
    res1$casrn[i] <- fix.casrn(res1$casrn[i],cname="",verbose=F)
    
  }
  #res1$casrn <- fix.casrn(res1$casrn,cname="",verbose=F)
  res1$critical_effect <- paste(res1$effect, res1$effect_description, sep = ";")
  res1$toxval_type <- gsub("\\s+$","", res1$toxval_type)
  runInsertTable(res1,"new_wignall_table",toxval.db,do.halt=T,verbose=F)
  
  
  
  #####################################################################
  cat("Build wignall_chemical_information table from res1\n")
  #####################################################################
  chemical_information <- res1[,2:3]
  chemical_information <- unique(chemical_information[,1:2])
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  chemical_information <- chemical_information[c('chemical_id','name','casrn')]
  runInsertTable(chemical_information,"wignall_chemical_information",toxval.db,do.halt=T,verbose=F)
  
  #####################################################################
  #cat("Build new_wignall_table  \n")
  #####################################################################
  
  #query <- "select wwt.*, ci.chemical_id from whole_wignall_table wwt inner join wignall_chemical_information ci on ci.name = wwt.name and ci.casrn =wwt.casrn"
  #res2 <- runQuery(query,toxval.db)
  #res2_var <- names(res2) %in% c("name","casrn")
  #res2 <- res2[!res2_var]
  #runInsertTable(res2,"new_wignall_table",toxval.db,do.halt=T,verbose=F)
  
}