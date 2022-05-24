library("openxlsx")

#--------------------------------------------------------------------------------------
#' Load niosh Source into dev_toxval_source_v4. 
#' @param toxval.db The version of toxval into which the source is loaded.
#' @param infile The input file ./niosh/niosh_files/niosh_IDLH_2020.xlsx

#--------------------------------------------------------------------------------------

import_niosh_source <- function(toxval.db,infile) {
  printCurrentFunction(toxval.db)

  #####################################################################
  cat("Build new_niosh table\n")
  #####################################################################
  res <- read.xlsx(infile)
  res["niosh_id"] <- c(1:length(res[,1]))
  res <- res[c('niosh_id', names(res[-8]))]
  
  runInsertTable(res,"new_niosh",toxval.db,do.halt=T,verbose=F)
  #####################################################################
  cat("Build niosh_chemical_information table from res\n")
  #####################################################################
  chemical_information <- res[,2:3]
  chemical_information <- unique(chemical_information[,1:2])
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  chemical_information <- chemical_information[c('chemical_id','name','casrn')]
  
  runInsertTable(chemical_information,"niosh_chemical_information",toxval.db,do.halt=T,verbose=F)

  
}

