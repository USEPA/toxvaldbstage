library("openxlsx")

#--------------------------------------------------------------------------------------
#' Load efsa2 Source into dev_toxval_source_v2. 
#' @param toxval.db The version of toxval into which the source is loaded.
#' @param infile The input file ./efsa2/efsa2_files/merge2/EFSA_combined_new.xlsx

#--------------------------------------------------------------------------------------

import_efsa2_source <- function(toxval.db,infile) {
  printCurrentFunction(toxval.db)

  #####################################################################
  cat("Build new_efsa2 table\n")
  #####################################################################
  res <- read.xlsx(infile)
  res <- lapply(res, function(x) type.convert(as.character(x),as.is = T))
  res <- data.frame(res,stringsAsFactors = F)
  res["new_efsa2_id"] <- c(1:length(res[,1]))
  res <- res[c('new_efsa2_id', names(res[-19]))]
  runInsertTable(res,"new_efsa2",toxval.db,do.halt=T,verbose=F)
  #####################################################################
  cat("Build efsa2_chemical_information table from res\n")
  #####################################################################
  chemical_information <- res[,2:3]
  chemical_information <- unique(chemical_information[,1:2])
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  chemical_information <- chemical_information[c('chemical_id','name','casrn')]

  runInsertTable(chemical_information,"efsa2_chemical_information",toxval.db,do.halt=T,verbose=F)

  
}

