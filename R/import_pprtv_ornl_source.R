library("openxlsx")

#--------------------------------------------------------------------------------------
#' Load pprtv_ornl Source into dev_toxval_source_v2. 
#' @param toxval.db The version of toxval into which the source is loaded.
#' @param infile The input file ./pprtv_ornl/pprtv_ornl_files/new_PPRTV_ORNL cancer noncancer.xlsx

#--------------------------------------------------------------------------------------

import_pprtv_ornl_source <- function(toxval.db,infile) {
  printCurrentFunction(toxval.db)

  #####################################################################
  cat("Build new_pprtv_ornl table\n")
  #####################################################################
  res <- read.xlsx(infile)
  res["pprtv_ornl_id"] <- c(1:length(res[,1]))
  res <- res[c('pprtv_ornl_id', names(res[-23]))]
  
  
  runInsertTable(res,"new_pprtv_ornl",toxval.db,do.halt=T,verbose=F)
  #####################################################################
  cat("Build pprtv_ornl_chemical_information table from res\n")
  #####################################################################
  chemical_information <- res[,2:3]
  chemical_information <- unique(chemical_information[,1:2])
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  chemical_information <- chemical_information[c('chemical_id','name','casrn')]
  
  runInsertTable(chemical_information,"pprtv_ornl_chemical_information",toxval.db,do.halt=T,verbose=F)

  
}

