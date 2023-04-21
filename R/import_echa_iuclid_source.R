# library("openxlsx")
# library("stringr")
#--------------------------------------------------------------------------------------
#' Load ECHA IUCLID Source into dev_toxval_source_v4.
#' @param toxval.db The version of toxval into which the source is loaded.
#' @param infile The input file ./echa_iuclid/echa_iuclid_files/echa_iuclid_v8.xlsx

#--------------------------------------------------------------------------------------
import_echa_iuclid_source <- function(toxval.db,infile, verbose = T) {
  printCurrentFunction(toxval.db)

  #####################################################################
  cat("Build new_echa_iuclid table\n")
  #####################################################################

  res <- openxlsx::read.xlsx(infile)

  runInsertTable(res,"new_echa_iuclid",toxval.db,do.halt=T,verbose=F)

  #####################################################################
  cat("Build echa_iuclid_chemical_information table from res\n")
  #####################################################################
  chemical_information <- res[,c("casrn","name")]
  chemical_information <- unique(chemical_information[,1:2])
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  chemical_information <- chemical_information[c('chemical_id','name','casrn')]

  runInsertTable(chemical_information,"echa_iuclid_chemical_information",toxval.db,do.halt=T,verbose=F)

}
