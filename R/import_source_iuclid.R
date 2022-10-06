#--------------------------------------------------------------------------------------
#' A generic template for adding data to toxval_source for a new source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param subf The subfolder containing the IUCLID subsource
#--------------------------------------------------------------------------------------
import_source_iuclid <- function(db, subf, chem.check.halt=F) {
  printCurrentFunction(db)
  source = "name of the source"
  source_table = "source_{source}"
  dir = paste0(toxval.config()$datapath,"iuclid/",subf,"/",subf,"_files/")
  file = paste0(dir,subf,".csv")
  res = read.csv(file)
  #####################################################################
  cat("Load the data\n")
  #####################################################################
  source_prep_and_load(db,source="IUCLID",table=paste0("source_IUCLID_",subf),res=res,F,T,T)
}
