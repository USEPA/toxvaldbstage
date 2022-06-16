#--------------------------------------------------------------------------------------
#' Load copper manufacturers Source into dev_toxval_source_v4.
#' @param db The version of toxval into which the source is loaded.
#' @param infile The input file ./copper/copper_files/Copper Data Entry - Final.xlsx
#--------------------------------------------------------------------------------------
import_copper_source <- function(db,
                                 infile="../copper/copper_files/Copper Data Entry - Final.xlsx",
                                 chem.check.halt=F) {
  printCurrentFunction(db)

  #####################################################################
  cat("create original_copper_table from source file\n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile)
  res = res[!is.na(res$casrn),]
  names(res)[c(7,34)]<- c("toxval_subtype1","year1")
  res$toxval_numeric_qualifier <- as.character(res$toxval_numeric_qualifier)

  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="Copper Manufacturers",table="source_copper",res=res,F,T,T)
}
