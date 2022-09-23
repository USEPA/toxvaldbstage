#--------------------------------------------------------------------------------------
#' Load Health Canada Source Info into toxval_source
#' @param db The version of toxval_source into which the source info is loaded.
#' @param infile The input file ./health_canada/health_canada_files/HealthCanada_TRVs_2010_AppendixA v2.xlsx
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#--------------------------------------------------------------------------------------
import_health_canada_source <- function(db,
                                        infile="HealthCanada_TRVs_2010_AppendixA v2.xlsx",
                                        chem.check.halt=T) {
  printCurrentFunction(db)
  infile = paste0(infile,"health_canada/health_canada_files/",infile)
  #####################################################################
  cat("Build original_health_canada_table \n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile, 1, colNames = T)

  names(res)[is.element(names(res),"CASRN")] = "casrn"
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="Health Canada",table="source_health_canada",res=res,F,T,T)
}
