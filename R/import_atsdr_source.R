library("openxlsx")
library("tibble")
library("janitor")
#--------------------------------------------------------------------------------------
#' Load atsdr Source into dev_toxval_source_v3.
#' @param db The version of toxval into which the source is loaded.
#' @param infile The input file ./atsdr/atsdr_files/ATSDR_MRLs_2020_Sept2020_Temp.xls
#'
#--------------------------------------------------------------------------------------
import_atsdr_source <- function(db,
                                infile="ATSDR_MRLs_2020_Sept2020_Temp.xlsx",
                                indir="../atsdr/atsdr_files/",
                                chem.check.halt=F) {
  printCurrentFunction(db)
  source="ATSDR MRLs 2020"
  #####################################################################
  cat("Build original_atsdr_table from new source file and fix date conversions \n")
  #####################################################################
  infile = paste0(indir,infile)
  res <- openxlsx::read.xlsx(infile)
  names.list <- c("source_name_sid", "casrn", "name","source_url", "data_collection", "source_name_cid", "route","duration","mrl","total_factors","endpoint", "status","date")
  res <- res[3:468,]
  names(res) <- names.list
  res$date <- excel_numeric_to_date(as.numeric(as.character(res$date)), date_system = "modern")
  res$date <- format(res$date, format = "%d-%b-%Y")

  res = res[!is.element(res$casrn,"NOCAS"),]
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="ATSDR MRLs 2020",table="source_atsdr",res=res,F,T,T)
}

