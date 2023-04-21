#--------------------------------------------------------------------------------------
#' @description Load atsdr Source into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile The input file ./atsdr/atsdr_files/ATSDR_MRLs_2020_Sept2020_Temp.xls
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#' @title FUNCTION_TITLE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[openxlsx]{read.xlsx}}
#'  \code{\link[janitor]{excel_numeric_to_date}}
#' @rdname import_atsdr_source
#' @export 
#' @importFrom openxlsx read.xlsx
#' @importFrom janitor excel_numeric_to_date
#--------------------------------------------------------------------------------------
import_atsdr_source <- function(db,
                                infile="ATSDR_MRLs_2020_Sept2020_Temp.xlsx",
                                chem.check.halt=F) {
  printCurrentFunction(db)
  indir = "atsdr/atsdr_files/"
  indir = paste0(toxval.config()$datapath,indir)

  source="ATSDR MRLs 2020"
  #####################################################################
  cat("Build original_atsdr_table from new source file and fix date conversions \n")
  #####################################################################
  infile = paste0(indir,infile)
  res <- openxlsx::read.xlsx(infile)
  names.list <- c("source_name_sid", "casrn", "name","source_url", "data_collection", "source_name_cid", "route","duration","mrl","total_factors","endpoint", "status","date")
  res <- res[3:468,]
  names(res) <- names.list
  res$date <- janitor::excel_numeric_to_date(as.numeric(as.character(res$date)), date_system = "modern")
  res$date <- format(res$date, format = "%d-%b-%Y")

  res = res[!generics::is.element(res$casrn,"NOCAS"),]
  #####################################################################
  cat("Prep and load the data\n")
  source_prep_and_load(db,source="ATSDR MRLs 2020",table="source_atsdr",res=res,F,T,T)
}

