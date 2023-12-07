#--------------------------------------------------------------------------------------
#' @#' Load chiu Source into dev_toxval_source_v3.
#' Data from the Chiu et al. paper on RfD values
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile The input file ./chiu/chiu_files/Full_RfD_databaseQAed-FINAL.xlsx
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn, #' stop to look at the results in indir/chemcheck.xlsx
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
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
#' @rdname import_chiu_source
#' @importFrom openxlsx read.xlsx
#--------------------------------------------------------------------------------------
import_chiu_source <- function(db,
                               infile="Full_RfD_databaseQAed-FINAL.xlsx",
                               chem.check.halt=F) {
  printCurrentFunction(db)
  infile = paste0(toxval.config()$datapath,"chiu/chiu_files/",infile)
  #####################################################################
  cat("Build original_chiu from source file \n")
  #####################################################################
  res0 <- openxlsx::read.xlsx(infile)
  res1 <- res0[,c("strCAS","strName","Source","Type","numValue","strHyperlink","strReference","strUnitsPOD","strCriticalEffect","strDateAssessed",
                  "Species","Strain","strDuration","Duration.type","tblOrgan_strSex","numUF","numUFa","numUFh","numUFs","numUFl","numUFd","numUFother","Route")]
  res2 <- res0[,c("strCAS","strName","Source","POD.type","numPOD","strHyperlink","strReference","strUnitsPOD","strCriticalEffect","strDateAssessed",
                  "Species","Strain","strDuration","Duration.type","tblOrgan_strSex","numUF","numUFa","numUFh","numUFs","numUFl","numUFd","numUFother","Route")]
  name.list <- c("casrn","name","subsource","toxval_type","toxval_numeric","record_url","long_ref","toxval_units","critical_effect","year",
                 "species","strain","study_duration_value","study_duration_units","sex","uncertainty_factor","ufa","ufh","ufs","ufl","ufd","ufother","exposure_route")
  names(res1) <- name.list
  names(res2) <- name.list
  res <- rbind(res1,res2)

  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="Chiu",table="source_chiu",res=res,F,T,T)
}
