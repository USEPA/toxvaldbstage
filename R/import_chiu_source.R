#--------------------------------------------------------------------------------------
#' Load chiu Source into dev_toxval_source_v3.
#' Data from the Chiu et al. paper on RfD values
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param indir The directory where the output file will be placed
#' @param infile The input file ./chiu/chiu_files/Full_RfD_databaseQAed-FINAL.xlsx
#' @param chem.chek.halt If TRUE and there are bad chemical names or casrn,
#' stop to look at the results in indir/chemcheck.xlsx
#--------------------------------------------------------------------------------------
import_chiu_source <- function(db="dev_toxval_source_v5",
                               indir="../chiu/chiu_files/",
                               infile="Full_RfD_databaseQAed-FINAL.xlsx",
                               chem.check.halt=T) {
  printCurrentFunction(db)
  #####################################################################
  cat("Build original_chiu from source file \n")
  #####################################################################
  res0 <- openxlsx::read.xlsx(paste0(indir,infile))
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
  cat("Do the chemical checking\n")
  #####################################################################
  source = "Chiu"
  res = as.data.frame(res)
  res$source = source
  res$clowder_id = "-"
  res$document_name = "-"
  res = fix.non_ascii.v2(res,source)
  res = source_chemical.process(db,res,source,chem.check.halt,casrn.col="casrn",name.col="name")

  #####################################################################
  cat("Set the default values for missing data\n")
  #####################################################################
  res = source_set_defaults(res,source)

  #####################################################################
  cat("Set the clowder_id and document name\n")
  #####################################################################
  res = set_clowder_id(res,source)

  #####################################################################
  cat("Build the hash key and load the data \n")
  #####################################################################
  toxval_source.hash.and.load(db,source,"source_chiu",F,T,res)
}




