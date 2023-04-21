#--------------------------------------------------------------------------------------
#' Load efsa2 Source into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile The input file ./efsa2/efsa2_files/merge2/EFSA_combined_new.xlsx
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#--------------------------------------------------------------------------------------
import_efsa2_source <- function(db,
                                infile="EFSA_combined_new 2022-07-19.xlsx",
                                chem.check.halt=F) {
  printCurrentFunction(db)
  infile = paste0(toxval.config()$datapath,"efsa2/efsa2_files/merge2/",infile)
  #####################################################################
  cat("Build new_efsa2 table\n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile)
  res <- lapply(res, function(x) utils::type.convert(as.character(x),as.is = T))
  res <- data.frame(res,stringsAsFactors = F)
  res["new_efsa2_id"] <- c(1:length(res[,1]))
  res <- res[c('new_efsa2_id', names(res[-19]))]
  res = as.data.frame(res)
  x = substr(res$casrn,1,5)
  mask = vector(length(x),mode="integer")
  mask[] = 1
  mask[x=="ACToR"] = 0
  res = res[mask==1,]
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="EFSA2",table="source_efsa2",res=res,F,T,T)
}
