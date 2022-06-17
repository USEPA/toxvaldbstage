#--------------------------------------------------------------------------------------
#' Load efsa2 Source into dev_toxval_source_v2.
#' @param db The version of toxval into which the source is loaded.
#' @param infile The input file ./efsa2/efsa2_files/merge2/EFSA_combined_new.xlsx
#--------------------------------------------------------------------------------------
import_efsa2_source <- function(db,
                                infile="../efsa2/efsa2_files/merge2/EFSA_combined_new.xlsx",
                                chem.check.halt=T) {
  printCurrentFunction(db)

  #####################################################################
  cat("Build new_efsa2 table\n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile)
  res <- lapply(res, function(x) type.convert(as.character(x),as.is = T))
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
