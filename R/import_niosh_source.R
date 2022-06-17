#--------------------------------------------------------------------------------------
#' Load niosh Source into dev_toxval_source_v4.
#' @param db The version of toxval into which the source is loaded.
#' @param infile The input file ./niosh/niosh_files/niosh_IDLH_2020.xlsx
#--------------------------------------------------------------------------------------
import_niosh_source <- function(db,
                                infile="../niosh/niosh_files/niosh_IDLH_2020.xlsx",
                                chem.check.halt=T) {
  printCurrentFunction(db)

  #####################################################################
  cat("Build new_niosh table\n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile)
  res["niosh_id"] <- c(1:length(res[,1]))
  res <- res[c('niosh_id', names(res[-8]))]
  res = res[res$casrn!="-",]
  res = res[res$casrn!="- ",]

  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="NIOSH",table="source_niosh",res=res,F,T,T)
}
