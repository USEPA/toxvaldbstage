#--------------------------------------------------------------------------------------
#' Load pprtv_ornl Source into dev_toxval_source_v2.
#' @param db The version of toxval into which the source is loaded.
#' @param infile The input file ./pprtv_ornl/pprtv_ornl_files/new_PPRTV_ORNL cancer noncancer.xlsx
#--------------------------------------------------------------------------------------
import_pprtv_ornl_source <- function(db,
                                     infile="../pprtv_ornl/pprtv_ornl_files/new_PPRTV_ORNL cancer noncancer.xlsx",
                                     chem.check.halt=F) {
  printCurrentFunction(db)

  #####################################################################
  cat("Build new_pprtv_ornl table\n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile)
  # res["pprtv_ornl_id"] <- c(1:length(res[,1]))
  # res <- res[c('pprtv_ornl_id', names(res[-23]))]

  res = res[!is.element(res$casrn,c("VARIOUS","MULTIPLE")),]
  cat(nrow(res),"\n")
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="PPRTV (ORNL)",table="source_pprtv_ornl",res=res,F,T,T)
}
