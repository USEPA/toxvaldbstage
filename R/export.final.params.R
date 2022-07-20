#-------------------------------------------------------------------------------------
#' Export the final values for the character params (e.g. toxval_type).
#'
#' @param toxval.db The version of toxval in which the data is altered.
#' @export
#--------------------------------------------------------------------------------------
export.final.params <- function(toxval.db) {
  printCurrentFunction(toxval.db)
  fields = runQuery("select distinct field from toxval_fix",db)[,1]
  allres = NULL
  for(field in fields) {
    field0 = paste0(field,"_original")
    query = paste0("select ",field,",",field0,",source from toxval")
    res = runQuery(query,toxval.db)
    res = unique(res)
    res$field = field
    res$infix = "N"
    names(res)[1] = "value"
    names(res)[2] = "value_original"
    res = res[,c("field","value","value_original","source","infix")]
    vals = runQuery(paste0("select distinct term_final from toxval_fix where field='",field,"'"),toxval.db)[,1]
    res[is.element(res$value,vals),"infix"] = "Y"
    allres = rbind(allres,res)
  }
  file = paste0("../dictionary/final_params ",Sys.Date(),".xlsx")
  write.xlsx(allres,file)
}
