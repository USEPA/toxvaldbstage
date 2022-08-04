#--------------------------------------------------------------------------------------
#' Set Toxval Defaults globally
#'
#' @param toxval.db The version of toxval from which to set defaults.
#' @export
#--------------------------------------------------------------------------------------
fill.toxval.defaults.global <- function(toxval.db){
  printCurrentFunction(toxval.db)
  defs <- runQuery("desc toxval",toxval.db)
  defs <- defs[,c(1,5)]
  col.list <- defs[is.element(defs[,"Default"],c("-","=")),1]
  col.list <- col.list[!is.na(col.list)]

  for(col in col.list){
    n <- runQuery(paste0("select count(*) from toxval where ",col," =''"),toxval.db)[1,1]
    if(n>0) {
      cat(col,n,"\n")
      query <- paste0("update toxval set ",col,"='-' where ",col," =''")
      runQuery(query,toxval.db)
    }
  }
}
