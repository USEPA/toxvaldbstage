#-------------------------------------------------------------------------------------
#' Set all empty cells in record_source to '-'
#' @param toxval.db The version of toxval in which the data is altered.
#' @return The database will be altered
#' @export
#--------------------------------------------------------------------------------------
fix.empty.record_source <- function(toxval.db){
  printCurrentFunction(toxval.db)
  res <- runQuery("desc record_source",toxval.db)
  mask <- vector(mode="integer",length=dim(res)[1])
  mask[] <- 0
  for(i in 1:dim(res)[1]) {
    if(contains(res[i,"Type"],"varchar")) mask[i] <- 1
    if(contains(res[i,"Type"],"text")) mask[i] <- 1
  }
  cols <- res[mask==1,"Field"]

  for(col in cols) {
    print(col)
    query <- paste0("update record_source set ",col,"='-' where ",col,"=''")
    runQuery(query,toxval.db)
    query <- paste0("update record_source set ",col,"='-' where ",col," is null ")
    runQuery(query,toxval.db)
    query <- paste0("update record_source set ",col,"='-' where ",col,"=' '")
    runQuery(query,toxval.db)
    query <- paste0("update record_source set ",col,"='-' where ",col,"='  '")
    runQuery(query,toxval.db)
  }
}
