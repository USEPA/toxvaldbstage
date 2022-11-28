#-------------------------------------------------------------------------------------
#' Set all hyphen cells in toxval to 'Not Specified'
#' @param toxval.db The version of toxval in which the data is altered.
#' @return The database will be altered
#' @export
#--------------------------------------------------------------------------------------
fix.hyphen.by.source <- function(toxval.db,source){
  printCurrentFunction(paste(toxval.db,":", source))
  res <- runQuery("desc toxval",toxval.db)
  mask <- vector(mode="integer",length=dim(res)[1])
  mask[] <- 0
  for(i in 1:dim(res)[1]) {
    if(contains(res[i,"Type"],"varchar")) mask[i] <- 1
    if(contains(res[i,"Type"],"text")) mask[i] <- 1
  }
  cols <- res[mask==1,"Field"]
  
  for(col in cols) {
    print(col)
    query <- paste0("update toxval set ",col,"='Not Specified' where ",col,"='-' and source like '",source,"'")
    runQuery(query,toxval.db)
    
  }
}
