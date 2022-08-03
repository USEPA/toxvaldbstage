#-------------------------------------------------------------------------------------
#'
#' Set the dtxsid values for all of the other tables
#'
#' @param toxval.db The version of toxvaldb to use.
#' @param table the Database table to be updated
#' @param verbose If TRUE, print out extra diagnostic messages
#-------------------------------------------------------------------------------------
set.dtxsid.by.source <- function(toxval.db,table,source, verbose=T) {
  printCurrentFunction(paste(toxval.db,":",table, ":", source))
  query <- paste0("
                  update ",table," a, source_chemical b
                  set a.dtxsid=b.dtxsid
                  where a.chemical_id=b.chemical_id
                  and a.dtxsid='-' and b.source
                  like '",source,"'")
  runQuery(query,toxval.db)
  v2 <- F
  if(v2) {
    query <- paste0("select toxval_id,chemical_id from ",table," where dtxsid='-'")
    res <- runQuery(query,toxval.db)
    for(i in 1:nrow(res)) {
      tid <- res[i,1]
      cid <- res[i,2]
      query <- paste0("select dtxsid from source_chemical where chemical_id=",cid," and source like '",source,"'")
      dtxsid <- runQuery(query,toxval.db)
      query <- paste0("update toxval set dtxsid='",dtxsid,"' where toxval_id=",tid," and source like '",source,"'")
      dtxsid <- runQuery(query,toxval.db)
    }
  }
}
