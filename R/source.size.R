#--------------------------------------------------------------------------------------
#' print out the size of each of the tables in toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#--------------------------------------------------------------------------------------
source.size <- function(db="res_toxval_source_v5") {
  printCurrentFunction(db)

  tlist = runQuery("show tables",sdb)[,1]
  for(table in tlist) {
    if(substr(table,1,7)=="source_") {
      query = paste0("select count(*) from ",table)
      n=runQuery(query,db)[1,1]
      cat(table,":",n,"\n")
    }
  }

}
