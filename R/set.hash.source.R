#-------------------------------------------------------------------------------------
#' Set the hash in source table
#' @param source.db The source database version where data is getting altered (eg: dev_toxval_source_v4).
#' @param source The table in source where the hash data is getting altered.
#' @param do.reset if TRUE, reset all hashes and start from scratch.
#' Otherwise only update empty values
#' @return The database will be altered
#' @export
#--------------------------------------------------------------------------------------
set.hash.source <- function(source.db,source, reset=F){
  printCurrentFunction(paste(source.db,":", source))
  
  if(reset) runQuery(paste0("update ",source,"  set source_hash='-'"),source.db)
  runQuery(paste0("update ",source," set source_hash='-' where source_hash is null "),source.db)
  mat = runQuery(paste0("select * from ",source,"
                 where source_hash='-' "), source.db)
  cat("Number of records to fix: ",dim(mat)[1],"\n")
  
  
  if(dim(mat)[1]>0) {
    for (i in 1:nrow(mat)){
      row <- mat[i,2:dim(mat)[2]]
      val <- digest(paste0(row,collapse=""), serialize = FALSE)
      query <- paste0("update ",source," set source_hash = '",val,"' where source_id = ",mat[i,1]," ")
      runInsert(query,source.db)
      if(i%%1000==0) {
        cat(i," out of ",nrow(mat),"\n")
      }
    }
  }
  
}
