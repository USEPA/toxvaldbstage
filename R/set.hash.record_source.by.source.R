#-------------------------------------------------------------------------------------
#' Set the hash in table record_source
#' @param toxval.db The version of toxval in which the data is altered.
#' @param do.reset if TRUE, reset all hashes and start from scratch.
#' Otherwise only update empty values
#' @return The database will be altered
#' @export
#--------------------------------------------------------------------------------------
set.hash.record_source.by.source <- function(toxval.db,source, reset=T){
  printCurrentFunction(paste(toxval.db,":", source))
  if(reset) runQuery(paste0("update record_source set record_source_hash='-' where source like '",source,"'"),toxval.db)
  runQuery(paste0("update record_source set record_source_hash='-' where record_source_hash is null and source like '",source,"'"),toxval.db)
  
  mat <- runQuery(paste0("select
                  record_source_id,
                  source,
                  record_source_type,
                  record_source_note,
                  record_source_level,
                  document_name,
                  long_ref,
                  title,
                  author,
                  journal,
                  volume,year,issue,page,pmid,guideline,glp,quality
                  from record_source
                  where record_source_hash='-' and source like '",source,"'"), toxval.db)
  cat("Number of records to fix: ",dim(mat)[1],"\n")
  if(dim(mat)[1]>0) {
    for (i in 1:nrow(mat)){
      row <- mat[i,2:dim(mat)[2]]
      val <- digest(paste0(row,collapse=""), serialize = FALSE)
      query <- paste0("update record_source set record_source_hash = '",val,"' where record_source_id = ",mat[i,1]," and source like '",source,"'")
      #print(query)
      runInsert(query,toxval.db,verbose=F)
      
      if(i%%1000==0) {
        n <- runQuery(paste0("select count(*) from record_source where record_source_hash='-' and source like '",source,"'"),toxval.db)[1,1]
        cat(i," out of ",nrow(mat),":",n," to do\n")
      }
    }
  }
  
}
