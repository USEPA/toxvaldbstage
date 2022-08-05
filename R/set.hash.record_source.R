#-------------------------------------------------------------------------------------
#' Set the hash in table record_source
#' @param toxval.db The version of toxval in which the data is altered.
#' @param do.reset if TRUE, reset all hashes and start from scratch.
#' Otherwise only update empty values
#' @return The database will be altered
#' @export
#--------------------------------------------------------------------------------------
set.hash.record_source <- function(toxval.db,do.reset=T){
  printCurrentFunction(toxval.db)
  runQuery("update record_source set record_source_hash='-' where record_source_hash is null",toxval.db)
  if(do.reset) runQuery("update record_source set record_source_hash='-'",toxval.db)

  mat <- runQuery("select
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
                  where record_source_hash='-'", toxval.db)
  cat("Number of records to fix: ",dim(mat)[1],"\n")
  if(dim(mat)[1]>0) {
    for (i in 1:nrow(mat)){
      row <- mat[i,2:dim(mat)[2]]
      val <- digest(paste0(row,collapse=""), serialize = FALSE)
      query <- paste0("update record_source set record_source_hash = '",val,"' where record_source_id = ",mat[i,1])
      #print(query)
      runInsert(query,toxval.db,verbose=F)

      if(i%%1000==0) {
        n <- runQuery("select count(*) from record_source where record_source_hash='-'",toxval.db)[1,1]
        cat(i," out of ",nrow(mat),":",n," to do\n")
      }
    }
  }

}
