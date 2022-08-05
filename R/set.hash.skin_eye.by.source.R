#-------------------------------------------------------------------------------------
#' Set the hash in table skin_eye
#' @param toxval.db The version of toxval in which the data is altered.
#' @param do.reset if TRUE, reset all hashes and start rom scratch.
#' Otherwise only update empty values
#' @return The database will be altered
#' @export
#--------------------------------------------------------------------------------------
set.hash.skin_eye.by.source <- function(toxval.db,source,do.reset=T){
  printCurrentFunction(paste(toxval.db,":", source))
  if(do.reset) runQuery(paste0("update skin_eye set skin_eye_hash='-' where source like '",source,"'") ,toxval.db)
  runQuery(paste0("update skin_eye set skin_eye_hash='-' where skin_eye_hash is null and source like '",source,"'") ,toxval.db)
  
  mat = runQuery(paste0("select
                  a.skin_eye_id,
                  a.dtxsid,
                  a.source,
                  a.study_type,
                  a.species,
                  a.strain,
                  a.reliability,
                  a.endpoint,
                  a.guideline,
                  a.result_text,
                  a.classification,
                  a.score,
                  a.year,
                  a.record_url,
                  a.glp,
                  a.authority,
                  b.casrn
                 from skin_eye a
                 INNER JOIN source_chemical b on a.chemical_id=b.chemical_id
                 where a.skin_eye_hash='-' and a.source like '",source,"'"), toxval.db)
  cat("Number of records to fix: ",dim(mat)[1],"\n")
  if(dim(mat)[1]>0) {
    for (i in 1:nrow(mat)){
      row <- mat[i,2:dim(mat)[2]]
      val = digest(paste0(row,collapse=""), serialize = FALSE)
      query = paste0("update skin_eye set skin_eye_hash = '",val,"' where skin_eye_id = ",mat[i,1]," and source like '",source,"'")
      runInsert(query,toxval.db,do.halt=T)
      if(i%%1000==0) {
        cat(i," out of ",nrow(mat),"\n")
      }
    }
  }
}
