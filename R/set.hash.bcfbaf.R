#-------------------------------------------------------------------------------------
#' Set the hash in table bcfbaf
#' @param toxval.db The version of toxval in which the data is altered.
#' @param do.reset if TRUE, reset all hashes and start rom scratch.
#' Otherwise only update empty values
#' @return The database will be altered
#' @export
#--------------------------------------------------------------------------------------
set.hash.bcfbaf = function(toxval.db,do.reset=T){
  printCurrentFunction(toxval.db)
  if(do.reset) runQuery("update bcfbaf set bcfbaf_hash='-'",toxval.db)
  runQuery("update bcfbaf set bcfbaf_hash='-' where bcfbaf_hash is null",toxval.db)
  mat = runQuery("select
                 bcfbaf_id,
                 value_type,
                 units,
                 species_supercategory,
                 species_scientific,
                 species_common,
                 author,
                 title,
                 journal,
                 tissue,
                 calc_method,
                 comments,
                 logkow_reference,
                 radiolabel,
                 exposure_duration,
                 exposure_type,
                 exposure_route,
                 media,
                 total_organic_carbon
                 from bcfbaf
                 where bcfbaf_hash='-'", toxval.db)
  cat("Number of records to fix: ",dim(mat)[1],"\n")
  if(dim(mat)[1]>0) {
    for (i in 1:nrow(mat)){
      row <- mat[i,2:dim(mat)[2]]
      val <- digest(paste0(row,collapse=""), serialize = FALSE)
      query <- paste0("update bcfbaf set bcfbaf_hash = '",val,"' where bcfbaf_id = ",mat[i,1])
      runInsert(query,toxval.db)
      if(i%%1000==0) {
        cat(i," out of ",nrow(mat),"\n")
      }
    }
  }
}
