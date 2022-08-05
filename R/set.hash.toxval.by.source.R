#-------------------------------------------------------------------------------------
#' Set the hash in table toxval
#' @param toxval.db The version of toxval in which the data is altered.
#' @param do.reset if TRUE, reset all hashes and start from scratch.
#' Otherwise only update empty values
#' @return The database will be altered
#' @export
#--------------------------------------------------------------------------------------
set.hash.toxval.by.source <- function(toxval.db,source, reset=T){
  printCurrentFunction(paste(toxval.db,":", source))
  if(reset) runQuery(paste0("update toxval set toxval_hash='-' where source like '",source,"'"),toxval.db)
  runQuery(paste0("update toxval set toxval_hash='-' where toxval_hash is null and source like '",source,"'"),toxval.db)
  mat = runQuery(paste0("select
                 a.toxval_id,
                 a.dtxsid,
                 a.source,
                 a.subsource,
                 a.year,
                 a.year_original,
                 a.source_url,
                 a.subsource_url,
                 a.toxval_type,
                 a.toxval_type_original,
                 a.toxval_subtype,
                 a.toxval_subtype_original,
                 a.toxval_numeric,
                 a.toxval_numeric_original,
                 a.toxval_numeric_converted,
                 a.toxval_numeric_qualifier,
                 a.toxval_numeric_qualifier_original,
                 a.toxval_units,
                 a.toxval_units_original,
                 a.toxval_units_converted,
                 a.study_duration_value,
                 a.study_duration_value_original,
                 a.study_duration_units,
                 a.study_duration_units_original,
                 a.study_duration_class,
                 a.study_duration_class_original,
                 a.study_type,
                 a.study_type_original,
                 a.exposure_route,
                 a.exposure_route_original,
                 a.exposure_method,
                 a.exposure_method_original,
                 a.exposure_form,
                 a.exposure_form_original,
                 a.media,
                 a.media_original,
                 a.sex,
                 a.sex_original,
                 a.species_original,
                 a.strain,
                 a.strain_original,
                 a.risk_assessment_class,
                 a.human_eco,
                 a.critical_effect,
                 a.generation,
                 a.lifestage,
                 a.details_text,
                 a.population,
                 b.casrn,
                 c.long_ref,
                 c.url,
                 c.document_name,
                 c.record_source_type,
                 c.record_source_note,
                 c.record_source_level,
                 c.title,
                 c.author,
                 c.journal,
                 c.volume,
                 c.year,
                 c.issue,
                 c.page,
                 c.pmid,
                 c.guideline,
                 c.glp,
                 c.quality
                 from toxval a
                 INNER JOIN source_chemical b on a.chemical_id=b.chemical_id
                 LEFT JOIN record_source c on a.toxval_id=c.toxval_id
                 where a.toxval_hash='-' and a.source like '",source,"'"), toxval.db)
  cat("Number of records to fix: ",dim(mat)[1],"\n")
  if(dim(mat)[1]>0) {
    for (i in 1:nrow(mat)){
      row <- mat[i,2:dim(mat)[2]]
      val <- digest(paste0(row,collapse=""), serialize = FALSE)
      query <- paste0("update toxval set toxval_hash = '",val,"' where toxval_id = ",mat[i,1]," and source like '",source,"'")
      runInsert(query,toxval.db)
      if(i%%1000==0) {
        cat(i," out of ",nrow(mat),"\n")
      }
    }
  }
  
}
