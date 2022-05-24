#-------------------------------------------------------------------------------------
#' Set the hash in table genetox_details
#' @param toxval.db The version of toxval in which the data is altered.
#' @param do.reset if TRUE, reset all hashes and start rom scratch.
#' Otherwise only update empty values
#' @return The database will be altered
#' @export
#--------------------------------------------------------------------------------------
set.hash.genetox_details = function(toxval.db,do.reset=T){
  printCurrentFunction(toxval.db)
  if(do.reset) runQuery("update genetox_details set genetox_details_hash='-'",toxval.db)
  runQuery("update genetox_details set genetox_details_hash='-' where genetox_details_hash is null",toxval.db)

  mat = runQuery("select
                 a.genetox_details_id,
                 a.dtxsid,
                 a.smiles_2d_qsar,
                 a.aggregate_study_type,
                 a.assay_category,
                 a.assay_code,
                 a.assay_type,
                 a.assay_type_standard,
                 a.assay_type_simple_aggregate,
                 a.dose_response,
                 a.duration,
                 a.metabolic_activation,
                 a.species,
                 a.species_strain,
                 a.strain,
                 a.sex,
                 a.panel_report,
                 a.assay_outcome,
                 a.assay_potency,
                 a.assay_result,
                 a.assay_result_std,
                 a.genetox_results,
                 a.genetox_note,
                 a.comment,
                 a.cytotoxicity,
                 a.data_quality,
                 a.document_number,
                 a.document_source,
                 a.reference,
                 a.reference_url,
                 a.source,
                 a.title,
                 a.year,
                 a.protocol_era,
                 b.casrn
                 from genetox_details a
                 INNER JOIN source_chemical b on a.chemical_id=b.chemical_id
                 where a.genetox_details_hash='-'", toxval.db)
  cat("Number of records to fix: ",dim(mat)[1],"\n")
  if(dim(mat)[1]>0) {
    for (i in 1:nrow(mat)){
      row <- mat[i,2:dim(mat)[2]]
      val = digest(paste0(row,collapse=""), serialize = FALSE)
      query = paste0("update genetox_details set genetox_details_hash = '",val,"' where genetox_details_id = ",mat[i,1])
      runInsert(query,toxval.db,do.halt=T)
      if(i%%1000==0) {
        cat(i," out of ",nrow(mat),"\n")
      }
    }
  }
}
