#-------------------------------------------------------------------------------------
#' Load the Genetox data from Grace
#' @param toxval.db The database to use.
#' @param verbose if TRUE output debug information
#--------------------------------------------------------------------------------------
toxval.load.genetox_details <- function(toxval.db, verbose=F) {
  printCurrentFunction(toxval.db)

  file <- "./genetox/genetox_alldata_for_load_2019-02-28.xlsx"
  res <- read.xlsx(file)
  name.list <- c(
    "casrn","preferred_name","dsstox_substance_id","dsstox_compound_id","smiles_2d_qsar",
    "aggregate_study_type",
    "assay_category","assay_code","assay_type","assay_type_standard","assay_type_simple_aggregate",
    "dose_response","duration","metabolic_activation",
    "species","species_strain","strain","sex",
    "panel_report",
    "assay_outcome","assay_potency","assay_result","assay_result_std","genetox_results","genetox_note",
    "comment","cytotoxicity","data_quality",
    "document_number","document_source","reference","reference_url","source","title","year","protocol_era"
  )

  res <- res[,name.list]
  res <- res[!is.na(res[,"casrn"]),]
  cas.list = res[,1:2]
  cid.list = get.cid.list.toxval(toxval.db, cas.list,"genetox_details")
  res$chemical_id <- cid.list$chemical_id
  #res <- merge(res,cid.list)
  res <- res[,!is.element(names(res),c("casrn","name"))]

  name.list <- names(res)
  exclude.list <- c("preferred_name","dsstox_substance_id","dsstox_compound_id")
  name.list <- name.list[!is.element(name.list,exclude.list)]
  res <- res[,name.list]
  res <- unique(res)
  #runQuery("delete from genetox_details",db)
  for(i in 1:nrow(res)) res[i,"genetox_details_uuid"] <- UUIDgenerate()
  runInsertTable(res, "genetox_details", toxval.db,verbose)
}
