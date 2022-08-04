#-----------------------------------------------------------------------------------
#
#' Export the detailed genetox data
#'
#' @param toxval.db Database version
#'
#' @return writes and Excel file with the name
#'  ../qc_export/toxval_genetox_details_[Sys.Date].xlsx
#'
#-----------------------------------------------------------------------------------
export.genetox_details <- function(toxval.db,dir="./export") {
  printCurrentFunction(toxval.db)

  query <- paste0("SELECT
                  a.casrn,a.name,a.dtxsid,
                  b.smiles_2d_qsar,
                  b.aggregate_study_type,
                  b.assay_category,b.assay_code,b.assay_type,b.assay_type_standard,b.assay_type_simple_aggregate,
                  b.dose_response,b.duration,b.metabolic_activation,
                  b.species,b.species_strain,b.strain,b.sex,
                  b.panel_report,
                  b.assay_outcome,b.assay_potency,b.assay_result,b.assay_result_std,b.genetox_results,b.genetox_note,
                  b.comment,b.cytotoxicity,b.data_quality,
                  b.document_number,b.document_source,b.reference,b.reference_url,b.source,b.title,b.year,b.protocol_era
                  FROM
                  chemical a
                  INNER JOIN genetox_details b on a.dtxsid=b.dtxsid
                  ")
  mat <- runQuery(query,toxval.db,T,F)

  file <- paste0(dir,"/toxval_genetox_details_",Sys.Date(),".xlsx")
  sty <- createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  write.xlsx(mat,file,firstRow=T,headerStyle=sty)
}
