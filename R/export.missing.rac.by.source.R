#-----------------------------------------------------------------------------------
#
#' Export the rows with a missing risk_assessment_class
#'
#' @param toxval.db Database version
#' @param source The source to be processed
#'
#' @return writes an Excel file with the name
#'  ./qc_export/toxval_missing_risk_assessment_class_Sys.Date().xlsx"
#'
#-----------------------------------------------------------------------------------
export.missing.rac.by.source <- function(toxval.db, source) {
  printCurrentFunction(paste(toxval.db,":", source))

  query <- paste0("SELECT
                  b.toxval_id,b.source_hash,b.source_table,
                  a.dtxsid,a.casrn,a.name,b.chemical_id,
                  b.source,b.subsource,
                  b.source_url,b.subsource_url,
                  b.qc_status,
                  b.details_text,
                  b.priority_id,
                  b.risk_assessment_class,
                  b.human_eco,
                  b.toxval_type,b.toxval_type_original,
                  b.toxval_subtype,
                  e.toxval_type_supercategory,
                  e.toxval_type_category,
                  b.toxval_numeric,b.toxval_units,
                  b.toxval_numeric_original,b.toxval_units_original,
                  b.toxval_numeric_standard,b.toxval_units_standard,
                  b.toxval_numeric_human,b.toxval_units_human,
                  b.study_type,b.study_type_original,
                  b.study_duration_class,b.study_duration_class_original,
                  b.study_duration_value,b.study_duration_value_original,
                  b.study_duration_units,b.study_duration_units_original,
                  b.species_id,b.species_original,d.common_name,d.latin_name,d.ecotox_group,d.habitat,
                  b.strain,b.strain_group,b.strain_original,
                  b.sex,b.sex_original,
                  b.generation,b.lifestage,
                  b.exposure_route,b.exposure_route_original,
                  b.exposure_method,b.exposure_method_original,
                  b.exposure_form,b.exposure_form_original,
                  b.media,b.media_original,
                  b.critical_effect,
                  b.critical_effect_original,
                  b.year,
                  b.datestamp,
                  f.long_ref,
                  f.title,
                  f.author,
                  f.journal,
                  f.volume,
                  f.year,
                  f.issue,
                  f.url,
                  f.document_name,
                  b.toxval_uuid,
                  b.toxval_hash
                  FROM
                  toxval b
                  INNER JOIN chemical a on a.dtxsid=b.dtxsid
                  LEFT JOIN species d on b.species_id=d.species_id
                  INNER JOIN toxval_type_dictionary e on b.toxval_type=e.toxval_type
                  INNER JOIN record_source f on b.toxval_id=f.toxval_id
                  WHERE
                  b.source='",source,"'")

  mat <- runQuery(query,toxval.db,T,F)
  mat <- unique(mat)
  print(dim(mat))
  nrow <- dim(mat)[1]
  file <- paste0(toxval.config()$datapath,"qc_export/toxvaldb_missing_risk_assessment_class_for_",source,"_",Sys.Date(),".xlsx")
  sty <- createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  write.xlsx(mat,file,firstRow=T,headerStyle=sty)
}
