#-----------------------------------------------------------------------------------
#
#' Export the rows with a missing risk_assessment_class
#'
#' @param toxval.db Database version
#'
#' @return writes an Excel file with the name
#'  ./qc_export/toxval_missing_risk_assessment_class_Sys.Date().xlsx"
#'
#-----------------------------------------------------------------------------------
export.missing.rac.by.source <- function(toxval.db, source) {
  printCurrentFunction(paste(toxval.db,":", source))
  
  query <- paste0("SELECT
                  a.casrn,a.name,b.toxval_id,b.toxval_uuid,a.dtxsid,
                  b.source,b.subsource,
                  b.toxval_type,b.toxval_type_original,
                  b.toxval_subtype,b.toxval_subtype_original,
                  b.toxval_numeric_qualifier,
                  b.toxval_numeric_qualifier_original,
                  b.toxval_numeric,b.toxval_numeric_original,
                  b.toxval_units,b.toxval_units_original,
                  b.risk_assessment_class,
                  b.study_type,b.study_type_original,
                  b.study_duration_class,b.study_duration_class_original,
                  b.study_duration_value,b.study_duration_value_original,
                  b.study_duration_units,b.study_duration_units_original,
                  b.human_eco,
                  b.strain,b.sex,
                  b.exposure_route,b.exposure_route_original,
                  b.exposure_method,b.exposure_method_original,
                  b.critical_effect,
                  b.year,b.quality_id,b.priority_id,
                  b.source_url, b.datestamp
                  FROM
                  source_chemical a
                  INNER JOIN toxval b on a.chemical_id=b.chemical_id
                  WHERE
                  b.risk_assessment_class='-'
                  and b.toxval_numeric>0 and b.source like '",source,"'")
  
  mat <- runQuery(query,toxval.db,T,F)
  mat <- unique(mat)
  print(dim(mat))
  nrow <- dim(mat)[1]
  # file <- paste0("./qc_export/toxvaldb_missing_risk_assessment_class_for_",source,"_",Sys.Date(),".xlsx")
  # sty <- createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  # write.xlsx(mat,file,firstRow=T,headerStyle=sty)
}
