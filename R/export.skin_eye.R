#-----------------------------------------------------------------------------------
#
#' Export the skin and eye data
#'
#' @param toxval.db Database version
#'
#' @return writes and Excel file with the name
#'  ../qc_export/toxval_skin_eye_[Sys.Date].xlsx
#'
#-----------------------------------------------------------------------------------
export.skin_eye <- function(toxval.db,dir="./export") {
  printCurrentFunction(toxval.db)

  query <- paste0("SELECT
                  a.casrn,a.name,a.dtxsid,
                  b.source,b.authority,b.study_type,
                  b.endpoint,b.classification,b.score,b.result_text,
                  b.species,b.strain,b.guideline,b.glp,b.reliability, b.year,b.record_url
                  FROM
                  chemical a
                  INNER JOIN skin_eye b on a.dtxsid=b.dtxsid
                  ")
  mat <- runQuery(query,toxval.db,T,F)

  file <- paste0(dir,"/toxval_skin_eye_",Sys.Date(),".xlsx")
  sty <- createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  write.xlsx(mat,file,firstRow=T,headerStyle=sty)
}
