#-----------------------------------------------------------------------------------
#
#' Export the summary genetox data
#'
#' @param toxval.db Database version
#'
#' @return writes and Excel file with the name
#'  ../qc_export/toxval_genetox_summary_[Sys.Date].xlsx
#'
#-----------------------------------------------------------------------------------
export.genetox_summary <- function(toxval.db,dir="./export") {
  printCurrentFunction(toxval.db)

  query <- paste0("SELECT
                  a.casrn,a.name,a.dtxsid,
                  b.reports_pos,b.reports_neg,b.reports_other,b.ames,b.micronucleus,b.genetox_call
                  FROM
                  chemical a
                  INNER JOIN genetox_summary b on a.dtxsid=b.dtxsid
                  ")
  mat <- runQuery(query,toxval.db,T,F)

  file <- paste0(dir,"/toxval_genetox_summary_",Sys.Date(),".xlsx")
  sty <- createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  write.xlsx(mat,file,firstRow=T,headerStyle=sty)
}
