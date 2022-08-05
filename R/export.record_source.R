#-----------------------------------------------------------------------------------
#
#' Build a data frame of the th data from record_source and export
#'
#' @param toxval.db Database version
##'
#'
#' @return writes an Excel file with the name
#'  ../export/toxval_record_summary_[Sys.Date].xlsx
#'
#-----------------------------------------------------------------------------------
export.record_source <- function(toxval.db,dir="./export") {
  printCurrentFunction(toxval.db)

  query <- paste0("SELECT * from record_source")

  mat <- runQuery(query,toxval.db,T,F)
  mat <- unique(mat)
  print(dim(mat))

  file <- paste0(dir,"/toxval_record_source_",Sys.Date(),".xlsx")
  sty <- createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  write.xlsx(mat,file,firstRow=T,headerStyle=sty)

}
