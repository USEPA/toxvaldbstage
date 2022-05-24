#-----------------------------------------------------------------------------------
#
#' Export the toxval_dictionary table
#'
#' @param toxval.db Database version
#'
#' @return writes and Excel file with the name
#'  ../qc_export/toxval_dictionary_[Sys.Date].xlsx
#'
#-----------------------------------------------------------------------------------
export.toxval_dictionary <- function(toxval.db,dir="./export") {
  printCurrentFunction(toxval.db)
  query <- paste0("SELECT * from toxval_dictionary")
  mat <- runQuery(query,toxval.db,T,F)
  file <- paste0(dir,"/toxval_dictionary_",Sys.Date(),".xlsx")
  sty <- createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  write.xlsx(mat,file,firstRow=T,headerStyle=sty)
}
