#-----------------------------------------------------------------------------------
#
#' Export ECHA data from all four ECHA sources and look for overlaps
#'
#' @param toxval.db Database version
#'
#' @return writes and Excel file with the name
#'  ./export/toxval_echa_repeats_[Sys.Date].xlsx
#'
#-----------------------------------------------------------------------------------
export.echa.repeats <- function(toxval.db) {
  printCurrentFunction(toxval.db)
  
  query <- paste0("select * from toxval where source like '%ECHA%'")
  mat <- runQuery(query,toxval.db,T,F)
  print(nrow(mat))
  
  file <- paste0("./export/toxval_echa_repeats_",Sys.Date(),".xlsx")
  sty <- createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  write.xlsx(mat,file,firstRow=T,headerStyle=sty)
}
