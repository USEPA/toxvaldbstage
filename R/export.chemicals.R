#-----------------------------------------------------------------------------------
#
#' Export all chemicals in the chemical and chemical_list tables
#'
#' @param toxval.db Database version
#'
#' @return writes and Excel file with the name
#'  ../export/toxval_chemicals_[Sys.Date].xlsx
#'
#-----------------------------------------------------------------------------------
export.chemicals <- function(toxval.db) {
  printCurrentFunction(toxval.db)

  query <- paste0("SELECT dtxsid,casrn,name from chemical_list")
  mat <- runQuery(query,toxval.db,T,F)
  print(nrow(mat))

  file <- paste0("./export/toxval_chemicals_",Sys.Date(),".xlsx")
  sty <- createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  write.xlsx(mat,file,firstRow=T,headerStyle=sty)
}
