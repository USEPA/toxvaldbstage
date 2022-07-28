#--------------------------------------------------------------------------------------
#' Load the chemical_source_index table.
#'
#' @param db The version of toxval_source into which the source is loaded.
#'
#--------------------------------------------------------------------------------------
fill.chemical_source_index <- function(db) {
  printCurrentFunction(db)
  file = paste0(toxval.config()$datapath,"chemical_mapping/chemical_source_index.xlsx")
  mat = openxlsx::read.xlsx(file)
  runQuery("delete from chemical_source_index",db)
  runInsertTable(mat,"chemical_source_index",db,do.halt=T,verbose=F)
}
