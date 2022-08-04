#-------------------------------------------------------------------------------------
#' Reset the numeric and units values to their original values
#' @param toxval.db The version of toxval in which the data is altered.
#' @return The database will be altered
#' @export
#--------------------------------------------------------------------------------------
reset.numeric <- function(toxval.db) {
  printCurrentFunction(toxval.db)
  query = "update toxval
  set toxval_numeric = toxval_numeric_original"
  runInsert(query,toxval.db,T,F,T)
}
