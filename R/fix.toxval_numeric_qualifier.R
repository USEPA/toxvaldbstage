#-------------------------------------------------------------------------------------
#' Fix the toxval_numeric_qualifier
#' @param toxval.db The version of toxval in which the data is altered.
#' @return The database will be altered
#' @export
#--------------------------------------------------------------------------------------
fix.toxval_numeric_qualifier = function(toxval.db){
  printCurrentFunction(toxval.db)
  runQuery("update toxval set toxval_numeric_qualifier='~' where  toxval_numeric_qualifier='ca' and source not like 'ECOTOX'",toxval.db)
  runQuery("update toxval set toxval_numeric_qualifier='~' where  toxval_numeric_qualifier='ca.' and source not like 'ECOTOX'",toxval.db)
  runQuery("update toxval set toxval_numeric_qualifier='~' where  toxval_numeric_qualifier='circa' and source not like 'ECOTOX'",toxval.db)
  runQuery("update toxval set toxval_numeric_qualifier='=' where  toxval_numeric_qualifier='' and source not like 'ECOTOX'",toxval.db)
  runQuery("update toxval set toxval_numeric_qualifier='=' where  toxval_numeric_qualifier='-' and source not like 'ECOTOX'",toxval.db)
}
