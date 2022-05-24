#-------------------------------------------------------------------------------------
#' Fix the toxval_numeric_qualifier by source
#' @param toxval.db The version of toxval in which the data is altered.
#' @return The database will be altered
#' @export
#--------------------------------------------------------------------------------------
fix.toxval_numeric_qualifier.by.source <- function(toxval.db, source){
  printCurrentFunction(paste(toxval.db,":", source))
  
  
  runQuery(paste0("update toxval set toxval_numeric_qualifier='~' where  toxval_numeric_qualifier='ca' and source like '",source,"'"),toxval.db)
  runQuery(paste0("update toxval set toxval_numeric_qualifier='~' where  toxval_numeric_qualifier='ca.' and source like '",source,"'"),toxval.db)
  runQuery(paste0("update toxval set toxval_numeric_qualifier='~' where  toxval_numeric_qualifier='circa' and source like '",source,"'"),toxval.db)
  runQuery(paste0("update toxval set toxval_numeric_qualifier='=' where  toxval_numeric_qualifier='' and source like '",source,"'"),toxval.db)
  runQuery(paste0("update toxval set toxval_numeric_qualifier='=' where  toxval_numeric_qualifier='-' and source like '",source,"'"),toxval.db)
}