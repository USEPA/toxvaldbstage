#-------------------------------------------------------------------------------------
#' Fix the qa_status flag
#' @param toxval.db The version of toxval in which the data is altered.
#' @return The database will be altered
#--------------------------------------------------------------------------------------
fix.qa_status.by.source <- function(toxval.db,source, reset=T){
  printCurrentFunction(paste(toxval.db,":", source))

  cat("  set all to 'not classified'\n")
  if(reset) runQuery(paste0("update toxval set qa_status=1 where source like '",source,"'") ,toxval.db)

  runQuery(paste0("update toxval set qa_status=0 where toxval_numeric<=0 and source like '",source,"'") ,toxval.db)
  runQuery(paste0("update toxval set qa_status=0 where toxval_type='-' and source like '",source,"'") ,toxval.db)
  runQuery(paste0("update toxval set qa_status=0 where toxval_units='-' and source like '",source,"'"),toxval.db)
  runQuery(paste0("update toxval set qa_status=0 where risk_assessment_class='other' and source like '",source,"'"),toxval.db)
}
