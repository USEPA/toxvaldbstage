#-------------------------------------------------------------------------------------
#' Fix the qa_status flag
#' @param toxval.db The version of toxval in which the data is altered.
#' @return The database will be altered
#--------------------------------------------------------------------------------------
fix.qa_status = function(toxval.db,reset=T){
  printCurrentFunction(toxval.db)

  cat("  set all to 'not classified'\n")
  if(reset) runQuery("update toxval set qa_status=1",toxval.db)

  runQuery("update toxval set qa_status=0 where toxval_numeric<=0",toxval.db)
  runQuery("update toxval set qa_status=0 where toxval_type='-'",toxval.db)
  runQuery("update toxval set qa_status=0 where toxval_units='-'",toxval.db)
  runQuery("update toxval set qa_status=0 where risk_assessment_class='other'",toxval.db)
}
