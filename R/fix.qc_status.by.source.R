#-------------------------------------------------------------------------------------
#' Fix the qa_status flag
#' @param toxval.db The version of toxval in which the data is altered.
#' @return The database will be altered
#' @export
#--------------------------------------------------------------------------------------
fix.qc_status.by.source <- function(toxval.db,source, reset=T){
  printCurrentFunction(paste(toxval.db,":", source))

  cat("  set all to 'not classified'\n")
  if(reset) runQuery(paste0("update toxval set qc_status='pass' where source like '",source,"'") ,toxval.db)

  runQuery(paste0("update toxval set qc_status='fail:toxval_numeric<0' where toxval_numeric<=0 and source = '",source,"'") ,toxval.db)
  runQuery(paste0("update toxval set qc_status='fail:toxval_type not specified' where toxval_type='-' and source = '",source,"'") ,toxval.db)
  runQuery(paste0("update toxval set qc_status='fail:toxval_units not specified' where toxval_units='-' and source = '",source,"'"),toxval.db)
  runQuery(paste0("update toxval set qc_status='fail:risk_assessment_class not specified' where risk_assessment_class='other' and source = '",source,"'"),toxval.db)
  runQuery(paste0("update toxval set qc_status='fail:dtxsid not specified' where dtxsid='NODTXSID' and source = '",source,"'"),toxval.db)
}
