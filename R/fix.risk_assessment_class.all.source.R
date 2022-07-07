#-------------------------------------------------------------------------------------
#' Fix the risk assessment class for all source.
#'
#' @param toxval.db The version of toxval in which the data is altered.
#' @param restart If TRUE, delete all values and start from scratch
#' @export
#--------------------------------------------------------------------------------------
fix.risk_assessment_class.all.source <- function(toxval.db,restart=T,add.rules=T) {
  printCurrentFunction(toxval.db)
  slist = runQuery("select distinct source from toxval",db) [,1]
  for(source in slist) {
    fix.risk_assessment_class.by.source(toxval.db,source, restart,add.rules)
  }
}
