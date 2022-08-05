#-------------------------------------------------------------------------------------
#' Fix the priority_id in the toxval table based on source
#'
#'
#' @param toxval.db The version of toxvaldb to use.
#' @export
#-------------------------------------------------------------------------------------
fix.priority_id <- function(toxval.db) {
  printCurrentFunction(toxval.db)
  file <- "./dictionary/source_priority.xlsx"
  dict <- read.xlsx(file)
  runQuery("update toxval set priority_id=-1",toxval.db)
  for(i in 1:nrow(dict)) {
    source <- dict[i,1]
    pid <- dict[i,2]
    query <- paste0("update toxval set priority_id=",pid," where source='",source,"'")
    runQuery(query,toxval.db)
  }
  x <- runQuery("select distinct source from toxval where priority_id=-1",toxval.db)
  if(nrow(x)>0) {
    print(x)
    #browser()
  }
}
