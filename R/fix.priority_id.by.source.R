#-------------------------------------------------------------------------------------
#' Fix the priority_id in the toxval table based on source
#'
#'
#' @param toxval.db The version of toxvaldb to use.
#' @export
#-------------------------------------------------------------------------------------
fix.priority_id.by.source <- function(toxval.db, source) {
  printCurrentFunction(paste(toxval.db,":", source))
  
  file <- "./dictionary/source_priority.xlsx"
  dict <- read.xlsx(file)
  dict1 <- dict[which(dict$source %in% source),]
  
  query <- paste0("update toxval set priority_id=-1 where source like '",source,"'")
  runQuery(query,toxval.db)

  for(i in 1:nrow(dict1)) {
    source <- dict1[i,1]
    pid <- dict1[i,2]
    query <- paste0("update toxval set priority_id=",pid," where source='",source,"'")
    runQuery(query,toxval.db)
  }
  x <- runQuery("select distinct source from toxval where priority_id=-1",toxval.db)
  if(nrow(x)>0) {
    print(x)
    #browser()
  }
  cat("Number of sources with missing priority_id:",nrow(x),"\n")
  # file <- paste0(toxval.config()$datapath,"dictionary/sources_with_missing_priority_id_",source,"_",Sys.Date(),".xlsx")
  # write.xlsx(x,file)

}
