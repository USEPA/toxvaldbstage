#-------------------------------------------------------------------------------------
#'
#' use name and dtxsid pulled from ECOTOX in source_chemical for ecotox 
#'
#' @param toxval.db The version of toxvaldb to use.
#' @param verbose If TRUE, print out extra diagnostic messages
#-------------------------------------------------------------------------------------
map.chemical.to.dsstox.ecotox <- function(toxval.db,source,verbose=T) {
  printCurrentFunction(paste(toxval.db,":", source))
  
  runQuery(paste0("update source_chemical set name=source_name where name='-' and source like '",source,"'"),toxval.db)
  runQuery(paste0("update source_chemical set casrn=source_casrn where casrn='-' and source like '",source,"'"),toxval.db)
  
  
}
