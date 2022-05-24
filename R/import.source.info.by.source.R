#--------------------------------------------------------------------------------------
#' Load Source Info for each source into toxval 
#' The information is in the file ./dictionary/source_in_2020_aug_17.xlsx
#' @param toxval.db The version of toxval into which the source info is loaded.
#' @export
#--------------------------------------------------------------------------------------
import.source.info.by.source <- function(toxval.db, source) {
  printCurrentFunction(paste(toxval.db,":", source))
  
  runInsert(paste0("delete from source_info where source='",source,"'"),toxval.db)
  
  
  mat <- read.xlsx('./dictionary/source_in_2020_aug_17.xlsx')
  mat1 <- mat[which(mat$source %in% source),]

  runInsertTable(mat1,"source_info",toxval.db,do.halt=T,verbose=T)
}
