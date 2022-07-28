#--------------------------------------------------------------------------------------
#' Load Source Info for each source into toxval
#' The information is in the file ~/dictionary/source_in_2020_aug_17.xlsx
#' @param toxval.db The version of toxval into which the source info is loaded.
#' @param source The specific source to be loaded, If NULL, load for all sources
#' @export
#--------------------------------------------------------------------------------------
import.source.info.by.source <- function(toxval.db, source=NULL) {
  printCurrentFunction(toxval.db)
  file = paste0(toxval.config()$datapath,"dictionary/source_info 2022-07-27.xlsx")
  print(file)
  mat =openxlsx::read.xlsx(file)
  slist = source
  if(is.null(source)) slist = runQuery("select distinct source from toxval",db)[,1]
  for(source in slist) {
    cat(source,"\n")
    runInsert(paste0("delete from source_info where source='",source,"'"),toxval.db)
    mat1 = mat[which(mat$source %in% source),]
    runInsertTable(mat1,"source_info",toxval.db,do.halt=T,verbose=T)
  }
}
