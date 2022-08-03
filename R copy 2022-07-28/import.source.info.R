
#--------------------------------------------------------------------------------------
#' Load Source Info into toxval. 
#' The information is in the file ./dictionary/source_in_2020_aug_17.xlsx
#' @param toxval.db The version of toxval into which the source info is loaded.
#' @export
#--------------------------------------------------------------------------------------
import.source.info <- function(toxval.db) {
  printCurrentFunction(toxval.db)
  runInsert("delete from source_info",toxval.db)

  
  mat <- read.xlsx('./dictionary/source_in_2020_aug_17.xlsx')
  mat2 <- unique(mat[,c("source","subsource")])

  res <- runQuery("select distinct source,subsource from toxval",toxval.db)
  res <- unique(res)
  for(i in 1:dim(res)[1]) {
    source <- res[i,1]
    subsource <- res[i,2]
    temp1 <- mat[is.element(mat[,"source"],source),]
    temp2 <- temp1[is.element(temp1[,"subsource"],subsource),]
    #cat(source,subsource,dim(temp1)[1],dim(temp2)[2],"\n")
    if(dim(temp1)[1]==0 || dim(temp2)[1]==0)
      cat(source,"\t",subsource,"\n")

  }
  runInsertTable(mat,"source_info",toxval.db,do.halt=T,verbose=T)
}
