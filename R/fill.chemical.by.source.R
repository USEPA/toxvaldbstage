#-------------------------------------------------------------------------------------
#'
#' Fill the chemical table
#'
#' @param toxval.db The version of toxvaldb to use.
#' @param verbose If TRUE, print out extra diagnostic messages
#-------------------------------------------------------------------------------------
fill.chemical.by.source <- function(toxval.db, source, verbose=T) {
  printCurrentFunction(paste(toxval.db,":", source))
  
  runQuery(paste0("delete from chemical where dtxsid in (select dtxsid from source_chemical where source like '",source,"')"),toxval.db)
  res <- runQuery(paste0("select dtxsid,casrn,name from source_chemical where source like '",source,"'") ,toxval.db)
  res <- res[!is.element(res$dtxsid,"-"),]
  cat("Rows in source_chemical:",nrow(res),"\n")
  res <- unique(res)
  cat("Unique rows in source_chemical:",nrow(res),"\n")
  x <- unique(res$dtxsid)
  cat("Unique dtxsid values:",length(x),"\n")
  res <- res[order(res$dtxsid),]
  res2 <- res
  res2$duplicate <- 0
  for(i in 2:nrow(res2)) {
    if(res2[i,"dtxsid"]==res2[i-1,"dtxsid"]) {
      res2[i,"duplicate"] <- 1
    }
  }
  res2 <- res2[res2$duplicate==0,]
  cat("Unique rows in res2:",nrow(res2),"\n")
  res2 <- res2[,c("dtxsid","casrn","name")]
  runInsertTable(res2, "chemical", toxval.db, verbose)
}
