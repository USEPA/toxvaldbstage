#-------------------------------------------------------------------------------------
#'
#' Fill the chemical table
#'
#' @param toxval.db The version of toxvaldb to use.
#' @param verbose If TRUE, print out extra diagnostic messages
#-------------------------------------------------------------------------------------
fill.chemical <- function(toxval.db,verbose=T) {
  printCurrentFunction()
  runQuery("delete from chemical",toxval.db)
  res <- runQuery("select dtxsid,casrn,name from source_chemical",toxval.db)
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
