#--------------------------------------------------------------------------------------
#' Get the sources with PFAS data
#' @param db The version of toxval into which the source is loaded.
#' @param infile The input file ./pprtv_ornl/pprtv_ornl_files/new_PPRTV_ORNL cancer noncancer.xlsx
#-----------------------------------------------------------------------------------
pfas.by.source <- function(db) {
  printCurrentFunction(db)
  file = "../pfas/PFAS Universe fixed columns.xlsx"
  mat = read.xlsx(file)

  slist = runQuery("select distinct source from source_chemical",db)
  slist$chemicals = 0
  slist$pfas_chemicals = 0
  slist$percent = 0
  names(slist) = c("source","chemicals","pfas_chemicals","percent")
  for(i in 1:nrow(slist)) {
    source = slist[i,"source"]
    query = paste0("select cleaned_casrn from source_chemical where source='",source,"'")
    clist = runQuery(query,db)[,1]
    slist[i,"chemicals"] = length(unique(clist))
    clist = clist[is.element(clist,mat$casrn)]
    slist[i,"pfas_chemicals"] = length(unique(clist))
    slist[i,"percent"] = 100 * slist[i,"pfas_chemicals"]/slist[i,"chemicals"]
  }
  slist = slist[order(slist$percent,decreasing=T),]
  browser()
  file = "../pfas/PFAS x source.xlsx"
  write.xlsx(slist,file)
}
