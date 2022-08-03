#-------------------------------------------------------------------------------------
#'
#' Check the status of the soruce_chemical tables
#'
#' @param toxval.db The version of toxvaldb to use.
#' @param source.db The vsource database version
#-------------------------------------------------------------------------------------
toxval.check.source_chemical <- function(toxval.db,source.db) {
  printCurrentFunction(toxval.db)

  slist1 = runQuery("select distinct source from toxval",toxval.db)[,1]
  slist2 = runQuery("select distinct source from source_chemical",source.db)[,1]
  slist = unique(sort(c(slist1,slist2)))
  nlist = c("source","n_source","n_source_dtxsid","n_source_dtxrid","n_toxval","n_toxval_dtxsid","n_toxval_dtxrid")
  res = as.data.frame(matrix(nrow=length(slist),ncol=length(nlist)))
  names(res) = nlist
  for(i in 1:length(slist)) {
    source =slist[i]
    n1 = runQuery(paste0("select count(*) from source_chemical where source='",source,"'"),source.db)
    n2 = runQuery(paste0("select count(distinct dtxsid) from source_chemical where source='",source,"'"),source.db)
    n3 = runQuery(paste0("select count(distinct dtxrid) from source_chemical where source='",source,"'"),source.db)
    n4 = runQuery(paste0("select count(*) from source_chemical where source='",source,"'"),toxval.db)
    n5 = runQuery(paste0("select count(distinct dtxsid) from source_chemical where source='",source,"'"),toxval.db)
    n6 = runQuery(paste0("select count(distinct dtxrid) from source_chemical where source='",source,"'"),toxval.db)
    res[i,"source"] = source
    res[i,"n_source"] = n1
    res[i,"n_source_dtxsid"] = n2
    res[i,"n_source_dtxrid"] = n3
    res[i,"n_toxval"] = n4
    res[i,"n_toxval_dtxsid"] = n5
    res[i,"n_toxval_dtxrid"] = n6
    #if(source=="ECHA eChemPortal") browser()
  }
  file = paste0(toxval.config()$datapath,"/export/toxval.check.source_chemical_",Sys.Date(),".xlsx")
  write.xlsx(res,file)
}
