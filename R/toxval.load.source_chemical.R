#-------------------------------------------------------------------------------------
#'
#' Perform the DSSTox mapping
#'
#' @param toxval.db The version of toxvaldb to use.
#' @param source The source to update for
#' @param verbose If TRUE, print out extra diagnostic messages
#-------------------------------------------------------------------------------------
toxval.load.source_chemical <- function(toxval.db,source.db,source=NULL,verbose=T) {
  printCurrentFunction(paste(toxval.db,":", source))

  if(!exists("DSSTOX")) {
    sys.date <- "2022-07-20"
    file <- paste0("../DSSTox/DSSTox_",sys.date,".RData")
    load(file)
    dsstox = DSSTOX
    rownames(dsstox) = dsstox$casrn
    names(dsstox)[is.element(names(dsstox),"dsstox_substance_id")] <- "dtxsid"
    DSSTOX <<- dsstox
  }
  dsstox = DSSTOX
  slist = runQuery("select distinct source from toxval",toxval.db)[,1]
  slist = slist[!is.element(slist,c("ECOTOX","ToRefDB"))]
  slist = sort(slist)
  if(!is.null(source)) slist = source
  for(source in slist) {
    cat("------------------------------------------------------\n")
    cat(source,"\n")
    cat("------------------------------------------------------\n")
    cat("pull data from the source database\n")
    runQuery(paste0("delete from source_chemical where source='",source,"'"),toxval.db)
    chems = runQuery(paste0("select * from source_chemical where source='",source,"'"),source.db)
    runInsertTable(chems, "source_chemical", toxval.db)
    count = runQuery(paste0("select count(*) from source_chemical where dtxsid is null and source='",source,"'"),source.db)[1,1]
    if(count>0) {
      cat("set dtxsid, name from missing chemicals",source,":",count,"\n")
      chems = runQuery(paste0("select distinct chemical_id,cleaned_casrn,cleaned_name as casrn from source_chemical where dtxsid is null and source='",source,"'"),toxval.db)
      names(chems) = c("chemical_id","casrn","name")
      clist = chems$casrn
      clist = clist[is.element(clist,dsstox$casrn)]
      if(length(clist)>0) {
        dtemp = dsstox[clist,]
        for(i in 1:nrow(chems)) {
          casrn = chems[i,"casrn"]
          name = chems[i,"name"]
          cid = chems[i,"chemical_id"]
          dtxsid = "NODTXSID"
          if(is.element(casrn,clist)) {
            name = dtemp[casrn,"preferred_name"]
            dtxsid = dtemp[casrn,"dtxsid"]
          }
          query = paste0("update source_chemical set casrn='",casrn,"', name='",name,"', dtxsid='",dtxsid,"' where chemical_id='",chemical_id,"' and source='",source,"'")
          runQuery(query,toxval.db)
          if(verbose) if(i%%500==0) cat("  chemicals updated:",i," out of ",nrow(chems),"\n")
        }
      }
    }
    cat("set dtxsid in the toxval table\n")
    runQuery(paste0("update source_chemical set dtxsid='NODTXSID' where dtxsid='-' and source='",source,"'"),toxval.db)
    runQuery(paste0("update toxval set dtxsid='NODTXSID' where source='",source,"'"),toxval.db)
    chems = runQuery(paste0("select chemical_id,dtxsid from source_chemical where source='",source,"'"),toxval.db)
    for(i in 1:nrow(chems)) {
      chemical_id = chems[i,"chemical_id"]
      dtxsid = chems[i,"dtxsid"]
      query = paste0("update toxval set dtxsid='",dtxsid,"' where chemical_id='",chemical_id,"'")
      runQuery(query,toxval.db)
      if(verbose) if(i%%500==0) cat("  chemicals updated:",i," out of ",nrow(chems),"\n")
    }
  }
}
