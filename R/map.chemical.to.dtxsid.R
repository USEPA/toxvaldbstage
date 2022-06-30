#-------------------------------------------------------------------------------------
#'
#' Perform the DSSTox mapping
#'
#' @param toxval.db The version of toxvaldb to use.
#' @param verbose If TRUE, print out extra diagnostic messages
#-------------------------------------------------------------------------------------
map.chemical.to.dtxsid <- function(toxval.db,source,verbose=T) {
  printCurrentFunction(paste(toxval.db,":", source))
  if(!exists("DSSTOX")) {
    sys.date <- "2021-08-12"
    file <- paste0("../DSSTox/DSSTox_",sys.date,".RData")
    load(file)
    DSSTOX <<- DSSTOX
  }
  dsstox = DSSTOX
  rownames(dsstox) = dsstox$casrn
  names(dsstox)[is.element(names(dsstox),"dsstox_substance_id")] <- "dtxsid"

  chems = runQuery(paste0("select distinct chemical_id,cleaned_casrn as casrn from source_chemical where source='",source,"'"),toxval.db)
  chems$dtxsid = "NODTXSID"
  #rownames(chems) = chems$casrn
  clist = chems$casrn
  clist = clist[is.element(clist,dsstox$casrn)]
  dtemp = dsstox[clist,]
  for(i in 1:length(clist)) {
    casrn = clist[i]
    name = dtemp[casrn,"preferred_name"]
    dtxsid = dtemp[casrn,"dtxsid"]
    chems[is.element(chems$casrn,casrn),"dtxsid"] = dtxsid
    chems[is.element(chems$casrn,casrn),"casrn"] = casrn
    chems[is.element(chems$casrn,casrn),"name"] = name
    if(verbose) if(i%%500==0) cat("dtxsid updated:",i," out of ",nrow(chems),"\n")
  }

  #runQuery("analyze table source_chemical",toxval.db)
  query = paste0("update source_chemical set casrn=cleaned_casrn, name=cleaned_name where source='",source,"'")
  runQuery(query,toxval.db)
  #runQuery("analyze table source_chemical",toxval.db)
  for(i in 1:nrow(chems)) {
    chemical_id = chems[i,"chemical_id"]
    casrn = chems[i,"casrn"]
    name = chems[i,"name"]
    dtxsid = chems[i,"dtxsid"]
    query = paste0("update source_chemical set dtxsid='",dtxsid,"' where chemical_id='",chemical_id,"'")
    runQuery(query,toxval.db)
    query = paste0("update toxval set dtxsid='",dtxsid,"' where chemical_id='",chemical_id,"'")
    runQuery(query,toxval.db)
    if(verbose) if(i%%500==0) cat("chemicals updated:",i," out of ",nrow(chems),"\n")
  }
}
