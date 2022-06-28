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
  chems = runQuery(paste0("select distinct chemical_id,cleaned_casrn as casrn from source_chemical where source='",source,"'"),toxval.db)
  chems$dtxsid = NA
  for(i in 1:nrow(chems)) {
    casrn = chems[i,"casrn"]
    if(is.element(casrn,dsstox$casrn)) {
      chems[i,"dtxsid"] = dsstox[casrn,"dsstox_substance_id"]
      chems[i,"casrn"] = casrn
      chems[i,"name"] = dsstox[casrn,"preferred_name"]
    }
    else chems[i,"dtxsid"] = "NODTXSID"
    if(verbose) if(i%%100==0) cat("dtxsid updated:",i," out of ",nrow(chems),"\n")
  }

  query = paste0("update source_chemical set casrn=cleaned_casrn, name=cleaned_name where source='",source,"'")
  runQuery(query,toxval.db)
  for(i in 1:nrow(chems)) {
    chemical_id = chems[i,"chemical_id"]
    casrn = chems[i,"casrn"]
    name = chems[i,"name"]
    dtxsid = chems[i,"dtxsid"]
    query = paste0("update toxval set dtxsid='",dtxsid,"' where chemical_id='",chemical_id,"'")
    runQuery(query,toxval.db)
    query = paste0("update source_chemical set dtxsid='",dtxsid,"' where chemical_id='",chemical_id,"'")
    runQuery(query,toxval.db)


    if(verbose) if(i%%100==0) cat("chemicals updated:",i," out of ",nrow(chems),"\n")
  }
}
