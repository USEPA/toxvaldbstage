#-------------------------------------------------------------------------------------
#'
#' Perform the DSSTox mapping
#'
#' @param toxval.db The version of toxvaldb to use.
#' @param verbose If TRUE, print out extra diagnostic messages
#-------------------------------------------------------------------------------------
map.chemical.to.dsstox.by.source <- function(toxval.db,source,verbose=T) {
  printCurrentFunction(paste(toxval.db,":", source))
  if(!exists("DSSTOX")) {
    sys.date <- "2021-08-12"
    file <- paste0("./DSSTox/DSSTox_",sys.date,".RData")
    load(file)
    DSSTOX <<- DSSTOX
  }
  runQuery(paste0("update source_chemical set name=source_name where dtxsid='-' and name='-' and source like '",source,"'"),toxval.db)
  runQuery(paste0("update source_chemical set casrn=source_casrn where dtxsid='-' and casrn='-' and source like '",source,"'"),toxval.db)
  
  casrn.list <- runQuery(paste0("select distinct source_casrn from source_chemical where dtxsid='-' and source like '",source,"'"),toxval.db)[,1]
  
  cat("length of casrn.list:",length(casrn.list),"\n")
  if(length(casrn.list)==0) return()
  for(i in 1:length(casrn.list)) {
    #print(casrn.list[i])
    source_casrn <- str_trim(stri_enc_toutf8(stri_enc_toascii(casrn.list[i])),"both")
    #cat("[",source_casrn,"]\n",sep="")
    if(is.element(source_casrn,DSSTOX$casrn)) {
      dtxsid <- DSSTOX[is.element(DSSTOX$casrn,source_casrn),"dsstox_substance_id"]
      pname <- DSSTOX[is.element(DSSTOX$casrn,source_casrn),"preferred_name"]
      pname <- str_replace_all(pname,"\'","\\\\'")
      query <- paste0("update source_chemical set name='",pname,"', dtxsid='",dtxsid,"' where source_casrn='",source_casrn,"' and source like '",source,"'")
      #if(verbose) cat(source_casrn,pname,"\n")
      #browser()
      runQuery(query,toxval.db)
    }
    else {
      dtxsid_null <- paste0("NODTXSID_",source_casrn)
      dtxsid_null <- substr(dtxsid_null,1,40)
      dtxsid_null <- str_replace_all(dtxsid_null,"\'","\\\\'")
      dtxsid_null <- str_replace_all(dtxsid_null,"\n","")
      source_casrn <- str_replace_all(source_casrn,"\'","\\\\'")
      if(verbose) cat(source_casrn,"\n")
      #browser()
      query <- paste0("update source_chemical set dtxsid='",dtxsid_null,"' where source_casrn='",source_casrn,"' and source like '",source,"'")
      runQuery(query,toxval.db)
    }
    if(verbose) if(i%%100==0) cat("chemicals updated:",i," out of ",length(casrn.list),"\n")
  }
}
