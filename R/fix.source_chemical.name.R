#-------------------------------------------------------------------------------------
#' Set all source_chemical names to the DSSTox preferred_name
#' @param toxval.db The version of toxval in which the data is altered.
#' @param soruce The name of the source the be fixed
#' @return The database will be altered
#' @export
#--------------------------------------------------------------------------------------
fix.source_chemical.name <- function(toxval.db,source=NULL){
  printCurrentFunction(paste(toxval.db,":", source))
  slist = runQuery("select distinct source from source_chemical",db)[,1]
  dsstox = DSSTOX
  dsstox = dsstox[!is.element(dsstox$dtxsid,"NODTXSID"),]
  rownames(dsstox) = dsstox$dtxsid
  if(!is.null(source)) slist = source
  for(source in slist) {
    chems = runQuery(paste0("select dtxsid,name from source_chemical where source='",source,"'"),toxval.db)
    chems = chems[is.element(chems$dtxsid,dsstox$dtxsid),]
    chems = chems[order(chems$name),]
    temp = dsstox[chems$dtxsid,]
    chems$new_name = temp$preferred_name
    chems2 = chems[chems$new_name!=chems$name,]
    cat(source,nrow(temp),nrow(chems2),"\n")
    if(nrow(chems2)>0) {
      for(i in 1:nrow(chems2)) {
        dtxsid = chems2[i,"dtxsid"]
        n0 = chems2[i,"new_name"]
        n0 = str_replace_all(n0,"\xb1","+/-")
        n1 = iconv(n0,from="UTF-8",to="ASCII//TRANSLIT")
        n2 = stri_escape_unicode(n1)
        n2 = gsub("'","\'",n2)
        #n2 = str_replace_all(n2,"\\\\'","\'")
        #n2 = str_replace_all(n2,"\\'","")
        #n2 = str_replace_all(n2,"\r"," ")
        #n2 = str_replace_all(n2,"\n"," ")
        n2 = str_replace_all(n2,"  "," ")
        n2 = str_trim(n2)

        query = paste0("update source_chemical set name='",n2,"' where dtxsid='",chems2[i,"dtxsid"],"' and source='",source,"'")
        cat(source," ---> ",chems2[i,"name"]," :::: ",n2,"\n")
        runQuery(query,toxval.db)
        if(i%%100==0) cat("fix.source_chemical.name finished",i, "out of ",nrow(chems2),"\n")
      }
      #browser()
    }
  }

}
