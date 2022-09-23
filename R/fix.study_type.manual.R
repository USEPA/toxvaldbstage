#-------------------------------------------------------------------------------------
#' Fix the study_type using manual curation
#' @param toxval.db The version of toxval in which the data is altered.
#' @return The database will be altered
#' @export
#--------------------------------------------------------------------------------------
fix.study_type.manual = function(toxval.db,source=NULL,filename="toxval_new_study_type 2022-09-14"){
  printCurrentFunction(toxval.db)
  file = paste0(toxval.config()$datapath,"dictionary/",filename,".xlsx")
  print(file)
  mat = read.xlsx(file)
  mat = mat[mat$dtxsid!='NODTXSID',]
  mat = mat[!is.na(mat$dtxsid),]
  slist = sort(unique(mat$source))
  if(!is.null(source)) slist = source
  #slist = slist[!is.element(slist,c("ToxRefDB","ECOTOX"))]
  for(source in slist) {
    temp0 = mat[is.element(mat$source,source),]
    temp0$key = paste(temp0[,1],temp0[,2],temp0[,3],temp0[,4])
    temp = unique(temp0[,c("source_hash","study_type_corrected")])
    names(temp) = c("source_hash","study_type")

    temp.old = runQuery(paste0("select source_hash,study_type from toxval where source='",source,"'"),db)

    temp$code = paste(temp$source_hash,temp$study_type)
    temp.old$code = paste(temp.old$source_hash,temp.old$study_type)
    n1 = nrow(temp)
    n2 = nrow(temp.old)
    temp = temp[!is.element(temp$code,temp.old$code),]
    n3 = nrow(temp)

    cat("==============================================\n")
    cat(source,n1,n2,n3,"\n")
    cat("==============================================\n")
    for(i in 1:nrow(temp)) {
      hk = temp[i,"source_hash"]
      st = temp[i,"study_type"]
      query = paste0("update toxval set study_type='",st,"' where source_hash='",hk,"'")
      runQuery(query,db)
      if(i%%1000==0) cat("finished",i,"out of",nrow(temp),"\n")
    }

    check = runQuery(paste0("select dtxsid,source,study_type,source_hash from toxval where dtxsid!='NODTXSID' and source='",source,
                            "' and toxval_type in (select toxval_type from toxval_type_dictionary where toxval_type_supercategory in ('Point of Departure','Lethality Effect Level'))
                            and human_eco='human health'"),toxval.db)
    check$key = paste(check[,1],check[,2],check[,3],check[,4])
    check = check[order(check$source_hash),]
    temp0 = temp0[order(temp0$source_hash),]

    key1 = temp0$key
    key2 = check$key
    n1 = length(key1[!is.element(key1,key2)])
    n2 = length(key2[!is.element(key2,key1)])
    cat("  check: ",source,n1,n2,"\n")
    if(n1>0 || n2>0) browser()
  }




}
