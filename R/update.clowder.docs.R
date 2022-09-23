#-------------------------------------------------------------------------------------
#'
#' Update the clowder links in toxval_source
#' @param db Database version of the source database
#' @param dir The directory where the update files sit
#-------------------------------------------------------------------------------------
update.clowder.docs <- function(toxval.db,source.db,dir="updates_2022-08-09") {
  printCurrentFunction()
  dir = paste0(toxval.config()$datapath,"clowder_v3/",dir,"/")
  flist = list.files(dir,pattern="clowder_id")
  slist = runQuery("select distinct source from source_chemical",source.db)[,1]
  sources = as.data.frame(cbind(slist,slist))
  sources[,2] = tolower(sources[,2])

  for(file0 in flist) {
    pointer = str_locate(file0,"_clowder")[1]-1
    source0 = str_replace_all(substr(file0,1,pointer),"_"," ")
    if(source0=="pprtv ornl") source0 = "pprtv (ornl)"
    cat(source0,"\n")
    if(is.element(source0,sources[,2])) {
      source = sources[is.element(sources[,2],source0),1]
      cat(source0,source,"\n")
      table = runQuery(paste0("select source_table from source_info where source='",source,"'"),toxval.db)[,1]
      cat(table,"\n")
      file = paste0(dir,file0)
      mat = read.xlsx(file)
      if(source=="EFSA2") {
        # n0 = runQuery(paste0("select count(*) from ",table," where clowder_id='-'"),source.db)[1,1]
        # for(i in 1:nrow(mat)) {
        #   cid = mat[i,"clowder_id"]
        #   dnm = mat[i,"document_name"]
        #   lrf = mat[i,"long_ref"]
        #   query = paste0("update ",table," set clowder_id='",cid,"', document_name='",dnm,"' where clowder_id='-' and long_ref='",lrf,"'")
        #   runQuery(query,source.db)
        #   #cat(query,"\n")
        #   #browser()
        # }
        # n1 = runQuery(paste0("select count(*) from ",table," where clowder_id='-'"),source.db)[1,1]
        # cat(source,table,n0,n1,"\n")
        # browser()
      }
      else if(source=="HAWC") {
        # sheets1 <- openxlsx::getSheetNames(file)
        # hawc_dfs <- lapply(sheets1, openxlsx::read.xlsx, xlsxFile = file)
        # n0 = runQuery(paste0("select count(*) from ",table," where clowder_id='-'"),source.db)[1,1]
        # for(j in 1:length(hawc_dfs)) {
        #   cat("sheet",j,"\n")
        #   mat = hawc_dfs[[j]]
        #   for(i in 1:nrow(mat)) {
        #     cid = mat[i,"clowder_id"]
        #     dnm = mat[i,"document_name"]
        #     lrf = str_replace_all(mat[i,"animal_group.experiment.study.full_citation"],"\\'","")
        #     query = paste0("update ",table," set clowder_id='",cid,"', document_name='",dnm,"' where clowder_id='-' and long_ref='",lrf,"'")
        #     runQuery(query,source.db)
        #   }
        # }
        # runQuery("update source_hawc set clowder_id='-',document_name='-' where clowder_id='need to find'",source.db)
        # n1 = runQuery(paste0("select count(*) from ",table," where clowder_id='-'"),source.db)[1,1]
        # cat(source,table,n0,n1,"\n")
        # browser()
      }
      else if(source=="HAWC PFAS 150") {
        # n0 = runQuery(paste0("select count(*) from ",table),source.db)[1,1]
        # for(i in 1:nrow(mat)) {
        #   cid = mat[i,"clowder_id"]
        #   dnm = mat[i,"document_name"]
        #   lrf = str_replace_all(mat[i,"animal_group.experiment.study.full_citation"],"\\'","")
        #   query = paste0("update ",table," set clowder_id='",cid,"', document_name='",dnm,"' where clowder_id='-' and long_ref='",lrf,"'")
        #   runQuery(query,source.db)
        #   #cat(query,"\n")
        #   #browser()
        # }
        # runQuery("update source_hawc_pfas_150 set clowder_id='-',document_name='-' where clowder_id='need to find'",source.db)
        # n1 = runQuery(paste0("select count(*) from ",table," where clowder_id='-'"),source.db)[1,1]
        # cat(source,table,n0,n1,"\n")
        # browser()
      }
      else if(source=="HAWC PFAS 430") {
        # n0 = runQuery(paste0("select count(*) from ",table),source.db)[1,1]
        # for(i in 1:nrow(mat)) {
        #   cid = mat[i,"clowder_id"]
        #   dnm = mat[i,"document_name"]
        #   lrf = str_replace_all(mat[i,"animal_group.experiment.study.full_citation"],"\\'","")
        #   query = paste0("update ",table," set clowder_id='",cid,"', document_name='",dnm,"' where clowder_id='-' and long_ref='",lrf,"'")
        #   runQuery(query,source.db)
        # }
        # runQuery("update source_hawc_pfas_430 set clowder_id='-',document_name='-' where clowder_id='need to find'",source.db)
        # n1 = runQuery(paste0("select count(*) from ",table," where clowder_id='-'"),source.db)[1,1]
        # cat(source,table,n0,n1,"\n")
        # browser()
      }
      else if(source=="IRIS") {
        # n0 = runQuery(paste0("select count(*) from ",table," where clowder_id='-'"),source.db)[1,1]
        # for(i in 1:nrow(mat)) {
        #   cid = mat[i,"clowder_id"]
        #   dnm = str_replace_all(mat[i,"document_name"],"\\'","")
        #   casrn = mat[i,"casrn"]
        #   query = paste0("update ",table," set clowder_id='",cid,"', document_name='",dnm,"' where clowder_id='-' and casrn='",casrn,"'")
        #   runQuery(query,source.db)
        #   #cat(query,"\n")
        #   #browser()
        # }
        # n1 = runQuery(paste0("select count(*) from ",table," where clowder_id='-'"),source.db)[1,1]
        # cat(source,table,n0,n1,"\n")
        # browser()
      }
      else if(source=="PPRTV (ORNL)") {
        # n0 = runQuery(paste0("select count(*) from ",table," where clowder_id='-'"),source.db)[1,1]
        # for(i in 1:nrow(mat)) {
        #   cid = mat[i,"clowder_id"]
        #   dnm = str_replace_all(mat[i,"document_name"],"\\'","")
        #   cleaned_casrn = mat[i,"casrn"]
        #   if(!is.na(cleaned_casrn)) {
        #     casrn0 = runQuery(paste0("select raw_casrn from source_chemical where source='PPRTV (ORNL)' and cleaned_casrn='",cleaned_casrn,"'"),source.db)[1,1]
        #     query = paste0("update ",table," set clowder_id='",cid,"', document_name='",dnm,"' where clowder_id='-' and casrn='",casrn0,"'")
        #     runQuery(query,source.db)
        #     #cat(query,"\n")
        #     #browser()
        #   }
        # }
        # n1 = runQuery(paste0("select count(*) from ",table," where clowder_id='-'"),source.db)[1,1]
        # cat(source,table,n0,n1,"\n")
        # browser()
      }
    }
  }
}
