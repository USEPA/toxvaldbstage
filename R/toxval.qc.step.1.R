#-------------------------------------------------------------------------------------
#' do an initial QC of the data by comparing hte current database to an old one
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @export
#--------------------------------------------------------------------------------------
toxval.qc.step.1 <- function(db.new="res_toxval_v92",db.old="dev_toxval_v91") {
  printCurrentFunction()
  slist1 = runQuery("select distinct source from toxval",db.new)[,1]
  slist2 = runQuery("select distinct source from toxval",db.old)[,1]
  slist = slist1[is.element(slist1,slist2)]
  nlist = c("source","db.new","db.old","records.new","records.old","match","fraction")
  res= as.data.frame(matrix(nrow=length(slist),ncol=length(nlist)))
  names(res) = nlist
  res$source = slist
  res$db.new = db.new
  res$db.old = db.old
  rownames(res) = res$source
  clist = c("dtxsid",
            "toxval_type",
            "toxval_numeric",
            "toxval_units",
            "exposure_route",
            "species_original",
            "risk_assessment_class",
            "study_type")
  for(i in 1:length(slist)) {
    source =slist[i]
    cat(source,"\n")
    query = paste0("select dtxsid,toxval_type,toxval_numeric,toxval_units,exposure_route,
                    species_original,risk_assessment_class,study_type
                    from toxval where source='",source,"'")
    mat1 = runQuery(query,db.new)
    mat2 = runQuery(query,db.old)
    mat1$hash = NA
    mat2$hash = NA
    for(i in 1:nrow(mat1)) mat1[i,"hash"] <- digest(paste0(mat1[i,],collapse=""), serialize = FALSE)
    for(i in 1:nrow(mat2)) mat2[i,"hash"] <- digest(paste0(mat2[i,],collapse=""), serialize = FALSE)
    res[source,"records.new"] = nrow(mat1)
    res[source,"records.old"] = nrow(mat2)
    hash1 = mat1$hash
    hash2 = mat2$hash
    hash = has1[is.element(hash1,hash2)]
    res[source,"match"] = nrow(hash)
    browser()
  }
  dir = "../toxval_qc_step_1/"
  file = paste0(dir,"toxval.qc.step.1 ",db.new," ",db_new," ",Sys.Date(),".xlsx")
  write.xlsx(res,file)
}
