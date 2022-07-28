#-------------------------------------------------------------------------------------
#' do an initial QC of the data by comparing the current database to an old one
#' @param db.new The new database version (toxval) for the comparison
#' @param db.old = The old database version for the comparison
#' @export
#--------------------------------------------------------------------------------------
toxval.qc.step.1 <- function(db.new="res_toxval_v92",db.old="dev_toxval_v9_1") {
  printCurrentFunction()
  dir = paste0(toxval.config()$datapath,"toxvaldb_qc_step_1/",db.old,"/")
  #dir = "../toxval_qc_step_1/"
  slist1 = runQuery("select distinct source from toxval",db.new)[,1]
  slist2 = runQuery("select distinct source from toxval",db.old)[,1]
  slist = slist1[is.element(slist1,slist2)]

  exclude = c("ECOTOX")
  # if(db.old=="dev_toxval_v8")
  #   exclude = c("Alaska DEC","Cal OEHHA","California DPH","Chiu","COSMOS","DOD",
  #             "DOE Protective Action Criteria ","DOE Wildlife Benchmarks","ECOTOX",
  #             "EPA OPP","EPA OPPT","FDA CEDI","HEAST","HESS","HPVIS","IRIS",
  #             "Mass. Drinking Water Standards","NIOSH","OSHA Air Contaminants",
  #             "OW Drinking Water Standards","Pennsylvania DEP MCLs","Pennsylvania DEP ToxValues",
  #             "PPRTV (ORNL)","TEST","WHO IPCS","Wignall","EFSA","HAWC","PPRTV (NCEA)",
  #             "DOE Protective Action Criteria","",
  #             "OSHA Air Contaminants","EPA AEGL","","","","","","","","","")
  # else exclude = c()
  slist = slist[!is.element(slist,exclude)]
  #slist = c("Health Canada","Alaska DEC","ToxRefDB","OSHA Air Contaminants","USGS HBSL")
  #slist = c("TEST")
  #slist = "Alaska DEC"
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
    query = paste0("select a.dtxsid,b.casrn,b.name,a.toxval_type,toxval_subtype,a.toxval_numeric,a.toxval_units,a.exposure_route,
                    a.risk_assessment_class,a.study_type
                    from toxval a, chemical b where a.dtxsid=b.dtxsid and a.source='",source,"'")
    # query = paste0("select dtxsid,toxval_type,toxval_numeric,toxval_units,exposure_route,
    #                 study_type
    #                 from toxval where source='",source,"'")
    mat1 = unique(runQuery(query,db.new))
    mat2 = unique(runQuery(query,db.old))
    mat1$hash = NA
    mat2$hash = NA
    nlist0 = c("dtxsid","toxval_type","toxval_subtype","toxval_numeric","toxval_units","exposure_route","risk_assessment_class","study_type")
    for(i in 1:nrow(mat1)) mat1[i,"hash"] <- digest(paste0(mat1[i,nlist0],collapse=""), serialize = FALSE)
    for(i in 1:nrow(mat2)) mat2[i,"hash"] <- digest(paste0(mat2[i,nlist0],collapse=""), serialize = FALSE)

    res[source,"records.new"] = nrow(mat1)
    res[source,"records.old"] = nrow(mat2)
    hash1 = mat1$hash
    hash2 = mat2$hash
    hash = hash1[is.element(hash1,hash2)]
    res[source,"match"] = length(hash)
    res[source,"fraction"] = length(hash)/length(hash1)

    mat1$db = db.new
    mat2$db = db.old
    mat = rbind(mat1,mat2)
    mat = mat[order(mat$db),]
    mat = mat[order(mat$toxval_numeric),]
    mat = mat[order(mat$dtxsid),]
    mat = mat[!is.element(mat$hash,hash),]
    if(nrow(mat)>0) {
      file = paste0(dir,"dbcomp ",source," ",db.new," ",db.old," ",Sys.Date(),".xlsx")
      write.xlsx(mat,file)
    }
  }
  file = paste0(dir,"toxval.qc.step.1 ",db.new," ",db.old," ",Sys.Date(),".xlsx")
  write.xlsx(res,file)
}
