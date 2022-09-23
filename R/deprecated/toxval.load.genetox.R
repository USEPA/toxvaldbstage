#-------------------------------------------------------------------------------------
#' Load the Genetox data from Grace
#' @param toxval.db The database to use.
#' @param verbose If TRUE output debug information
#' @param do.read If TRUE, read in the DSSTox file
#' @export
#--------------------------------------------------------------------------------------
toxval.load.genetox <- function(toxval.db, source.db, verbose=F,do.read=T) {
  printCurrentFunction(toxval.db)

  # if(do.read) {
  #   file = paste0(toxval.config()$datapath,"chemicals/TSCA active final/DSSTox_TSCAACTIVEUNION++_20181203.xlsx")
  #   chems = read.xlsx(file)
  #   rownames(chems) = chems[,"DSSTox_Substance_Id"]
  #   CHEMS.GT <= chems
  # }
  # chems = CHEMS.GT

  file = paste0(toxval.config()$datapath,"genetox/dec10/genetox_scored_allTSCA_101218_rev.xlsx")
  res1 = read.xlsx(file)
  res1 = res1[,c(2,3,4)]
  names(res1)[1] = "dtxsid"
  rownames(res1) = res1[,"dtxsid"]
  file = paste0(toxval.config()$datapath,"genetox/genetox_scored_qc_poc_280419.xlsx")
  res2 = read.xlsx(file)
  res2 = res2[,c(2,3,4)]
  names(res2)[1] = "dtxsid"
  rownames(res2) = res2[,"dtxsid"]
  for(i in 1:nrow(res1)) {
    dtx = res1[i,"dtxsid"]
    res2[dtx,] = res1[dtx,]
  }
  res = res2
  dsstox = DSSTOX
  rownames(dsstox) = dsstox$dtxsid
  browser()
  chems = dsstox[res$dsstox,c("casrn","preferred_name")]
  names(chems) = c("casrn","name")
  res = cbind(chems,res)
  browser()
  # dtlist = res[,"dtxsid"]
  #
  #
  # res$name = chems[dtlist,"Substance_Name"]
  # res$casrn = chems[dtlist,"Substance_CASRN"]
  # res = res[,c("casrn","name","Overall_genetox_call")]
  # names(res)[3] = "genetox_call"
  #
  # res = res[!is.na(res[,"casrn"]),]

  res2 = source_chemical.extra(toxval.db,source.db,mat,"Genetox Summary")
  res2 = subset(res2,select=-c(casrn,name,chemical_index))

  # cas.list = res[,1:2]
  # cid.list = get.cid.list.toxval(toxval.db, cas.list,"Genotoxicity")
  # res$chemical_id = cid.list$chemical_id
  # #res = merge(res,cid.list)
  # res = res[,3:4]

  #runQuery("delete from genetox_summary",db)
  runInsertTable(res, "genetox_summary", toxval.db,verbose)
}
