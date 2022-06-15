#--------------------------------------------------------------------------------------
#' Add the hash key to the source tables and add the new rows
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param source Name of the source
#' @param table Name of the database table
#' @param do.reset If TRUE, delete data from the database for this source before
#' inserting new data. Default FALSE
#' @param do.insert If TRUE, insert data into the database, default TRUE
#' @param res The data frame to be processed
#--------------------------------------------------------------------------------------
toxval_source.hash.and.load <- function(db="dev_toxval_source_v5",
                                        source,
                                        table,
                                        do.reset=F,
                                        do.insert=F,
                                        res) {

  printCurrentFunction(paste(db,source,table))
  if(is.element("chemical_index",names(res))) res = subset(res,select=-c(chemical_index))
  res$source_hash = "-"
  #####################################################################
  cat("Build the hash key \n")
  #####################################################################
  nold = runQuery(paste0("select count(*) from ",table),db)
  if(nold>0) {
    sample0 = runQuery(paste0("select * from ",table, " limit 1"),db)
    nlist = names(sample0)
    nlist = nlist[!is.element(nlist,c("chemical_id","source_id","clowder_id","document_name","source_hash","qc_status"))]
    sh0 = sample0[1,"source_hash"]
    sample = sample0[,nlist]
    sh1 = digest(paste0(sample,collapse=""), serialize = FALSE)
    cat("test that the columns are right for the hash key: ",sh0,sh1,"\n")
    if(sh0!=sh1) browser()
  }
  else {
    nlist = runQuery(paste0("desc ",table),db)[,1]
    nlist = nlist[!is.element(nlist,c("chemical_id","source_id","clowder_id","document_name","source_hash","qc_status"))]
  }

  #####################################################################
  cat("check the columns\n")
  #####################################################################
  nlist0 = names(res)
  nlist0 = nlist0[!is.element(nlist0,c("chemical_id","source_id","clowder_id","document_name","source_hash","qc_status"))]
  nlist01 = nlist0[!is.element(nlist0,nlist)]
  nlist10 = nlist[!is.element(nlist,nlist0)]
  if(length(nlist01)>0) {
    cat("res has columns not in db\n")
    print(nlist01)
    print(nlist10)
    browser()
  }

  res$source_hash = "-"
  nlist1 = names(res)
  to.remove = nlist1[!is.element(nlist1,nlist)]
  nlist1 = nlist1[!is.element(nlist1,to.remove)]
  to.add = nlist[!is.element(nlist,nlist1)]
  if(length(to.add)>0) {
    cat("columns to add:",to.add,"\n")
    temp = as.data.frame(matrix(nrow=nrow(res),ncol=length(to.add)))
    names(temp) = to.add
    temp[] = "-"
    res = cbind(res,temp)
  }
  else cat("no columns need to be added\n")
  res.temp = res[,nlist]

  for (i in 1:nrow(res)){
    row <- res.temp[i,]
    res[i,"source_hash"] <- digest(paste0(row,collapse=""), serialize = FALSE)
    if(i%%1000==0) cat(i," out of ",nrow(res),"\n")
  }
  #####################################################################
  cat("See what is new \n")
  #####################################################################
  shlist0 = runQuery(paste("select source_hash from ",table),db)[,1]
  shlist1 = res$source_hash
  n0 = length(shlist0)
  n1 = length(shlist1)
  n01 = length(shlist0[is.element(shlist0,shlist1)])
  newfrac = 100*(n1-n01)/n1
  cat("**************************************************************************\n")
  cat(source,"\n")
  cat("hash matching: original,new,match:",n0,n1,n01," new percent: ",format(newfrac,digits=2),"\n")
  cat("**************************************************************************\n")

  if(do.reset) {
    #####################################################################
    cat("Do you really want to clean the database?\n")
    browser()
    #####################################################################
    runQuery(paste("delete from ",table),db)
  }
  else res = res[!is.element(res$source_hash,shlist0),]

  #####################################################################
  cat("Add to the database \n")
  #####################################################################
  if(nrow(res)>0) {
    cat("entering new rows:",nrow(res),"\n")
    if(do.insert) runInsertTable(res,table,db,do.halt=T,verbose=F)
  }
  else cat("no new rows to add\n")
  # r1 = res[1,]
  # r2 = runQuery(paste0("select * from ",table," where `name`='",r1$name,"'"),db)
  # nlist = names(r1)[is.element(names(r1),names(r2))]
  # r1 = r1[,nlist]
  # r2 = r2[,nlist]
  # browser()
  # #r2 = r2[is.element(r2$url,r1$url),]
  # x = res.temp[is.element(res.temp$casrn,sample$casrn),]
  # y = x[is.element(x$critical_effect,sample$critical_effect),]
  # z = y[1,]
  # z==sample
  # sample1 = runQuery(paste0("select * from ",table, " limit 1"),db)
  # sh0 = sample1$source_hash
  # sh1 = digest(paste0(z,collapse=""), serialize = FALSE)
  # sample2 = sample1[,names(z)]
  # sample2$source_hash = "-"
  # sh2 = digest(paste0(sample2,collapse=""), serialize = FALSE)
}
# deal with source-specific fixes
# if(table=="alaska_dec") {
#   res[is.na(res$phenotype),"phenotype"] = "-"
#   res$clowder_id = NA
# }
# if(source=="ATSDR MRLs 2020") {
#   res$clowder_id = NA
#   res$document_name = NA
#   res[is.na(res$source_name_cid),"source_name_cid"] = "-"
# }
# if(table=="new_atsdr_pfas_2021") {
#   res$clowder_id = "6238b97ae4b0b18cb57ce4f6"
#   res$document_name = "tp200-c2.pdf"
#   res[is.na(res$source_name_cid),"source_name_cid"] = "-"
#   res[is.na(res$critical_effect_original),"critical_effect_original"] = "-"
#   res[is.na(res$exposure_form_original),"exposure_form_original"] = "-"
#   res[is.na(res$exposure_form),"exposure_form"] = "-"
#   res[is.na(res$strain),"strain"] = "-"
#   res[is.na(res$strain_original),"strain_original"] = "-"
#   res[is.na(res$species_original),"species_original"] = "-"
#   res[is.na(res$exposure_form),"exposure_form"] = "-"
#   res[is.na(res$toxval_details),"toxval_details"] = "-"
# }
# if(table=="new_atsdr_pfas") {
#   res$clowder_id = "6238e943e4b0b18cb57ced5a"
#   res[is.na(res$sex),"sex"] = "-"
#   #res[is.na(res$toxval_numeric),"toxval_numeric"] = -1
#   res$document_name = NA
# }
# if(table=="original_caloehha_table") {
#   res[is.na(res)] = "-"
#   res$document_name = NA
#   res$clowder_id = NA
# }
# if(table=="original_copper_table") {
#   res[is.na(res)] = "-"
#   res$document_name = NA
#   res$clowder_id = NA
# }
# if(table=="original_cosmos") {
#   res[is.na(res)] = "-"
#   res$document_name = NA
#   res$clowder_id = NA
# }
# if(table=="dod_meg_2013") {
#   res[is.na(res)] = "-"
#   res$document_name = NA
#   res$clowder_id = NA
# }
# if(table=="original_dod_ered_table") {
#   res[is.na(res)] = "-"
#   res$document_name = NA
#   res[res$clowder_id=="-","clowder_id"] = NA
#   #res$clowder_id = NA
# }
# if(table=="new_doe_table") {
#   res[is.na(res)] = "-"
#   res$document_name = NA
#   res[res$clowder_id=="-","clowder_id"] = NA
#   res[res$new_molecular_weight_MW=="-","new_molecular_weight_MW"] = NA
#   res[res$LEL_ppm=="-","LEL_ppm"] = NA
#   res[res[,"ppm_to_mg/m3"]=="-","ppm_to_mg/m3"] = NA
# }
# if(table=="original_doe_benchmarks_table") {
#   res[is.na(res)] = "-"
#   res$document_name = NA
#   res$SOURCE_NAME_CID = NA
#   res[res$clowder_id=="-","clowder_id"] = NA
# }
# if(table=="new_echa3") {
#   res[is.na(res)] = "-"
#   res$document_name = NA
#   res[res$clowder_id=="-","clowder_id"] = NA
#   res[res$toxval_numeric=="-","toxval_numeric"] = NA
#   res[res$generation=="-","generation"] = NA
#   res[res$study_duration_value=="-","study_duration_value"] = NA
# }
# if(table=="new_echa") {
#   res[is.na(res)] = "-"
#   res$document_name = NA
#   res[res$clowder_id=="-","clowder_id"] = NA
#   res[res$toxval_numeric=="-","toxval_numeric"] = NA
#   res[res$generation=="-","generation"] = NA
#   res[res$study_duration_value=="-","study_duration_value"] = NA
# }
# if(table=="new_echa_iuclid") {
#   res[is.na(res)] = "-"
#   res$document_name = NA
#   res[res$clowder_id=="-","clowder_id"] = NA
# }
# if(table=="new_wignall_table") {
#   res[is.na(res)] = "-"
# }
# if(table=="new_test_table") {
#   res[is.na(res)] = "-"
# }
# if(table=="original_chiu") {
#   res$document_name = NA
#   res[res$clowder_id=="-","clowder_id"] = NA
# }
# if(table=="original_lanl_table") {
#   res[is.na(res)] = "-"
#   res$document_name = NA
#   res[res$clowder_id=="-","clowder_id"] = NA
# }
# if(table=="may_2021_rsl_table") {
#   res[is.na(res)] = "-"
#   res$document_name = NA
#   res[res$clowder_id=="-","clowder_id"] = NA
# }
# if(table=="nov_2021_rsl_table") {
#   res[is.na(res)] = "-"
#   res$document_name = NA
#   res[res$clowder_id=="-","clowder_id"] = NA
# }
# if(table=="new_pprtv_ncea") {
#   res[is.na(res)] = "-"
#   res$document_name = NA
#   res[res$clowder_id=="-","clowder_id"] = NA
# }
# if(table=="new_pprtv_ornl") {
#   res[is.na(res)] = "-"
#   res$document_name = NA
#   res[res$clowder_id=="-","clowder_id"] = NA
# }
# if(table=="new_penn_table") {
#   res[is.na(res)] = "-"
#   res$document_name = NA
#   res[res$clowder_id=="-","clowder_id"] = NA
# }
# if(table=="new_pfas_summary_pods") {
#   res[is.na(res)] = "-"
#   res$document_name = NA
#   res[res$clowder_id=="-","clowder_id"] = NA
# }
# if(table=="original_pfas_150_sem") {
#   res[is.na(res)] = "-"
#   res$document_name = NA
#   res[res$clowder_id=="-","clowder_id"] = NA
# }
# if(table=="new_niosh") {
#   res[is.na(res)] = "-"
#   res$document_name = NA
#   res[res$clowder_id=="-","clowder_id"] = NA
# }
# if(table=="new_combined_hpvis_table") {
#   res[is.na(res)] = "-"
#   res$document_name = NA
#   res[res$clowder_id=="-","toxval_upper_range"] = NA
#   res[res$clowder_id=="-","clowder_id"] = NA
# }
# if(table=="original_hess") {
#   res[is.na(res)] = "-"
#   res[res$clowder_id=="-","clowder_id"] = NA
# }
# if(table=="hawc_new") {
#   res[is.na(res)] = "-"
#   res[res$clowder_id=="-","clowder_id"] = NA
# }
# if(table=="original_hawc_pfas_150") {
#   res[is.na(res)] = "-"
#   res[res$clowder_id=="-","clowder_id"] = NA
# }
# if(table=="original_hawc_pfas_430") {
#   res[is.na(res)] = "-"
#   res[res$clowder_id=="-","clowder_id"] = NA
#  }
# if(table=="new_iris") {
#   res[is.na(res)] = "-"
#   res[res$clowder_id=="-","clowder_id"] = NA
#   res[res$document_name=="-","document_name"] = NA
# }
# if(table=="new_heast") {
#   res[is.na(res)] = "-"
#   res[res$clowder_id=="-","clowder_id"] = NA
#   res[res$year=="-","year"] = NA
# }
# if(table=="new_heast_rfd_rfc") {
#   res[is.na(res)] = "-"
#   res[res$clowder_id=="-","clowder_id"] = NA
#   res[res$year=="-","year"] = NA
# }
# if(table=="new_pfas_150_sem") {
#   res[is.na(res)] = "-"
#   res[res$clowder_id=="-","clowder_id"] = NA
# }
# if(is.element(table,c("new_health_canada_table",
#                       "original_health_canada_table",
#                       "original_opp_table",
#                       "new_opp_table",
#                       "original_oppt_table",
#                       "new_oppt_table",
#                       "new_efsa2",
#                       "new_efsa"))) {
#   res[is.na(res)] = "-"
#   res[res$clowder_id=="-","clowder_id"] = NA
#   res[res$document_name=="-","document_name"] = NA
#   if(table=="original_opp_table") res[res[,"cancer.slope.factor.(mg/kg-day)-1"]=="-","cancer.slope.factor.(mg/kg-day)-1"] = NA
# }
# if(is.element(table,c("original_envirotox"))) {
#   res[is.na(res)] = "-"
#   #res[res$clowder_id=="-","clowder_id"] = NA
#   res[res$document_name=="-","document_name"] = NA
#   if(table=="original_opp_table") res[res[,"cancer.slope.factor.(mg/kg-day)-1"]=="-","cancer.slope.factor.(mg/kg-day)-1"] = NA
# }
# if(is.element(table,c("new_efsa2"))) {
#   res[is.na(res)] = "-"
#   res[res$clowder_id=="-","clowder_id"] = ""
#   #res[res$document_name=="-","document_name"] = NA
# }
# if(is.element(table,c("echa2"))) {
#   res[is.na(res)] = "-"
#   res[res$clowder_id=="-","clowder_id"] = NA
#   res[res$document_name=="-","document_name"] = NA
# }
# if(is.element(table,c("original_echa_echemportal_api"))) {
#   res[is.na(res)] = "-"
#   #res[res$clowder_id=="-","clowder_id"] = NA
#   #res[res$document_name=="-","document_name"] = NA
# }


# ADD SOURCE-SPECIFIC RULES ABOVE
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
