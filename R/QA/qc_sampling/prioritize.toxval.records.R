#-----------------------------------------------------------------------------------
#' Prioritize records for QC
#'
#' @param toxval.db Database version
#' @param sys.date Date string for ToxValDB export
#' @return prioritization files
#'
#-----------------------------------------------------------------------------------
prioritize.toxval.records <- function(toxval.db="res_toxval_v95",fraction=0.1,curation_method=NULL) {
  printCurrentFunction(toxval.db)
  dir = "Repo/data/qc_prioritization/"
  if(!exists("TOXVAL_ALL")) {
    file = paste0(dir,"/toxval_for_qc_prioritization.RData")
    load(file=file)
    TOXVAL_ALL <<- res
  }
  done = FALSE
  total_sampled = 0

  tv = TOXVAL_ALL
  tv = tv[tv$source_hash!="-",]
  tv = tv[tv$qc_status=="not determined",]
  curation_method = 'automated'
  total_records = nrow(tv)
  nlist = c("dtxsid","name","source_hash","source","toxval_type_supercategory","toxval_units","study_type","human_eco","source_table")
  tv = tv[,nlist]
  rownames(tv) = tv$source_hash

  repdose = c("subchronic","chronic","developmental","reproduction","reproduction developmental","neurotoxicity chronic","neurotoxicity subchronic")

  #---------------------------------------------------------------------------------------------------------------
  cat("Rule 1: ", fraction*100, "% human health repeat dose points of departure, mg/kg-day, mg/m3, exclude ECOTOX and ToxRefDB\n")
  tv2 = tv[!is.element(tv$source,c("ECOTOX","ToxRefDB")),]
  tv2 = tv2[tv2$human_eco=="human health",]
  tv2 = tv2[is.element(tv2$toxval_units,c("mg/kg-day","mg/m3")),]
  tv2 = tv2[tv2$toxval_type_supercategory=="Point of Departure",]
  tv2 = tv2[is.element(tv2$study_type,repdose),]
  slist = tv2$source_hash
  slist = sample(slist,length(slist)*fraction)
  tv$rule1 = 0
  tv[slist,"rule1"] = 1
  cat("  ",length(slist),"\n")
  total_sampled = total_sampled + length(slist)
  if ((curation_method == 'automated' & total_sampled > 200) |
      (curation_method == 'manual' & total_sampled > (total_records * 0.2))){
    done = TRUE
  }

  #---------------------------------------------------------------------------------------------------------------
  cat("Rule 2: 100% human health repeat dose points of departure, mg/kg-day, mg/m3, include all sources for PFAS\n")
  dir = paste0(dir,"input_files/")
  file = paste0(dir,"pfas_catalog 2023-06-29.xlsx")
  pfas = read.xlsx(file)
  dlist = pfas$dtxsid
  tv2 = tv[is.element(tv$dtxsid,dlist),]
  tv2 = tv2[tv2$human_eco=="human health",]
  tv2 = tv2[is.element(tv2$toxval_units,c("mg/kg-day","mg/m3")),]
  tv2 = tv2[tv2$toxval_type_supercategory=="Point of Departure",]
  tv2 = tv2[is.element(tv2$study_type,repdose),]
  slist = tv2$source_hash
  tv$rule2 = 0
  if(!done){
    tv[slist,"rule2"] = 1
    total_sampled = total_sampled + length(slist)
  }
  cat("  ",length(slist),"\n")
  if ((curation_method == 'automated' & total_sampled > 200) |
      (curation_method == 'manual' & total_sampled > (total_records * 0.2))){
    done = TRUE
  }


  #---------------------------------------------------------------------------------------------------------------
  cat("Rule 3: ", fraction*100, "% eco with units of mg/L\n")
  tv2 = tv[tv$human_eco=="eco",]
  tv2 = tv2[!is.element(tv2$source,c("ECOTOX","ToxRefDB")),]
  tv2 = tv2[is.element(tv2$toxval_units,c("mg/L")),]
  tv2 = tv2[tv2$toxval_type_supercategory=="Point of Departure",]
  #tv2 = tv2[is.element(tv2$study_type,repdose),]
  slist = tv2$source_hash
  slist = sample(slist,length(slist)*fraction)
  tv$rule3 = 0
  if(!done){
    tv[slist,"rule3"] = 1
    total_sampled = total_sampled + length(slist)
  }
  cat("  ",length(slist),"\n")
  if ((curation_method == 'automated' & total_sampled > 200) |
      (curation_method == 'manual' & total_sampled > (total_records * 0.2))){
    done = TRUE
  }


  #---------------------------------------------------------------------------------------------------------------
  cat("Rule 4: ", fraction*100, "% points of departure, human health, each source\n")
  slist = NULL
  tv2 = tv[tv$human_eco=="human health",]
  tv2 = tv2[!is.element(tv2$source,c("ECOTOX","ToxRefDB")),]
  srclist = sort(unique(tv2$source))
  for(src in srclist) {
    tv3 = tv2[tv2$source==src,]
    tv3 = tv3[tv3$toxval_type_supercategory=="Point of Departure",]
    if(nrow(tv3)>0) {
      slist3 = tv3$source_hash
      slist3 = sample(slist3,length(slist3)*fraction)
      slist = c(slist,slist3)
    }
  }
  tv$rule4 = 0
  if(!done){
    tv[slist,"rule4"] = 1
    total_sampled = total_sampled + length(slist)
  }
  cat("  ",length(slist),"\n")
  if ((curation_method == 'automated' & total_sampled > 200) |
      (curation_method == 'manual' & total_sampled > (total_records * 0.2))){
    done = TRUE
  }

  #---------------------------------------------------------------------------------------------------------------
  cat("Rule 5: ", fraction*100, "% lethality effect level, human health, each source\n")
  slist = NULL
  tv2 = tv[tv$human_eco=="human health",]
  tv2 = tv2[!is.element(tv2$source,c("ECOTOX","ToxRefDB")),]
  srclist = sort(unique(tv2$source))
  for(src in srclist) {
    tv3 = tv2[tv2$source==src,]
    tv3 = tv3[tv3$toxval_type_supercategory=="Lethality Effect Level",]
    if(nrow(tv3)>0) {
      slist3 = tv3$source_hash
      slist3 = sample(slist3,length(slist3)*fraction)
      slist = c(slist,slist3)
    }
  }
  tv$rule5 = 0
  if(!done){
    tv[slist,"rule5"] = 1
    total_sampled = total_sampled + length(slist)
  }
  cat("  ",length(slist),"\n")
  if ((curation_method == 'automated' & total_sampled > 200) |
      (curation_method == 'manual' & total_sampled > (total_records * 0.2))){
    done = TRUE
  }

  #---------------------------------------------------------------------------------------------------------------
  cat("Rule 6: ", fraction*100, "% toxicity value, human health, each source\n")
  slist = NULL
  tv2 = tv[tv$human_eco=="human health",]
  tv2 = tv2[!is.element(tv2$source,c("ECOTOX","ToxRefDB")),]
  srclist = sort(unique(tv2$source))
  for(src in srclist) {
    tv3 = tv2[tv2$source==src,]
    tv3 = tv3[tv3$toxval_type_supercategory=="Toxicity Value",]
    if(nrow(tv3)>0) {
      slist3 = tv3$source_hash
      slist3 = sample(slist3,length(slist3)*fraction)
      slist = c(slist,slist3)
    }
  }
  tv$rule6 = 0
  if(!done){
    tv[slist,"rule6"] = 1
    total_sampled = total_sampled + length(slist)
  }
  cat("  ",length(slist),"\n")
  if ((curation_method == 'automated' & total_sampled > 200) |
      (curation_method == 'manual' & total_sampled > (total_records * 0.2))){
    done = TRUE
  }

  #---------------------------------------------------------------------------------------------------------------
  cat("Rule 7: ", fraction*100, "% points of departure, eco, each source\n")
  slist = NULL
  tv2 = tv[tv$human_eco=="eco",]
  tv2 = tv2[!is.element(tv2$source,c("ECOTOX","ToxRefDB")),]
  srclist = sort(unique(tv2$source))
  for(src in srclist) {
    tv3 = tv2[tv2$source==src,]
    tv3 = tv3[tv3$toxval_type_supercategory=="Point of Departure",]
    if(nrow(tv3)>0) {
      slist3 = tv3$source_hash
      slist3 = sample(slist3,length(slist3)*fraction)
      slist = c(slist,slist3)
    }
  }
  tv$rule7 = 0
  if(!done){
    tv[slist,"rule7"] = 1
    total_sampled = total_sampled + length(slist)
  }
  cat("  ",length(slist),"\n")
  if ((curation_method == 'automated' & total_sampled > 200) |
      (curation_method == 'manual' & total_sampled > (total_records * 0.2))){
    done = TRUE
  }

  #---------------------------------------------------------------------------------------------------------------
  cat("Rule 8: ", fraction*100, "% lethality effect level, eco, each source\n")
  slist = NULL
  tv2 = tv[tv$human_eco=="eco",]
  tv2 = tv2[!is.element(tv2$source,c("ECOTOX","ToxRefDB")),]
  srclist = sort(unique(tv2$source))
  for(src in srclist) {
    tv3 = tv2[tv2$source==src,]
    tv3 = tv3[tv3$toxval_type_supercategory=="Lethality Effect Level",]
    if(nrow(tv3)>0) {
      slist3 = tv3$source_hash
      slist3 = sample(slist3,length(slist3)*fraction)
      slist = c(slist,slist3)
    }
  }
  tv$rule8 = 0
  if(!done){
    tv[slist,"rule8"] = 1
    total_sampled = total_sampled + length(slist)
  }
  cat("  ",length(slist),"\n")
  if ((curation_method == 'automated' & total_sampled > 200) |
      (curation_method == 'manual' & total_sampled > (total_records * 0.2))){
    done = TRUE
  }

  #---------------------------------------------------------------------------------------------------------------
  cat("Rule 9: ", fraction*100, "% toxicity value, eco, each source\n")
  slist = NULL
  tv2 = tv[tv$human_eco=="eco",]
  tv2 = tv2[!is.element(tv2$source,c("ECOTOX","ToxRefDB")),]
  srclist = sort(unique(tv2$source))
  for(src in srclist) {
    tv3 = tv2[tv2$source==src,]
    tv3 = tv3[tv3$toxval_type_supercategory=="Toxicity Value",]
    if(nrow(tv3)>0) {
      slist3 = tv3$source_hash
      slist3 = sample(slist3,length(slist3)*fraction)
      slist = c(slist,slist3)
    }
  }
  tv$rule9 = 0
  if(!done){
    tv[slist,"rule9"] = 1
    total_sampled = total_sampled + length(slist)
  }
  cat("  ",length(slist),"\n")
  if ((curation_method == 'automated' & total_sampled > 200) |
      (curation_method == 'manual' & total_sampled > (total_records * 0.2))){
    done = TRUE
  }

  #---------------------------------------------------------------------------------------------------------------
  cat("Rule 10: ", fraction*100, "% remaining toxval_type_supercategory, each source\n")
  slist = NULL
  tv2 = tv[!is.element(tv$toxval_type_supercategory,c("Toxicity Value","Lethality Effect Level","Point of Departure")),]
  tv2 = tv2[!is.element(tv2$source,c("ECOTOX","ToxRefDB")),]
  srclist = sort(unique(tv2$source))
  for(src in srclist) {
    tv3 = tv2[tv2$source==src,]
    if(nrow(tv3)>0) {
      slist3 = tv3$source_hash
      slist3 = sample(slist3,length(slist3)*fraction)
      slist = c(slist,slist3)
    }
  }
  tv$rule10 = 0
  if(!done){
    tv[slist,"rule10"] = 1
    total_sampled = total_sampled + length(slist)
  }
  cat("  ",length(slist),"\n")
  if ((curation_method == 'automated' & total_sampled > 200) |
      (curation_method == 'manual' & total_sampled > (total_records * 0.2))){
    done = TRUE
  }

  #---------------------------------------------------------------------------------------------------------------
  cat("summarize\n")
  x = tv[,c("rule1","rule2","rule3","rule4","rule5","rule6","rule7","rule8","rule9","rule10")]
  y = rowSums(x)
  y[y>1] = 1
  tv$do.qc = y
  sampled_records <- tv[tv$do.qc ==1,c("source_hash","source_table")]

  slist = sort(unique(tv$source))
  res = as.data.frame(matrix(nrow=length(slist),ncol=3))
  names(res) = c("source","records","qc")
  for(i in 1:length(slist)) {
    src = slist[i]
    tv2 = tv[tv$source==src,]
    res[i,"source"] = src
    res[i,"records"] = nrow(tv2)
    res[i,"qc"] = sum(tv2$do.qc)
  }

  return(sampled_records)
}
