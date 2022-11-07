#-----------------------------------------------------------------------------------
#
#' Export samples of the cancer, genetox, and skin/eye data
#'
#' @param toxval.db Database version
#'
#-----------------------------------------------------------------------------------
export.cgse.samples <- function(toxval.db="res_toxval_v93") {
  printCurrentFunction(toxval.db)

  dir = paste0(toxval.config()$datapath,"export/cgse_samples/")
  cat("genetox_details\n")
  res1 = runQuery("select dtxsid,source,assay_category,assay_type,metabolic_activation,species,strain,assay_result,year from genetox_details where dtxsid='DTXSID5020281'",toxval.db)
  file = paste0(dir,"genetox_details_sample.xlsx")
  write.xlsx(res1,file)

  res2 = runQuery("select dtxsid,reports_pos, reports_neg, reports_other, ames, micronucleus from genetox_summary where dtxsid='DTXSID5020281'",toxval.db)
  file = paste0(dir,"genetox_summary_sample.xlsx")
  write.xlsx(res2,file)

  res3 = runQuery("select dtxsid,source,study_type,species,strain,reliability,endpoint,guideline,result_text,classification,score,year from skin_eye where dtxsid='DTXSID5020281'",toxval.db)
  res3[res3$year<0,"year"] = NA
  file = paste0(dir,"skin_eye_sample.xlsx")
  write.xlsx(res3,file)

  res4 = runQuery("select dtxsid,source,exposure_route,cancer_call,url from cancer_summary skin_eye where dtxsid='DTXSID5020281'",toxval.db)
  file = paste0(dir,"cancer_summary.xlsx")
  write.xlsx(res4,file)
}
