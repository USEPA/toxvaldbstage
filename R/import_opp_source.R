#--------------------------------------------------------------------------------------
#' Load opp Source into dev_toxval_source_v2.
#' @param db The version of toxval into which the source is loaded.
#' @param infile The input file ./opp/opp_files/OPP RfD.xlsx
#--------------------------------------------------------------------------------------
import_opp_source <- function(db,
                              infile="../opp/opp_files/OPP RfD.xlsx",
                              chem.check.halt=T) {
  printCurrentFunction(db)

  #####################################################################
  cat("Build original_opp_table \n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile ,1,colNames = T)
  print(names(res))
  res1 = res[,c("casrn","name","acute.RfD.mg/kg-day","acute.HHBP.sensitive.lifestage")]
  names(res1) = c("casrn","name","toxval_numeric","sensitive_lifestage")
  res1$toxval_type = "RfD"
  res1$toxval_units = "mg/kg-day"
  res1$risk_assessment_class = "acute"

  res2 = res[,c("casrn","name","acute.HHBP.ppm","acute.HHBP.sensitive.lifestage")]
  names(res2) = c("casrn","name","toxval_numeric","sensitive_lifestage")
  res2$toxval_type = "HHBP"
  res2$toxval_units = "ppm"
  res2$risk_assessment_class = "acute"

  res3 = res[,c("casrn","name","chronic.RfD.mg/kg-day","chronic.HHBP.sensitive.lifestage")]
  names(res3) = c("casrn","name","toxval_numeric","sensitive_lifestage")
  res3$toxval_type = "RfD"
  res3$toxval_units = "mg/kg-day"
  res3$risk_assessment_class = "chronic"

  res4 = res[,c("casrn","name","chronic.HHBP.ppm","chronic.HHBP.sensitive.lifestage")]
  names(res4) = c("casrn","name","toxval_numeric","sensitive_lifestage")
  res4$toxval_type = "HHBP"
  res4$toxval_units = "ppm"
  res4$risk_assessment_class = "chronic"

  res5 = res[,c("casrn","name","cancer.slope.factor.(mg/kg-day)-1")]
  names(res5) = c("casrn","name","toxval_numeric")
  res5$sensitive_lifestage = "-"
  res5$toxval_type = "cancer slope factor"
  res5$toxval_units = "(mg/kg-day)-1"
  res5$risk_assessment_class = "cancer"

  res6 = res[,c("casrn","name","cancer.slope.factor.(mg/kg-day)-1","carcinogenic.HHBP.ppb" )]
  names(res6) = c("casrn","name","toxval_numeric")
  res6$sensitive_lifestage = "-"
  res6$toxval_type = "carcinogenic.HHBP"
  res6$toxval_units = "ppb"
  res6$risk_assessment_class = "cancer"

  nlist = c("casrn","name","toxval_type","toxval_numeric","toxval_units","risk_assessment_class","sensitive_lifestage")
  resall = rbind(res1[,nlist],res2[,nlist],res3[,nlist],res4[,nlist],res5[,nlist],res6[,nlist])
  resall = resall[!is.na(resall$toxval_numeric),]
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="EPA OPP",table="source_opp",res=resall,F,T,T)
}
