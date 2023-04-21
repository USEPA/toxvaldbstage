#--------------------------------------------------------------------------------------
#' @description Load OPP Source into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile The input file ./opp/opp_files/OPP RfD.xlsx
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#' @title FUNCTION_TITLE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[openxlsx]{read.xlsx}}
#'  \code{\link[dplyr]{arrange}}, \code{\link[dplyr]{mutate}}
#'  \code{\link[stringr]{str_trim}}
#' @rdname import_opp_source
#' @export 
#' @importFrom openxlsx read.xlsx
#' @importFrom dplyr arrange mutate
#' @importFrom stringr str_squish
#--------------------------------------------------------------------------------------
import_opp_source <- function(db,
                              infile="source_opp_raw_20230131.xlsx",
                              chem.check.halt=T) {
  printCurrentFunction(db)
  infile = paste0(toxval.config()$datapath,"opp/opp_files/",infile)
  #####################################################################
  cat("Build original_opp_table \n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile ,1,colNames = T)
  print(names(res))
  res1 = res[,c("CAS.Number","Common.Name.and.Reference.Document",
                "Acute.or.One.Day.PAD.(RfD).(mg/kg/day)", "Acute.HHBP.Sensitive.Lifestage/.Population")]
  names(res1) = c("casrn","name","toxval_numeric", "sensitive_lifestage")
  res1$toxval_type = "RfD"
  res1$toxval_units = "mg/kg/day"
  res1$risk_assessment_class = "acute"

  res2 = res[,c("CAS.Number","Common.Name.and.Reference.Document",
                "Acute.or.One.Day.HHBPs.(ppb)","Acute.HHBP.Sensitive.Lifestage/.Population")]
  names(res2) = c("casrn","name","toxval_numeric","sensitive_lifestage")
  res2$toxval_type = "HHBP"
  res2$toxval_units = "ppb"
  res2$risk_assessment_class = "acute"

  res3 = res[,c("CAS.Number","Common.Name.and.Reference.Document",
                "Chronic.or.Lifetime.PAD.(rfD).(mg/kg/day)","Chronic.HHBP.Sensitive.Lifestage/Population")]
  names(res3) = c("casrn","name","toxval_numeric","sensitive_lifestage")
  res3$toxval_type = "RfD"
  res3$toxval_units = "mg/kg/day"
  res3$risk_assessment_class = "chronic"

  res4 = res[,c("CAS.Number","Common.Name.and.Reference.Document",
                "Chronic.or.Lifetime.HHBPs.(ppb)","Chronic.HHBP.Sensitive.Lifestage/Population")]
  names(res4) = c("casrn","name","toxval_numeric","sensitive_lifestage")
  res4$toxval_type = "HHBP"
  res4$toxval_units = "ppb"
  res4$risk_assessment_class = "chronic"

  res5 = res[,c("CAS.Number","Common.Name.and.Reference.Document",
                "Cancer.Quantification.(Q1).Values.(CSF).(mg/kg/per.day).-1")]
  names(res5) = c("casrn","name","toxval_numeric")
  res5$sensitive_lifestage = "--"
  res5$toxval_type = "cancer slope factor"
  res5$toxval_units = "(mg/kg/perday)-1"
  res5$risk_assessment_class = "cancer"

  res6 = res[,c("CAS.Number","Common.Name.and.Reference.Document","Carcinogenic.HHBP.(E-6.to.E-4.).(ppb)" )]
  names(res6) = c("casrn","name","toxval_numeric")
  res6$sensitive_lifestage = "--"
  res6$toxval_type = "carcinogenic.HHBP"
  res6$toxval_units = "ppb"
  res6$risk_assessment_class = "cancer"

  nlist = c("casrn","name","toxval_type","toxval_numeric","toxval_units","risk_assessment_class","sensitive_lifestage")
  resall = rbind(res1[,nlist],res2[,nlist],res3[,nlist],res4[,nlist],res5[,nlist],res6[,nlist]) %>%
    dplyr::arrange(name)
  resall$toxval_numeric[resall$toxval_numeric %in% c("--", "-")] = NA
  resall$sensitive_lifestage[resall$sensitive_lifestage %in% c("--", "-")] = NA


  resall = resall %>%
    # Fix name encoding error
    dplyr::mutate(name = gsub("&amp;", "&", name),
           # Fix excess whitespace
           casrn = stringr::str_squish(casrn))
  # resall = resall[!is.na(resall$toxval_numeric),]
  resall$url = unique(res$url)
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="EPA OPP",table="source_opp",res=resall,F,T,T)
}
