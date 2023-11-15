#--------------------------------------------------------------------------------------
#' @description Load DOE LANL ECORISK Source into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile The input file ./doe_lanl_ecorisk/doe_lanl_ecorisk_files/ESLs_R3.3.xlsx
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
#' @rdname import_doe_lanl_ecorisk_source
#' @export
#' @importFrom openxlsx read.xlsx

#--------------------------------------------------------------------------------------
import_doe_lanl_ecorisk_source <- function(db,
                               infile="ESLs_R3.3.xlsx",
                               chem.check.halt=T)  {
  printCurrentFunction(db)
  infile = paste0(toxval.config()$datapath,"doe_lanl_ecorisk/doe_lanl_ecorisk_files/",infile)
  #####################################################################
  cat("Build original_doe_lanl_ecorisk_table \n")
  #####################################################################
  res <- openxlsx::read.xlsx( infile ,1,colNames = T)

  bad.codes = c("AL","SB","AS","BA","BE",
                "B","CD","CL(-1)","CR","CR(+6)","CO","CU",
                "CN(-1)","F(-1)","FE","PB","LI","MN","HGI",
                "HGM","MO","NI","ClO4(-1)","SE","AG","SR",
                "TL","TI","U","V","ZN",
                "AM-241","CS-134","CS-137/ BA-137","CO-60","EU-152",
                "PB-210","NP-237","PU-238","PU-239/240","PU-241","RA-226","RA-228",
                "NA-22","SR-90/ Y-90","TH-228","TH-229","TH-230","TH-232","H-3",
                "U-233","U-234","U-235","U-236","U-238")
  res = res[!generics::is.element(res$Analyte.Code,bad.codes),]

  nlist = c("Analyte.Category","Analyte.Group","Analyte.Name","Analyte.Code","ESL.Medium","ESL.Receptor","No.Effect.ESL",
            "Low.Effect.ESL","Units","Minimum.ESL","ESL.ID" )
  nlist = c("category","group","name","casrn","medium","species","noesl","loesl","toxval_units","minimum_esl","esl_id" )
  names(res) = nlist
  res1 = res[,c("category","group","name","casrn","medium","species","noesl","toxval_units","esl_id" )]
  res2 = res[,c("category","group","name","casrn","medium","species","loesl","toxval_units","esl_id" )]
  res1$toxval_type = "No Effect ESL"
  res2$toxval_type = "Low Effect ESL"
  nlist = c("category","group","name","casrn","medium","species","toxval_numeric","toxval_units","esl_id","toxval_type")
  names(res1) = nlist
  names(res2) = nlist
  resall = rbind(res1,res2)
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="DOE LANL ECORISK",table="source_doe_lanl_ecorisk",res=resall,F,T,T)
}
