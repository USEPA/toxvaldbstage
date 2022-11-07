#--------------------------------------------------------------------------------------
#' Load IRIS Source into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile1 The input file ./iris/iris_files/IRIS_non_cancer_clean 2020-05-27.xlsx
#' @param infile2 The input file ./iris/iris_files/IRIS_cancer_clean 2020-05-27.xlsx
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#--------------------------------------------------------------------------------------
import_iris_source <- function(db,
                               infile1="IRIS_non_cancer_clean 2022-10-21.xlsx",
                               infile2="IRIS_cancer_clean 2022-10-21.xlsx",
                               chem.check.halt=T) {
  printCurrentFunction(db)

  infile1 = paste0(toxval.config()$datapath,"iris/iris_files/",infile1)
  infile2 = paste0(toxval.config()$datapath,"iris/iris_files/",infile2)
  #####################################################################
  cat("Build new_iris_noncancer table from infile1 \n")
  #####################################################################
  res1 <- openxlsx::read.xlsx(infile1)
  res1["iris_noncancer_id"] <- c(1:length(res1[,1]))
  res1 <- res1[c('iris_noncancer_id', names(res1[-16]))]
  #runInsertTable(res1,"new_iris_noncancer",db,do.halt=T,verbose=F)

  nlist = c("casrn","name","record_url","System",
            "critical_effect","PoD","uf_composite","confidence","toxval_units",
            "rfd_type","exposure_route","risk_assessment_class","rfd_numeric","pod_type",
            "pod_numeric")
  res1 = res1[,nlist]
  for(i in 1:nrow(res1)) {
    ce = paste0(res1[i,"System"],":",res1[i,"critical_effect"])
    res1[i,"critical_effect"] = ce
  }
  res1 = subset(res1,select=-c(System))

  nlist1 = c("casrn","name","record_url","critical_effect", "uf_composite",
             "confidence","exposure_route","risk_assessment_class",
             "pod_type","pod_numeric","toxval_units")
  res1a = res1[,nlist1]
  nlist2 = c("casrn","name","record_url","critical_effect","uf_composite",
             "confidence","exposure_route","risk_assessment_class",
             "rfd_type","rfd_numeric","toxval_units")
  res1b = res1[,nlist2]
  nlist = c("casrn","name","record_url","critical_effect","uf_composite",
             "confidence","exposure_route","risk_assessment_class",
             "toxval_type","toxval_numeric","toxval_units")
  names(res1a) = nlist
  names(res1b) = nlist

  res_noncancer = rbind(res1a,res1b)
  res_noncancer$extrapolation_method = "-"
  res_noncancer$class = "noncancer"
  #####################################################################
  cat("Build new_iris_cancer table from infile2 \n")
  #####################################################################
  res2 <- openxlsx::read.xlsx(infile2)
  res2["iris_cancer_id"] <- c(1:length(res2[,1]))
  res2 <- res2[c('iris_cancer_id', names(res2[-10]))]
  #runInsertTable(res2,"new_iris_cancer",db,do.halt=T,verbose=F)

  nlist2 = c("name","casrn","exposure_route","critical_effect",
            "extrapolation_method","toxval_type","toxval_numeric","toxval_units" )
  res2 = res2[,nlist2]

  res2$class = "cancer"
  res2$critical_effect = "cancer"
  res2$uf_composite = NA
  res2$risk_assessment_class = "cancer"
  res2$confidence = NA
  res2$record_url = NA

  nlist = names(res_noncancer)
  res_cancer = res2[,nlist]
  for(i in 1:nrow(res_cancer)) {
    casrn <- res_cancer[i,"casrn"]
    if(is.element(casrn,res_noncancer[,"casrn"])) {
      temp <- res_noncancer[is.element(res_noncancer[,"casrn"],casrn),]
      url <- temp[1,"record_url"]
      res_cancer[is.element(res_cancer[,"casrn"],casrn),"record_url"] <- url
    }
  }
  resall = rbind(res_noncancer,res_cancer)
  resall = resall[!is.element(resall$casrn,c("Various","-","")),]
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="IRIS",table="source_iris",res=resall,F,T,T)
}
