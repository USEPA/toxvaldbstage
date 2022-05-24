library("openxlsx")
#--------------------------------------------------------------------------------------
#' Load IRIS Source into dev_toxval_source_v4. 
#' @param toxval.db The version of toxval into which the source is loaded.
#' @param infile1 The input file ./iris/iris_files/IRIS_non_cancer_clean 2020-05-27.xlsx
#' @param infile2 The input file ./iris/iris_files/IRIS_cancer_clean 2020-05-27.xlsx


#--------------------------------------------------------------------------------------

import_iris_source <- function(toxval.db,infile1, infile2) {
  printCurrentFunction(toxval.db)
  
  #####################################################################
  cat("Build new_iris_noncancer table from infile1 \n")
  #####################################################################
  res1 <- read.xlsx(infile1)
  res1["iris_noncancer_id"] <- c(1:length(res1[,1]))
  res1 <- res1[c('iris_noncancer_id', names(res1[-16]))]
  #runInsertTable(res1,"new_iris_noncancer",toxval.db,do.halt=T,verbose=F)
  
  #####################################################################
  cat("Build new_iris_cancer table from infile2 \n")
  #####################################################################
  res2 <- read.xlsx(infile2)
  res2["iris_cancer_id"] <- c(1:length(res2[,1]))
  res2 <- res2[c('iris_cancer_id', names(res2[-10]))]
  #runInsertTable(res2,"new_iris_cancer",toxval.db,do.halt=T,verbose=F)
  
  #####################################################################
  cat("Build iris_noncancer_chemical_information table from res1\n")
  #####################################################################
  chemical_information <- res1[,2:3]
  chemical_information <- unique(chemical_information[,1:2])
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  chemical_information <- chemical_information[c('chemical_id','name','casrn')]
  
  #runInsertTable(chemical_information,"iris_noncancer_chemical_information",toxval.db,do.halt=T,verbose=F)
  
  #####################################################################
  cat("Build iris_cancer_chemical_information table from res2\n")
  #####################################################################
  chemical_information2 <- res2[,2:3]
  chemical_information2 <- unique(chemical_information2[,1:2])
  chemical_information2["chemical_id"] <- c(1:length(chemical_information2[,1]))
  chemical_information2 <- chemical_information2[c('chemical_id','name','casrn')]
  
  #runInsertTable(chemical_information2,"iris_cancer_chemical_information",toxval.db,do.halt=T,verbose=F)
  
  #####################################################################
  cat("Build new_iris table \n")
  #####################################################################
  query <- "select casrn,name,record_url,System,critical_effect,PoD,uf_composite,confidence,
  toxval_units,rfd_type,exposure_route,risk_assessment_class, rfd_numeric, pod_type, pod_numeric
  from new_iris_noncancer" 
  noncancer <- runQuery(query,toxval.db,T,F)
  print(dim(noncancer))
  noncancer <- unique(noncancer)
  print(dim(noncancer))
  temp1 <- noncancer[,c("casrn","name","rfd_type","rfd_numeric","toxval_units","exposure_route","critical_effect","risk_assessment_class","record_url")]
  temp2 <- noncancer[,c("casrn","name","pod_type","pod_numeric","toxval_units","exposure_route","critical_effect","risk_assessment_class","record_url")]
  name.list <- c("casrn","name","toxval_type","toxval_numeric","toxval_units","exposure_route","critical_effect","risk_assessment_class","record_url")
  names(temp1) <- name.list
  names(temp2) <- name.list
  temp1 <- temp1[!is.na(temp1[,"toxval_numeric"]),]
  temp2 <- temp2[!is.na(temp2[,"toxval_numeric"]),]
  res <- rbind(temp1,temp2)
  query <- "select name,casrn,exposure_route,critical_effect,toxicity_value,
  extrapolation_method,toxval_type,toxval_numeric,toxval_units
  from new_iris_cancer" 
  cancer <- runQuery(query,toxval.db,T,F)
  name.list <- c("casrn","name","toxval_type","toxval_numeric","toxval_units","exposure_route","critical_effect")
  cancer <- cancer[,name.list]
  cancer$risk_assessment_class <- "cancer"
  cancer$record_url <- "-"
  
  for(i in 1:nrow(cancer)) {
    casrn <- cancer[i,"casrn"]
    if(is.element(casrn,res[,"casrn"])) {
      temp <- res[is.element(res[,"casrn"],casrn),]
      url <- temp[1,"record_url"]
      cancer[is.element(cancer[,"casrn"],casrn),"record_url"] <- url
    }
  }
  res <- rbind(res,cancer)
  res[,"source_id"] <- c(1:length(res[,1]))
  res <- res[,c('source_id', names(res[-10]))]
  #print(str(res))
  runInsertTable(res,"new_iris",toxval.db,do.halt=T,verbose=F)
  
}
  
  