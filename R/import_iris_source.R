library(openxlsx)
#--------------------------------------------------------------------------------------
#' Load IRIS Source into dev_toxval_source_v4.
#' @param db The version of toxval into which the source is loaded.
#' @param infile1 The input file ./iris/iris_files/IRIS_non_cancer_clean 2020-05-27.xlsx
#' @param infile2 The input file ./iris/iris_files/IRIS_cancer_clean 2020-05-27.xlsx
#--------------------------------------------------------------------------------------
import_iris_source <- function(db,
                               infile1="../iris/iris_files/IRIS_non_cancer_clean 2020-05-27.xlsx",
                               infile2="../iris/iris_files/IRIS_cancer_clean 2020-05-27.xlsx",
                               chem.check.halt=T) {
  printCurrentFunction(db)
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


#
#   #####################################################################
#   cat("Build iris_noncancer_chemical_information table from res1\n")
#   #####################################################################
#   chemical_information <- res1[,2:3]
#   chemical_information <- unique(chemical_information[,1:2])
#   chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
#   chemical_information <- chemical_information[c('chemical_id','name','casrn')]
#   #runInsertTable(chemical_information,"iris_noncancer_chemical_information",db,do.halt=T,verbose=F)
#
#   #####################################################################
#   cat("Build iris_cancer_chemical_information table from res2\n")
#   #####################################################################
#   chemical_information2 <- res2[,2:3]
#   chemical_information2 <- unique(chemical_information2[,1:2])
#   chemical_information2["chemical_id"] <- c(1:length(chemical_information2[,1]))
#   chemical_information2 <- chemical_information2[c('chemical_id','name','casrn')]
#   #runInsertTable(chemical_information2,"iris_cancer_chemical_information",db,do.halt=T,verbose=F)
#
#   browser()
#   #####################################################################
#   cat("Build new_iris table \n")
#   #####################################################################
#   query <- "select casrn,name,record_url,System,critical_effect,PoD,uf_composite,confidence,
#   toxval_units,rfd_type,exposure_route,risk_assessment_class, rfd_numeric, pod_type, pod_numeric
#   from new_iris_noncancer"
#   noncancer <- runQuery(query,db,T,F)
#   print(dim(noncancer))
#   noncancer <- unique(noncancer)
#   print(dim(noncancer))
#   temp1 <- noncancer[,c("casrn","name","rfd_type","rfd_numeric","toxval_units","exposure_route","critical_effect","risk_assessment_class","record_url")]
#   temp2 <- noncancer[,c("casrn","name","pod_type","pod_numeric","toxval_units","exposure_route","critical_effect","risk_assessment_class","record_url")]
#   name.list <- c("casrn","name","toxval_type","toxval_numeric","toxval_units","exposure_route","critical_effect","risk_assessment_class","record_url")
#   names(temp1) <- name.list
#   names(temp2) <- name.list
#   temp1 <- temp1[!is.na(temp1[,"toxval_numeric"]),]
#   temp2 <- temp2[!is.na(temp2[,"toxval_numeric"]),]
#   res <- rbind(temp1,temp2)
#   query <- "select name,casrn,exposure_route,critical_effect,toxicity_value,
#   extrapolation_method,toxval_type,toxval_numeric,toxval_units
#   from new_iris_cancer"
#   cancer <- runQuery(query,db,T,F)
#   name.list <- c("casrn","name","toxval_type","toxval_numeric","toxval_units","exposure_route","critical_effect")
#   cancer <- cancer[,name.list]
#   cancer$risk_assessment_class <- "cancer"
#   cancer$record_url <- "-"
#
#   for(i in 1:nrow(cancer)) {
#     casrn <- cancer[i,"casrn"]
#     if(is.element(casrn,res[,"casrn"])) {
#       temp <- res[is.element(res[,"casrn"],casrn),]
#       url <- temp[1,"record_url"]
#       cancer[is.element(cancer[,"casrn"],casrn),"record_url"] <- url
#     }
#   }
#   res <- rbind(res,cancer)
#   res[,"source_id"] <- c(1:length(res[,1]))
#   res <- res[,c('source_id', names(res[-10]))]
#   #print(str(res))
#
# #   #####################################################################
# #   cat("Do the chemical checking\n")
# #   #####################################################################
# #   source = "IRIS"
# #   res = as.data.frame(res)
# #   res = res[!is.element(res$casrn,c("-","Various")),]
# #   res$clowder_id = "-"
# #   res = fix.non_ascii.v2(res,source)
# #   res = source_chemical.process(db,res,source,chem.check.halt,casrn.col="casrn",name.col="name",verbose=F)
# #
# #   #####################################################################
# #   cat("Set the default values for missing data\n")
# #   #####################################################################
# #   res = source_set_defaults(res,source)
# #
# #   #####################################################################
# #   cat("Set the clowder_id and document name\n")
# #   #####################################################################
# #   res = set_clowder_id(res,source)
# #
# #   #####################################################################
# #   cat("Build the hash key and load the data \n")
# #   #####################################################################
# #   toxval_source.hash.and.load(db,source,"new_iris",F,F,res)
# #   browser()
# #   return(1)
# #
# #   runInsertTable(res,"new_iris",db,do.halt=T,verbose=F)
# #
# # }
# #
