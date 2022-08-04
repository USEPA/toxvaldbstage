library("openxlsx")
#--------------------------------------------------------------------------------------
#' create IRIS non cancer clean source file from iris_scrape_rfd_rfc 2020-05-27.xlsx 
#' and iris_scrape_woe 2020-05-27.xlsx (build using iris.scraper.R)
#' @param infile1 The input file ../iris/iris_files/iris_scrape_rfd_rfc 2020-05-27.xlsx
#' @param infile2 The input file ../iris/iris_files/iris_scrape_woe 2020-05-27.xlsx


#--------------------------------------------------------------------------------------
iris.noncancer.clean = function(infile1, infile2){
  printCurrentFunction()

  iris_rfd_rfc <- read.xlsx(infile1,1)
  iris_woe <- read.xlsx(infile2,1)
  iris_woe <- iris_woe[!(iris_woe$WOE.Characterization == "WOE Characterization" & iris_woe$Framework.for.WOE.Characterization == "Framework for WOE Characterization"), ]

  iris_non_cancer_raw <- merge(x = iris_rfd_rfc, y = iris_woe,  by = c("casrn", "name"), all.x =  T)

  route_vals <- grep("Oral|Inhalation",iris_non_cancer_raw$WOE.Characterization)
  iris_non_cancer_raw[route_vals, "exposure_route"] <- gsub(".*\\((.*) route\\)","\\1", iris_non_cancer_raw[route_vals,"WOE.Characterization"])
  # for toxval units mg/m3 , exposure_unit is inhalation
  route_vals2 <- grep("mg/m3",iris_non_cancer_raw$toxval_units)
  iris_non_cancer_raw[route_vals2, "exposure_route"] <- "Inhalation"

  #default oral
  iris_non_cancer_raw$exposure_route[is.na(iris_non_cancer_raw$exposure_route)] <- "Oral"


  risk_vals <- grep("chronic|subchronic",iris_non_cancer_raw$toxval_numeric, value = T, ignore.case = T)
  risk_vals <- which(iris_non_cancer_raw$toxval_numeric %in% risk_vals)
  iris_non_cancer_raw[risk_vals, "risk_assessment_class"] <- gsub(".*\\((.*)\\)","\\1", iris_non_cancer_raw[risk_vals,"toxval_numeric"])
  #default chronic
  iris_non_cancer_raw$risk_assessment_class[is.na(iris_non_cancer_raw$risk_assessment_class)] <- "chronic"



  iris_non_cancer_raw[ , "rfd_numeric"] <- iris_non_cancer_raw$toxval_numeric
  iris_non_cancer_raw[ , "rfd_numeric"] <- gsub("\\s*x 10", "e", iris_non_cancer_raw$rfd_numeric)  
  iris_non_cancer_raw[ , "rfd_numeric"] <- gsub("(.*\\d+)(\\(.*)", "\\1", iris_non_cancer_raw$rfd_numeric)
  iris_non_cancer_raw[ , "rfd_numeric"] <- gsub("(.*\\d+)(\\s*\\\n\\\t.*)", "\\1", iris_non_cancer_raw$rfd_numeric)
  iris_non_cancer_raw$rfd_numeric <-  as.numeric(iris_non_cancer_raw$rfd_numeric)


  iris_non_cancer_raw$pod_type <- gsub("(.*)(\\:.*)", "\\1", iris_non_cancer_raw$PoD)
  iris_non_cancer_raw$pod_type <- gsub("\\s*", "", iris_non_cancer_raw$pod_type)
  iris_non_cancer_raw$pod_type <- gsub(":.*", "", iris_non_cancer_raw$pod_type)

  iris_non_cancer_raw$pod_numeric <- gsub("(.*\\:\\s*)(.*\\d+)(\\s*.*)", "\\2", iris_non_cancer_raw$PoD)
  iris_non_cancer_raw$pod_numeric <- gsub("(.*\\d+)(\\s*\\w+\\/\\w+.*)","\\1",iris_non_cancer_raw$pod_numeric)
  iris_non_cancer_raw$pod_numeric <-gsub("\\s*x\\s*10", "e", iris_non_cancer_raw$pod_numeric)
  iris_non_cancer_raw$pod_numeric <- as.numeric(iris_non_cancer_raw$pod_numeric)
  
  iris_non_cancer_raw$casrn[iris_non_cancer_raw$casrn == ""] <- "-"
  # target column values matches record url values
  #iris_non_cancer_raw$target[!is.na(iris_non_cancer_raw$target)] %in% iris_non_cancer_raw$record_url

  cols2rm <- c("target", "WOE.Characterization","Framework.for.WOE.Characterization","toxval_numeric")
  iris_non_cancer_clean <- iris_non_cancer_raw[,!(names(iris_non_cancer_raw) %in% cols2rm)]
  names(iris_non_cancer_clean)[names(iris_non_cancer_clean) == "Basis"] <- "critical_effect"
  names(iris_non_cancer_clean)[names(iris_non_cancer_clean) == "Composite.UF"] <- "uf_composite"
  names(iris_non_cancer_clean)[names(iris_non_cancer_clean) == "toxval_type"] <- "rfd_type"
  names(iris_non_cancer_clean)[names(iris_non_cancer_clean) == "Confidence"] <- tolower("Confidence")

  write.xlsx(iris_non_cancer_clean, paste0("IRIS_non_cancer_clean ",Sys.Date(),".xlsx"))

}