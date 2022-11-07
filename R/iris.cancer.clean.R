library("openxlsx")
library('XML')
library('stringi')
#--------------------------------------------------------------------------------------
#' create IRIS cancer clean source file from https://cfpub.epa.gov/ncea/iris/search/index.cfm
#' by selecting cancer, oral and inhalation data, and toxicity value checkboxes.
#' @param infile The input file ../iris/iris_files/excelView.xls

#--------------------------------------------------------------------------------------
iris.cancer.clean = function(sys.date="2022-10-21"){
  printCurrentFunction()
  infile = paste0(toxval.config()$datapath,"iris/iris_files/IRIS cancer ",sys.date,".xlsx")
  iris_cancer_raw <- read.xlsx(infile)
  names(iris_cancer_raw) = iris_cancer_raw[1,]
  iris_cancer_raw = iris_cancer_raw[2:nrow(iris_cancer_raw),]
  #iris_cancer_raw <- readHTMLTable(infile , stringsAsFactors = FALSE)
  #iris_cancer_raw <- iris_cancer_raw[[1]]
  iris_cancer_raw <-  data.frame(iris_cancer_raw, stringsAsFactors = F)
  #write.xlsx(iris_cancer_raw, file = "iris_cancer_raw.xlsx")

  iris_cancer_raw[iris_cancer_raw$Exposure.Route == "Oral","toxval_type"] <- "oral slope factor"
  iris_cancer_raw[iris_cancer_raw$Exposure.Route == "Inhalation","toxval_type"] <- "inhalation unit risk"
  iris_cancer_raw$toxval_numeric <- sub("\\s.*","",iris_cancer_raw$Toxicity.Value)
  iris_cancer_raw$toxval_units <- sub("^\\S*\\s+(\\S+.*)", "\\1", iris_cancer_raw$Toxicity.Value)

  iris_cancer_raw$toxval_units <- gsub("Âµ", "u", iris_cancer_raw$toxval_units)
  iris_cancer_raw$toxval_units <- gsub("\\(.*\\)", "", iris_cancer_raw$toxval_units)
  iris_cancer_raw$toxval_units <- gsub("\\s+$", "", iris_cancer_raw$toxval_units)

  iris_cancer_raw[grep("per", iris_cancer_raw$toxval_units),"toxval_units"] <- paste("(", iris_cancer_raw[grep("per", iris_cancer_raw$toxval_units),"toxval_units"], ")-1", sep = "")
  iris_cancer_raw$toxval_units <- gsub("per\\s+", "", iris_cancer_raw$toxval_units)

  names(iris_cancer_raw) <- c("name", "casrn", "exposure_route", "critical_effect","toxicity_value", "extrapolation_method", "toxval_type", "toxval_numeric","toxval_units")

  iris_cancer_raw[grep("x", iris_cancer_raw$toxval_numeric),"toxval_numeric"] <- gsub("(^.*)(x10)(.*)","\\1\\e\\3", iris_cancer_raw[grep("x", iris_cancer_raw$toxval_numeric),"toxval_numeric"])

  iris_cancer_raw$toxval_numeric <-as.numeric(iris_cancer_raw$toxval_numeric)

  file = paste0(toxval.config()$datapath,"iris/iris_files/IRIS_cancer_clean ",sys.date,".xlsx")
  write.xlsx(iris_cancer_raw, file)
  #write.xlsx(iris_cancer_raw, paste0("IRIS_cancer_clean ",sys.date,".xlsx"))
}
