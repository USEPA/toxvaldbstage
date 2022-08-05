library('openxlsx')
library('tidyr')
#-------------------------------------------------------------------------------------
#' Load NEW PPRTV (NCEA) to toxval_source. The data is found in a series of files:
#' ../pprtv_ncea/pprtv_ncea_files/assessments.xlsx
#' ../pprtv_ncea/pprtv_ncea_files/assessment_study.xlsx
#' ../pprtv_ncea/pprtv_ncea_files/reference.xlsx
#' ../pprtv_ncea/pprtv_ncea_files/study.xlsx
#' ../pprtv_ncea/pprtv_ncea_files/dose_reg2.csv
#' ../pprtv_ncea/PPRTV_scrape2020-04-08.xlsx
#' @param source.db The version of toxval_source into which the tables are loaded.
#' @export
#-------------------------------------------------------------------------------------
pprtv.ncea.load.all = function(source.db){
  printCurrentFunction(source.db)
  read.xlsx("../pprtv_ncea/pprtv_ncea_files/assessments.xlsx") -> chemicals
  read.xlsx("../pprtv_ncea/pprtv_ncea_files/assessment_study.xlsx") -> relations
  read.xlsx("../pprtv_ncea/pprtv_ncea_files/reference.xlsx") -> results
  read.xlsx("../pprtv_ncea/pprtv_ncea_files/study.xlsx") -> study
  read.csv("../pprtv_ncea/pprtv_ncea_files/dose_reg2.csv", stringsAsFactors = FALSE) -> dosereg
  read.xlsx("../pprtv_ncea/PPRTV_scrape2020-04-08.xlsx") -> scrape
  
  
  
  #Fix the Scrape's Columns
  scrape$Effect.Level = iconv(scrape$Effect.Level,"latin1","ASCII","~")
  separate(scrape,Effect.Level,c("POD_Type","POD_numeric","POD_units"),"~~~~") -> scrape
  scrape$toxval_numeric = iconv(scrape$toxval_numeric,"latin1","ASCII","~")
  
  separate(scrape,toxval_numeric,c("toxval_numeric","toxval_units"),"~~~~") -> scrape
  scrape$Route = gsub("orla","oral", scrape$Route, ignore.case = TRUE)
  scrape$Route = gsub(" -",":", scrape$Route)
  scrape$Route = gsub("Occupational Inhalation study","inhalation", scrape$Route, ignore.case = TRUE)
  scrape$Route = gsub(" study","", scrape$Route)
  scrape$Route = tolower(scrape$Route)
  separate(scrape,Route,c("exposure_route","exposure_method"),":", fill = "right") -> scrape
  
  
  #Fix Dosereg columns
  dosereg$Route_of_Exposure = gsub("\\(","- ", dosereg$Route_of_Exposure)
  dosereg$Route_of_Exposure = gsub(")","", dosereg$Route_of_Exposure)
  dosereg$Route_of_Exposure = tolower(dosereg$Route_of_Exposure)
  separate(dosereg,Route_of_Exposure,c("exposure_route","exposure_method")," - ", fill = "right") -> dosereg
  
  #Fix Results Columns
  results[results$Reference.Value.Type == "Chronic Reference Dose (c-RfD)", 3] = "RfD"
  results[results$Reference.Value.Type == "Sub-chronic Reference Concentration (s-RfC)", 3] = "SRfC"
  results[results$Reference.Value.Type == "Chronic Reference Concentration (c-RfC)", 3] = "RfC"
  results[results$Reference.Value.Type == "Sub-chronic Reference Dose (s-RfD)", 3] = "SRfD"
  
  
  #Merge 
  chemicals = chemicals[,1:4]
  names(chemicals)[2] = "name"
  chemicals$name = tolower(chemicals$name)
  relations = relations[,1:3]
  results = results[,c(1:3,5:13,17:22)]
  
  merge(dosereg, relations) -> dosereg
  merge(dosereg, chemicals) -> dosereg
  merge(chemicals, results) -> results
  
  
  #Find 1:1 relationship between results & scrape
  names(results)[9] = "Study.ID"
  names(results)[2] = "name"
  results$name = tolower(results$name)
  scrape$name = tolower(scrape$name)
  names(results)[6] = "toxval_type"
  merge(results, scrape) -> results_scrape
  merge(results, scrape, all.x = TRUE, all.y = TRUE) -> results_scrape
  View(results_scrape)
}
