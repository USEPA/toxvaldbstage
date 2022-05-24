library('openxlsx')
#--------------------------------------------------------------------------------------
#' Load chiu Source into dev_toxval_source_v3. 
#' Data from the Chiu et al. paper on RfD values
#'
#' @param toxval.db The version of toxval into which the source is loaded.
#' @param infile The input file ./chiu/chiu_files/Full_RfD_databaseQAed-FINAL.xlsx

#--------------------------------------------------------------------------------------
import_chiu_source <- function(toxval.db,infile) {
  printCurrentFunction(toxval.db)
  
  #####################################################################
  cat("Build original_chiu from source file \n")
  #####################################################################
  res0 <- read.xlsx(infile)
  res1 <- res0[,c("strCAS","strName","Source","Type","numValue","strHyperlink","strReference","strUnitsPOD","strCriticalEffect","strDateAssessed",
                  "Species","Strain","strDuration","Duration.type","tblOrgan_strSex","numUF","numUFa","numUFh","numUFs","numUFl","numUFd","numUFother","Route")]
  res2 <- res0[,c("strCAS","strName","Source","POD.type","numPOD","strHyperlink","strReference","strUnitsPOD","strCriticalEffect","strDateAssessed",
                  "Species","Strain","strDuration","Duration.type","tblOrgan_strSex","numUF","numUFa","numUFh","numUFs","numUFl","numUFd","numUFother","Route")]
  name.list <- c("casrn","name","subsource","toxval_type","toxval_numeric","record_url","long_ref","toxval_units","critical_effect","year",
                 "species","strain","study_duration_value","study_duration_units","sex","uncertainty_factor","UFa","UFh","UFs","UFl","UFd","UFother","exposure_route")
  names(res1) <- name.list
  names(res2) <- name.list
  res <- rbind(res1,res2)
  #print(str(res))
  runInsertTable(res,"original_chiu",toxval.db,do.halt=T,verbose=F)
  #####################################################################
  cat("Build new_chiu table from res, year changes made in new_chiu based on toxval_v8 creates errors, hence changed in v9 toxval.load.chiu\n")
  #####################################################################
  
  res[,"toxval_units"] <- "mg/kg-day"
  res$study_type <- "chronic"
  res$risk_assessment_class <- "chronic"
  res$source <- "Chiu"
  res$exposure_method <- "-"
  res$source_url <- "https://doi.org/10.1289/EHP3368"
  res <- res[!is.na(res[,"casrn"]),]
  for(i in 1:nrow(res)) {
    if(contains(res[i,"toxval_type"],"RfD")) res[i,"toxval_type"] <- "RfD"
    if(contains(res[i,"toxval_type"],"MRL")) res[i,"toxval_type"] <- "MRL"
    x <- res[i,"year"]
    if(!is.na(x)) {
      nc <- nchar(x)
      x <- as.numeric(substr(x,nc-3,nc))
      if(is.na(x)) browser()
      res[i,"year"] <- x
    }
    else res[i,"year"] <- -1
    res[i,"study_duration_units"] <- tolower(res[i,"study_duration_units"])
    res[i,"species"] <- tolower(res[i,"species"])
    res[i,"strain"] <- tolower(res[i,"strain"])
    res[i,"sex"] <- tolower(res[i,"sex"])
    if(is.na(res[i,"exposure_route"])) {res[i,"exposure_route"] <- "-"; res[i,"exposure_method"] <- "-"}
    else if(res[i,"exposure_route"]=="Oral - other") {res[i,"exposure_route"] <- "oral"; res[i,"exposure_method"] <- "other"}
    else if(res[i,"exposure_route"]=="Oral diet") {res[i,"exposure_route"] <- "oral"; res[i,"exposure_method"] <- "diet"}
    else if(res[i,"exposure_route"]=="Oral drinking water") {res[i,"exposure_route"] <- "oral"; res[i,"exposure_method"] <- "drinking water"}
    else if(res[i,"exposure_route"]=="Oral gavage") {res[i,"exposure_route"] <- "oral"; res[i,"exposure_method"] <- "gavage"}
    else if(res[i,"exposure_route"]=="Other") {res[i,"exposure_route"] <- "other"; res[i,"exposure_method"] <- "other"}
    res[i,"casrn"] <- fix.casrn(res[i,"casrn"])
  }
  name.list <- c("casrn","name","source","subsource","toxval_type","toxval_numeric","toxval_units",
                 "study_type","risk_assessment_class","record_url","source_url","critical_effect","year",
                 "species","strain","sex","study_duration_value","study_duration_units","exposure_route","exposure_method",
                 "long_ref", "uncertainty_factor")
  
  res$year <- as.numeric(res$year)
  print(dim(res))
  res <- unique(res)
  print(dim(res))
  res["chiu_id"] <- c(1:length(res[,1]))
  res[grep("\\[",res$critical_effect),"critical_effect"] <- gsub("\\[","(",res[grep("\\[",res$critical_effect),"critical_effect"])
  res[grep("\\]",res$critical_effect),"critical_effect"] <- gsub("\\]",")",res[grep("\\]",res$critical_effect),"critical_effect"])
  
  
  runInsertTable(res,"new_chiu",toxval.db,do.halt=T,verbose=F)
  
  #####################################################################
  cat("Build chiu_chemical_information table from res\n")
  #####################################################################
  chemical_information <- res[,c("name","casrn")]
  chemical_information <- unique(chemical_information[,1:2])
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  chemical_information <- chemical_information[c('chemical_id','name','casrn')]
  
  runInsertTable(chemical_information,"chiu_chemical_information",toxval.db,do.halt=T,verbose=F)
  
  
}


  
  
  