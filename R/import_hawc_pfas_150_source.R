#--------------------------------------------------------------------------------------
#' Load HAWC PFAS 150 Source into dev_toxval_source_v4.
#' @param db The version of toxval into which the source is loaded.
#' @param infile1 The input file ./hawc_pfas/hawc_pfas_files/hawc_pfas_150_raw3.xlsx , extracted
#' from https://hawcprd.epa.gov , assessment name - PFAS 150 (2021) and assessment id - 100500085.
#' Data extraction using HawcClient and extraction script hawc_pfas_150.py
#' @param infile2 The input file ./hawc_pfas/hawc_pfas_files/hawc_pfas_150_doses3.xlsx
#' @param infile3 The input file ./hawc_pfas/hawc_pfas_files/hawc_pfas_150_groups3.xlsx
#--------------------------------------------------------------------------------------
import_hawc_pfas_150_source <- function(db,
                                        infile1="../hawc_pfas/hawc_pfas_files/hawc_pfas_150_raw3.xlsx",
                                        infile2="../hawc_pfas/hawc_pfas_files/hawc_pfas_150_doses3.xlsx",
                                        infile3="../hawc_pfas/hawc_pfas_files/hawc_pfas_150_groups3.xlsx",
                                        chem.check.halt=T) {
  printCurrentFunction(db)

  #####################################################################
  cat("Build original_hawc_pfas_150 table from source file  \n")
  #####################################################################
  res_pfas3 <- openxlsx::read.xlsx(infile1)
  res_pfas3 <- lapply(res_pfas3, function(x) type.convert(as.character(x), as.is = T))
  res_pfas3 <- data.frame(res_pfas3, stringsAsFactors = F)
  dim(res_pfas3)

  #####################################################################
  cat("All closed empty square bracket entries to - in effects\n")
  #####################################################################
  res_pfas3$effects <- gsub("^\\[\\]$","-",res_pfas3$effects)

  #####################################################################
  cat("Strip square brackets from beginning and end\n")
  #####################################################################
  res_pfas3$animal_group.experiment.study.searches <- gsub("(^\\[)(\\d+)(\\]$)","\\2",res_pfas3$animal_group.experiment.study.searches)
  res_pfas3$animal_group.experiment.study.identifiers <- gsub("(^\\[)(.*)(\\]$)","\\2",res_pfas3$animal_group.experiment.study.identifiers)

  #####################################################################
  cat("All na columns to character type\n")
  #####################################################################
  for (i in 1:ncol(res_pfas3[, colSums(is.na(res_pfas3)) == nrow(res_pfas3)])){
    res_pfas3[,names(res_pfas3[, colSums(is.na(res_pfas3)) == nrow(res_pfas3)][i])] <- as.character(res_pfas3[,names(res_pfas3[, colSums(is.na(res_pfas3)) == nrow(res_pfas3)][i])])
  }
  #variations in NA to NA
  res_pfas3$bmd_notes <- gsub("[NA\\,\\\n]+",NA,res_pfas3$bmd_notes)
  res_pfas3$confidence_interval <- gsub("[NA\\,\\\n]+",NA,res_pfas3$confidence_interval)
  res_pfas3$animal_group.experiment.study.block_id <- gsub("[NA\\,\\\n]+",NA,res_pfas3$animal_group.experiment.study.block_id)
  # strip begining and ending quotation marks
  res_pfas3 <- as.data.frame(sapply(res_pfas3, function(x) gsub("\"", "", x)))
  #res_dose3 <- openxlsx::read.xlsx(infile2,sheetIndex = 1, encoding="UTF-8")
  res_dose3 <- openxlsx::read.xlsx(infile2)
  res_dose3 <- lapply(res_dose3, function(x) type.convert(as.character(x), as.is = T))
  res_dose3 <- data.frame(res_dose3, stringsAsFactors = F)
  print(dim(res_dose3))
  #res_groups3 <- openxlsx::read.xlsx(infile3,sheetIndex = 1, encoding="UTF-8")
  res_groups3 <- openxlsx::read.xlsx(infile3)
  res_groups3 <- lapply(res_groups3, function(x) type.convert(as.character(x), as.is = T))
  res_groups3 <- data.frame(res_groups3, stringsAsFactors = F)
  print(dim(res_groups3))

  names.list <- c("assessment","name" ,"organ","NOEL","LOEL",
                  "FEL","url","bmd","animal_group.experiment.study.id","animal_group.experiment.study.title" ,"animal_group.experiment.study.authors_short",
                  "animal_group.experiment.study.authors","animal_group.experiment.study.year","animal_group.experiment.study.journal",
                  "animal_group.experiment.study.full_text_url","animal_group.experiment.study.short_citation","animal_group.experiment.study.full_citation",
                  "animal_group.experiment.study.url","animal_group.experiment.name","animal_group.experiment.type",
                  "animal_group.experiment.chemical","animal_group.experiment.cas","animal_group.experiment.chemical_source",
                  "animal_group.experiment.vehicle","animal_group.experiment.guideline_compliance",
                  "animal_group.dosing_regime.id","animal_group.dosing_regime.route_of_exposure",
                  "animal_group.dosing_regime.duration_exposure","animal_group.dosing_regime.duration_exposure_text",
                  "animal_group.species","animal_group.strain" ,"animal_group.sex","animal_group.name","animal_group.generation",
                  "noel_names.noel","noel_names.loel")
  hawc_pfas_150 <- res_pfas3[,names.list]

  #####################################################################
  cat("map noel,loel, fel values and units from dose dictionary\n")
  #####################################################################
  dose_dict <- res_dose3[,c("dose_regime","dose_group_id","dose","dose_units.name")]
  hawc_pfas_150$NOEL_values <- dose_dict[match(paste(hawc_pfas_150$animal_group.dosing_regime.id,hawc_pfas_150$NOEL),paste(dose_dict$dose_regime,dose_dict$dose_group_id)),"dose"]
  hawc_pfas_150$NOEL_units <-  dose_dict[match(paste(hawc_pfas_150$animal_group.dosing_regime.id,hawc_pfas_150$NOEL),paste(dose_dict$dose_regime,dose_dict$dose_group_id)),"dose_units.name"]
  hawc_pfas_150$LOEL_values <- dose_dict[match(paste(hawc_pfas_150$animal_group.dosing_regime.id,hawc_pfas_150$LOEL),paste(dose_dict$dose_regime,dose_dict$dose_group_id)),"dose"]
  hawc_pfas_150$LOEL_units <-  dose_dict[match(paste(hawc_pfas_150$animal_group.dosing_regime.id,hawc_pfas_150$LOEL),paste(dose_dict$dose_regime,dose_dict$dose_group_id)),"dose_units.name"]

  dose_dict2 <- unique(dose_dict[,c("dose_regime","dose")])
  doses<- aggregate(dose ~ dose_regime, data = dose_dict2, toString)
  hawc_pfas_150$doses <-  doses[match(hawc_pfas_150$animal_group.dosing_regime.id,doses$dose_regime),"dose"]

  #hawc_pfas_150$study_url <-  paste("https://hawcprd.epa.gov",hawc_pfas_150$animal_group.experiment.study.url, sep = "")
  hawc_pfas_150$endpoint_url <-  paste("https://hawcprd.epa.gov",hawc_pfas_150$url, sep = "")
  hawc_pfas_150$assessment_url <-  paste("https://hawcprd.epa.gov/assessment/",hawc_pfas_150$assessment,"/", sep = "")
  hawc_pfas_150$FEL_values <- dose_dict[match(paste(hawc_pfas_150$animal_group.dosing_regime.id,hawc_pfas_150$FEL),paste(dose_dict$dose_regime,dose_dict$dose_group_id)),"dose"]
  hawc_pfas_150$FEL_units <- dose_dict[match(paste(hawc_pfas_150$animal_group.dosing_regime.id,hawc_pfas_150$FEL),paste(dose_dict$dose_regime,dose_dict$dose_group_id)),"dose_units.name"]
  fac_cols <- sapply(hawc_pfas_150, is.factor)                          # Identify all factor columns
  hawc_pfas_150[fac_cols] <- lapply(hawc_pfas_150[fac_cols], as.character)  # Convert all factors to characters
  names.list <- c("assessment","critical_effect","target","NOEL_original","LOEL_original",
                  "FEL_original","endpoint_url_original","bmd","study_id","title","authors_short","author","year","journal",
                  "full_text_url","short_ref","long_ref","study_url_original","experiment_name","experiment_type",
                  "name","casrn","chemical_source","media","guideline_compliance",
                  "dosing_regime_id","route_of_exposure","exposure_duration_value",
                  "exposure_duration_text","species","strain","sex","population","generation","noel_names","loel_names",
                  "NOEL_values","NOEL_units","LOEL_values",
                  "LOEL_units","doses","record_url","source_url","FEL_values","FEL_units")

  names(hawc_pfas_150) <- names.list
  hawc_pfas_150_final <- hawc_pfas_150[,names.list]
  hawc_pfas_150_final <-  hawc_pfas_150_final[ , !(names(hawc_pfas_150_final) %in% "assessment")]
  hawc_pfas_150_final$exposure_route <- hawc_pfas_150_final$route_of_exposure
  hawc_pfas_150_final$exposure_method <- hawc_pfas_150_final$route_of_exposure
  hawc_pfas_150_final$study_duration_value <- hawc_pfas_150_final$exposure_duration_text
  hawc_pfas_150_final$study_duration_units <- hawc_pfas_150_final$exposure_duration_text
  names(hawc_pfas_150_final)[match("exposure_duration_value",names(hawc_pfas_150_final))] <- "exposure_duration_value_original"
  names(hawc_pfas_150_final)[match("experiment_type",names(hawc_pfas_150_final))] <- "study_type_original"
  hawc_pfas_150_final$study_type <- hawc_pfas_150_final$study_type_original
  hawc_pfas_150_final$fel_names <- "FEL"
  hawc_pfas_150_final$source <- "HAWC PFAS 150"
  hawc_pfas_150_final$subsource <- "PFAS 150 (2020)"
  h1 <- hawc_pfas_150_final[,c(1:33,35,38:39,40:42,45:49,51:52)]
  h2 <- hawc_pfas_150_final[,c(1:33,34,36:37,40:42,45:49,51:52)]
  h3 <- hawc_pfas_150_final[,c(1:33,50,43:44,40:42,45:49,51:52)]

  names(h1)[34] <- "toxval_type"
  names(h1)[35] <- "toxval_numeric"
  names(h1)[36] <- "toxval_units"
  names(h2)[34] <- "toxval_type"
  names(h2)[35] <- "toxval_numeric"
  names(h2)[36] <- "toxval_units"
  names(h3)[34] <- "toxval_type"
  names(h3)[35] <- "toxval_numeric"
  names(h3)[36] <- "toxval_units"

  hawc_pfas_150_final <- rbind(h1,h2,h3)
  rownames(hawc_pfas_150_final) <- c()

  print(dim(hawc_pfas_150_final))
  hawc_pfas_150_final <- hawc_pfas_150_final[which(hawc_pfas_150_final$toxval_numeric != '-999'),]
  print(dim(hawc_pfas_150_final))

  #####################################################################
  cat("Do the chemical checking\n")
  #####################################################################
  source = "HAWC PFAS 150"
  res = as.data.frame(hawc_pfas_150_final)
  res = fix.non_ascii.v2(res,source)
  res = source_chemical.process(db,res,source,chem.check.halt,casrn.col="casrn",name.col="name",verbose=F)

  #####################################################################
  cat("Set the default values for missing data\n")
  #####################################################################
  res = source_set_defaults(res,source)

  #####################################################################
  cat("Set the clowder_id and document name\n")
  #####################################################################
  res = set_clowder_id(res,source)

  #####################################################################
  cat("Build the hash key and load the data \n")
  #####################################################################
  toxval_source.hash.and.load(db,source,"original_hawc_pfas_150",F,F,res)
  browser()
  return(1)



  runInsertTable(hawc_pfas_150_final,"original_hawc_pfas_150",db,do.halt=T,verbose=F)

  #####################################################################
  cat("Build new_hawc_pfas table \n")
  #####################################################################
  hawc_pfas_150_final$casrn <-  gsub("([a-zA-Z]+\\s+[a-zA-Z]*\\:*\\s*)(.*)","\\2",hawc_pfas_150_final$casrn)

  hawc_pfas_150_final$exposure_route <- gsub("(^[a-zA-Z]+)(\\s*.*)","\\1", hawc_pfas_150_final$route_of_exposure)
  hawc_pfas_150_final$exposure_method <- gsub("(^[a-zA-Z]+\\s*)(.*)","\\2", hawc_pfas_150_final$route_of_exposure)

  hawc_pfas_150_final$exposure_method <- gsub("^\\-\\s+","", hawc_pfas_150_final$exposure_method)

  hawc_pfas_150_final$study_duration_value <- gsub("(^\\d+)(\\s+.*)","\\1",hawc_pfas_150_final$exposure_duration_text)
  hawc_pfas_150_final$study_duration_value <- gsub("(^\\d+)(.*)","\\1",hawc_pfas_150_final$study_duration_value)
  range_vals <- grep("\\-", hawc_pfas_150_final$study_duration_value)

  hawc_pfas_150_final[range_vals,"study_duration_value"] <- hawc_pfas_150_final[range_vals,"exposure_duration_value_original"]

  hawc_pfas_150_final$study_duration_units <- gsub("(^GD)(\\s+.*)","\\1",hawc_pfas_150_final$exposure_duration_text)
  hawc_pfas_150_final$study_duration_units <- gsub("(^\\d+\\s+)(\\w+)(\\s*.*)","\\2",hawc_pfas_150_final$study_duration_units)
  hawc_pfas_150_final$study_duration_units <- gsub("(.*)(\\d+\\s+)(\\w+)(\\s*.*)","\\3",hawc_pfas_150_final$study_duration_units)
  hawc_pfas_150_final$study_duration_units <- gsub("(\\d+\\s*)(\\w+)(\\s*.*)","\\2",hawc_pfas_150_final$study_duration_units)

  hawc_pfas_150_final[is.element(hawc_pfas_150_final$study_duration_units,"d"),"study_duration_units"] <- "day"
  hawc_pfas_150_final[is.element(hawc_pfas_150_final$study_duration_units,"GD"),"study_duration_units"] <- "day"
  hawc_pfas_150_final[is.element(hawc_pfas_150_final$study_duration_units,"wk"),"study_duration_units"] <- "week"
  hawc_pfas_150_final[is.element(hawc_pfas_150_final$study_duration_units,"yr"),"study_duration_units"] <- "year"

  hawc_pfas_150_final$study_type <- gsub("(^\\w+\\-*\\w*)(\\s*.*)","\\1",hawc_pfas_150_final$study_type_original)

  hawc_pfas_150_final$study_duration_value <- as.numeric(hawc_pfas_150_final$study_duration_value)


  new_res2 <- hawc_pfas_150_final

  # assign appropriate data types
  new_res2 <- lapply(new_res2, function(x) type.convert(as.character(x), as.is = T))
  new_res2 <- data.frame(new_res2, stringsAsFactors = F)




  # convert na and empty values in character columns into hyphens
  for (i in 1:ncol(new_res2)) {
    if (class(new_res2[,i]) == "character") {
      new_res2[which(is.na(new_res2[i])), names(new_res2)[i]] <- "-"
    }
  }

  new_res2$bmd <- NA

  new_res2[,which(sapply(new_res2, function(x)all(is.na(x)))) ] <- as.character(new_res2[,which(sapply(new_res2, function(x)all(is.na(x)))) ])


  #print(str(new_res2))

  #runInsertTable(new_res2,"new_hawc_pfas_150",db,do.halt=T,verbose=F)


  #####################################################################
  cat("Build hawc_pfas_150_chemical_information table from new_res2\n")
  #####################################################################

  chemical_information <- new_res2[,c("name","casrn")]
  chemical_information <- unique(chemical_information[,1:2])
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  chemical_information <- chemical_information[c('chemical_id','name','casrn')]

  #runInsertTable(chemical_information,"hawc_pfas_150_chemical_information",db,do.halt=T,verbose=F)


}

