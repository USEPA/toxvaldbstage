library("openxlsx")
library('tidyr')
#--------------------------------------------------------------------------------------
#' Load HAWC Source into dev_toxval_source_v3. 
#' @param toxval.db The version of toxval into which the source is loaded.
#' @param infile1 The input file ./hawc/hawc_files/hawc_original_12_06_21.xlsx
#' @param infile2 The input file ./hawc/hawc_files/dose_dict.xlsx


#--------------------------------------------------------------------------------------
import_hawc_source <- function(toxval.db,infile1, infile2) {
  printCurrentFunction(toxval.db)
  
  #####################################################################
  cat("Build original_hawc table \n")
  #####################################################################
  sheets1 <- openxlsx::getSheetNames(infile1)
  hawc_dfs <- lapply(sheets1, openxlsx::read.xlsx, xlsxFile = infile1)
  # subsetting toxval specific variables from 100's of variables
  hawc_cols <- c("assessment","groups","name" ,"organ","NOEL","LOEL",
                 "FEL","url","animal_group.experiment.study.id","animal_group.experiment.study.title" ,"animal_group.experiment.study.authors_short",
                 "animal_group.experiment.study.authors","animal_group.experiment.study.year","animal_group.experiment.study.journal",
                 "animal_group.experiment.study.full_text_url","animal_group.experiment.study.short_citation","animal_group.experiment.study.full_citation",
                 "animal_group.experiment.study.url","animal_group.experiment.name","animal_group.experiment.type",
                 "animal_group.experiment.chemical","animal_group.experiment.cas","animal_group.experiment.chemical_source",
                 "animal_group.experiment.vehicle","animal_group.experiment.guideline_compliance",
                 "animal_group.dosing_regime.id","animal_group.dosing_regime.route_of_exposure",
                 "animal_group.dosing_regime.duration_exposure","animal_group.dosing_regime.duration_exposure_text",
                 "animal_group.species","animal_group.strain" ,"animal_group.sex","animal_group.name","animal_group.generation",
                 "noel_names.noel","noel_names.loel")
  
  new_hawc <- lapply(hawc_dfs, "[", hawc_cols)
  new_hawc_df <- do.call("rbind", new_hawc)
  #dim(new_hawc_df)
  # original with only a subset of variables
  #runInsertTable(new_hawc_df,"hawc_original",toxval.db,do.halt=T,verbose=F)
  #print(str(new_hawc_df))
  
  #####################################################################
  cat("read in the dose dictionary \n")
  #####################################################################
  s <- read.xlsx(infile2)
  #runInsertTable(s,"hawc_dose_dictionary",toxval.db,do.halt=T,verbose=F)
  #print(str(s))
  
  #####################################################################
  cat("map hawc original with dose dictionary \n")
  #####################################################################
  new_hawc_df$NOEL_values <- s[match(paste(new_hawc_df$animal_group.dosing_regime.id,new_hawc_df$NOEL),paste(s$dose_regime,s$dose_group_id)),"dose"]
  new_hawc_df$NOEL_units <-  s[match(paste(new_hawc_df$animal_group.dosing_regime.id,new_hawc_df$NOEL),paste(s$dose_regime,s$dose_group_id)),"name"]
  new_hawc_df$LOEL_values <- s[match(paste(new_hawc_df$animal_group.dosing_regime.id,new_hawc_df$LOEL),paste(s$dose_regime,s$dose_group_id)),"dose"]
  new_hawc_df$LOEL_units <-  s[match(paste(new_hawc_df$animal_group.dosing_regime.id,new_hawc_df$LOEL),paste(s$dose_regime,s$dose_group_id)),"name"]
  new_hawc_df$FEL_values <- s[match(paste(new_hawc_df$animal_group.dosing_regime.id,new_hawc_df$FEL),paste(s$dose_regime,s$dose_group_id)),"dose"]
  new_hawc_df$FEL_units <-  s[match(paste(new_hawc_df$animal_group.dosing_regime.id,new_hawc_df$FEL),paste(s$dose_regime,s$dose_group_id)),"name"]
  
  
  s_new <- unique(s[,c("dose_regime","dose")])
  doses<- aggregate(dose ~ dose_regime, data = s_new, toString)
  new_hawc_df$doses <-  doses[match(new_hawc_df$animal_group.dosing_regime.id,doses$dose_regime),"dose"]
  
  fac_cols <- sapply(new_hawc_df, is.factor)                          # Identify all factor columns
  new_hawc_df[fac_cols] <- lapply(new_hawc_df[fac_cols], as.character)  # Convert all factors to characters
  
  
  new_hawc_df$endpoint_url <-  paste("https://hawcproject.org",new_hawc_df$url, sep = "")
  new_hawc_df$study_url <-  paste("https://hawcproject.org",new_hawc_df$animal_group.experiment.study.url, sep = "")
  new_hawc_df$assessment_url <-  paste("https://hawcproject.org/assessment/",new_hawc_df$assessment,"/", sep = "")
  
  # remove dose dict expressed as groups variable
  
  new_hawc_df <- new_hawc_df[,names(new_hawc_df)[names(new_hawc_df) != "groups"]]
  
  names.list <- c("assessment","critical_effect","target","NOEL_original","LOEL_original",
                  "FEL_original","endpoint_url_original","study_id","title","authors_short","author","year","journal",
                  "full_text_url","short_ref","long_ref","study_url_original","experiment_name","experiment_type",
                  "name","casrn","chemical_source","media","guideline_compliance",
                  "dosing_regime_id","route_of_exposure","exposure_duration_value",
                  "exposure_duration_text","species","strain","sex","population","generation","noel_names","loel_names",
                  "NOEL_values","NOEL_units","LOEL_values",
                  "LOEL_units","FEL_values","FEL_units","doses","endpoint_url","study_url","source_url")
  
  names(new_hawc_df) <- names.list
  new_hawc_df$fel_names <- "FEL"
  
  # entire full_text_url field is empty, hence assigning as hyphen to maintain character type
  new_hawc_df[which(is.na(new_hawc_df$full_text_url)),"full_text_url"] <- "-"
  
  h1 <- new_hawc_df[,c(1:33,34,36,37,42:45)]
  h2 <- new_hawc_df[,c(1:33,35,38,39,42:45)]
  h3 <- new_hawc_df[,c(1:33,46,40,41,42:45)]

  names(h1)[34] <- "toxval_type"
  names(h1)[35] <- "toxval_numeric"
  names(h1)[36] <- "toxval_units"

  names(h2)[34] <- "toxval_type"
  names(h2)[35] <- "toxval_numeric"
  names(h2)[36] <- "toxval_units"

  names(h3)[34] <- "toxval_type"
  names(h3)[35] <- "toxval_numeric"
  names(h3)[36] <- "toxval_units"

  new_hawc_df_final <- rbind(h1,h2,h3)
  rownames(new_hawc_df_final) <- c()

  new_hawc_df_final$study_type <- new_hawc_df_final$experiment_type
  new_hawc_df_final$exposure_route <- new_hawc_df_final$route_of_exposure
  new_hawc_df_final$exposure_method <- new_hawc_df_final$route_of_exposure
  new_hawc_df_final$study_duration_value <- new_hawc_df_final$exposure_duration_text
  new_hawc_df_final$study_duration_units <- new_hawc_df_final$exposure_duration_text

  #dim(new_hawc_df_final)
  new_hawc_df_final <- new_hawc_df_final[which(!is.na(new_hawc_df_final$toxval_numeric)),]
  #print(dim(new_hawc_df_final))
  new_hawc_df_final[,"source_id"] <- c(1:length(new_hawc_df_final[,1]))
  new_hawc_df_final <- new_hawc_df_final[,c("source_id",names(new_hawc_df_final[-46]))]


  runInsertTable(new_hawc_df_final,"hawc_new",toxval.db,do.halt=T,verbose=F)
  #print(str(new_hawc_df_final))

  #####################################################################
  cat("Build hawc_chemical_information table from new_hawc_df_final\n")
  #####################################################################
  chemical_information <- new_hawc_df_final[,c("name","casrn")]
  chemical_information <- unique(chemical_information[,1:2])
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  chemical_information <- chemical_information[,c('chemical_id','name','casrn')]
  #print(View(chemical_information))
  #runInsertTable(chemical_information,"hawc_chemical_information",toxval.db,do.halt=T,verbose=F)

}
  

