#--------------------------------------------------------------------------------------
#' Load HAWC Source into toxval_source
#'
#' Note that the different tabs in the input sheet have different names, so these need
#' to be adjusted manually for the code to work. This is a problem wit how the data
#' is stored in HAWC
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile1 The input file ./hawc/hawc_files/hawc_original_12_06_21.xlsx
#' @param infile2 The input file ./hawc/hawc_files/dose_dict.xlsx
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#--------------------------------------------------------------------------------------
import_hawc_source <- function(db,
                               infile1="hawc_original_12_06_21.xlsx",
                               infile2="dose_dict.xlsx",
                               chem.check.halt=T) {
  printCurrentFunction(db)
  source = "HAWC"

  infile1 = paste0(toxval.config()$datapath,"hawc/hawc_files/",infile1)
  infile2 = paste0(toxval.config()$datapath,"hawc/hawc_files/",infile2)
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

  #####################################################################
  cat("read in the dose dictionary \n")
  #####################################################################
  s <- openxlsx::read.xlsx(infile2)
  #runInsertTable(s,"hawc_dose_dictionary",db,do.halt=T,verbose=F)
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

  res = new_hawc_df_final
  res = res[!is.element(res$casrn,"NOCAS"),]
  names(res)[is.element(names(res),"LOEL_original")] = "loel_original"
  names(res)[is.element(names(res),"NOEL_original")] = "noel_original"
  names(res)[is.element(names(res),"FEL_original")] = "fel_original"

  #####################################################################
  cat("Collapse duplicated that just differ by critical effect \n")
  #####################################################################
  res2 = res[,!names(res)%in%c("critical_effect","source_id","endpoint_url_original","endpoint_url","target")]
  cat(nrow(res),"\n")
  res2$hashkey = NA
  for(i in 1:nrow(res2)) {
    hashkey = digest(paste0(res2[i,],collapse=""), serialize = FALSE)
    res2[i,"hashkey"] = hashkey
    res[i,"hashkey"] = hashkey
  }
  res2 = unique(res2)
  res2$critical_effect = NA
  for(i in 1:nrow(res2)) {
    hashkey = res2[i,"hashkey"]
    res3 = res[res$hashkey==hashkey,]
    x = res3$target
    y = res3$critical_effect
    ce = ""
    for(j in 1:length(x)) ce=paste0(ce,x[j],":",y[j],"|")
    ce = substr(ce,1,(nchar(ce)-1))
    res2[i,"critical_effect"] = ce
  }
  res2$source_id = NA
  res2$endpoint_url_original = NA
  res2$endpoint_url = NA
  res2$target = NA
  res2 = res2[,!names(res2)%in%c("hashkey")]
  res = res2
  cat(nrow(res),"\n")

  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="HAWC",table="source_hawc",res=res,F,T,T)
}
