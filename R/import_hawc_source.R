#--------------------------------------------------------------------------------------
#' @description Load HAWC Source into toxval_source
#'
#' Note that the different tabs in the input sheet have different names, so these need
#' to be adjusted manually for the code to work. This is a problem wit how the data
#' is stored in HAWC
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile1 The input file ./hawc/hawc_files/hawc_original_12_06_21.xlsx
#' @param infile2 The input file ./hawc/hawc_files/dose_dict.xlsx
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#' @title FUNCTION_TITLE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[openxlsx]{getSheetNames}}, \code{\link[openxlsx]{read.xlsx}}
#'  \code{\link[stats]{aggregate}}
#'  \code{\link[digest]{digest}}
#' @rdname import_hawc_source
#' @export
#' @importFrom openxlsx getSheetNames read.xlsx
#' @importFrom stats aggregate
#' @importFrom digest digest
#--------------------------------------------------------------------------------------
import_hawc_source <- function(db,
                               infile1="hawc_original_12_06_21.xlsx",
                               infile2="dose_dict.xlsx",
                               chem.check.halt=FALSE,
                               do.reset=FALSE,
                               do.insert=FALSE) {
  printCurrentFunction(db)
  source = "HAWC Project"
  source_table = "source_hawc"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2021-12-06")

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

  # Imported dose dictionary logic from import_hawc_pfas_source to address dose_group_id
  res_dose3 <- openxlsx::read.xlsx(infile2) %>%
    dplyr::select(dose_regime, dose_group_id, dose, name) %>%
    dplyr::distinct()
  res_dose3[] = lapply(res_dose3, as.character)
  dose_dict <- res_dose3 %>%
    dplyr::arrange(dose_regime, dose_group_id, name, dose)
  dose_dict_orig = dose_dict
  # Get counts of dose entries per dose_regime - units pairs
  dose_dict = dose_dict %>%
    dplyr::select(dose_regime, name, dose) %>%
    dplyr::distinct() %>%
    #group_by() %>%
    dplyr::count(dose_regime, name) %>%
    dplyr::mutate(name_n = paste0("(", n, ") ", name)) %>%
    dplyr::select(-n) %>%
    dplyr::left_join(dose_dict, by=c("dose_regime", "name")) %>%
    # Combine doses by regime unit groups
    tidyr::pivot_wider(id_cols = c("dose_regime", "name", "name_n"),
                       names_from = "dose_group_id",
                       values_from = "dose") %>%
    tidyr::unite("dose", -dose_regime, -name, -name_n, sep=", ") %>%
    dplyr::mutate(dose = gsub(", NA", "", dose))


  #####################################################################
  cat("map hawc original with dose dictionary \n")
  #####################################################################
  new_hawc_df$NOEL_values <- s[match(paste(new_hawc_df$animal_group.dosing_regime.id,new_hawc_df$NOEL),paste(s$dose_regime,s$dose_group_id)),"dose"]
  new_hawc_df$NOEL_units <-  s[match(paste(new_hawc_df$animal_group.dosing_regime.id,new_hawc_df$NOEL),paste(s$dose_regime,s$dose_group_id)),"name"]
  new_hawc_df$LOEL_values <- s[match(paste(new_hawc_df$animal_group.dosing_regime.id,new_hawc_df$LOEL),paste(s$dose_regime,s$dose_group_id)),"dose"]
  new_hawc_df$LOEL_units <-  s[match(paste(new_hawc_df$animal_group.dosing_regime.id,new_hawc_df$LOEL),paste(s$dose_regime,s$dose_group_id)),"name"]
  new_hawc_df$FEL_values <- s[match(paste(new_hawc_df$animal_group.dosing_regime.id,new_hawc_df$FEL),paste(s$dose_regime,s$dose_group_id)),"dose"]
  new_hawc_df$FEL_units <-  s[match(paste(new_hawc_df$animal_group.dosing_regime.id,new_hawc_df$FEL),paste(s$dose_regime,s$dose_group_id)),"name"]
  #s_new <- unique(s[,c("dose_regime","dose_group_id","dose")])
  #doses<- stats::aggregate(dose ~ dose_regime + dose_group_id, data = s_new, toString)
  # Changed doses field to use dose_dict that results from hawc_pfas script logic

  new_hawc_df$doses <-  dose_dict[match(new_hawc_df$animal_group.dosing_regime.id,dose_dict$dose_regime),"dose"]
  # fix nested df in doses column issue
  corrected_column <- new_hawc_df$doses
  new_hawc_df$doses <- corrected_column$dose


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
  res = res[!generics::is.element(res$casrn,"NOCAS"),]
  names(res)[generics::is.element(names(res),"LOEL_original")] = "loel_original"
  names(res)[generics::is.element(names(res),"NOEL_original")] = "noel_original"
  names(res)[generics::is.element(names(res),"FEL_original")] = "fel_original"

  #####################################################################
  cat("Add the code from the original version from Aswani\n")
  #####################################################################
  unique(res$study_type)
  res$study_type <- tolower(res$study_type)
  para_vals <- grep("\\(",res$study_type)
  res[para_vals, "study_type"] <- gsub("(.*)(\\s+\\(.*)","\\1",res[para_vals, "study_type"])
  ##### fix exposure_route
  unique(res$exposure_route)
  res$exposure_route <- tolower(res$exposure_route)
  oral_vals <- grep("oral", res$exposure_route)
  res[oral_vals, "exposure_route"] <- gsub("(oral)(\\s+.*)","\\1",res[oral_vals, "exposure_route"])
  injection_vals <- grep("injection", res$exposure_route)
  res[injection_vals, "exposure_route"] <- gsub("(.*)(\\s+injection)","\\1",res[injection_vals, "exposure_route"])
  ####### fix exposure_method
  unique(res$exposure_method)
  res$exposure_method <- tolower(res$exposure_method)
  oral_vals <- grep("oral", res$exposure_method)
  res[oral_vals, "exposure_method"] <- gsub("(oral\\s+)(.*)","\\2",res[oral_vals, "exposure_method"])
  injection_vals <- grep("injection", res$exposure_method)
  res[injection_vals, "exposure_method"] <- gsub("(.*\\s+)(injection)","\\2",res[injection_vals, "exposure_method"])
  res$exposure_method <- tolower(res$exposure_method)

  ######### fix study duration value and units
  #hour vals
  hour_vals <- grep("hour", res$study_duration_value, ignore.case = T)
  res[hour_vals,"study_duration_units"] <- "hour"
  res[hour_vals,"study_duration_value"] <- gsub("^([0-9]+)(\\s+)(hours)","\\1",res[hour_vals,"study_duration_value"])
  # day vals
  day_vals <- grep("day", res$study_duration_value, ignore.case = T)
  res[day_vals,"study_duration_units"] <- "day"
  res[day_vals,"study_duration_value"] <- gsub("^([0-9]+)(\\s+)(days)(.*)","\\1",res[day_vals,"study_duration_value"])
  day_vals <- grep("day", res$study_duration_value, ignore.case = T)
  res[day_vals,"study_duration_value"] <- gsub("^([0-9]+\\-)([0-9]+)(\\s+)(days)(.*)","\\2",res[day_vals,"study_duration_value"])
  #week vals
  week_vals <- grep("week", res$study_duration_value, ignore.case = T)
  res[week_vals,"study_duration_units"] <- "week"
  res[week_vals,"study_duration_value"] <- gsub("^([0-9]+)(\\s+)(weeks)(.*)","\\1",res[week_vals,"study_duration_value"])
  week_vals <- grep("week", res$study_duration_value, ignore.case = T)
  res[week_vals,"study_duration_value"] <- gsub("^(.*[^0-9]+)([0-9]+)(\\s+)(weeks)(.*)","\\2",res[week_vals,"study_duration_value"])
  #month vals (without PND)
  month_vals <- grep("months$", res$study_duration_value, ignore.case = T)
  res[month_vals,"study_duration_units"] <- "month"
  res[month_vals,"study_duration_value"] <- gsub("^([0-9]+)(\\s+)(months)","\\1",res[month_vals,"study_duration_value"])
  #one time vals
  one_time_vals <- grep("one time", res$study_duration_value, ignore.case = T)
  res[one_time_vals,"study_duration_units"] <- "one time"
  res[one_time_vals,"study_duration_value"] <- "1"
  # GD range vals
  GD_vals <- grep("GD\\s+.*\\-[^a-zA-Z]+$", res$study_duration_value, ignore.case = T)
  res[GD_vals,"study_duration_units"] <- "GD"
  res[GD_vals,"study_duration_value"] <- gsub("^(GD)(\\s+.*\\-\\s*)(.*)","\\3",res[GD_vals,"study_duration_value"])

  # GD until vals
  GD_until_vals <- grep("GD.*until.*[^0]$", res$study_duration_value, ignore.case = T)
  res[GD_until_vals,"study_duration_units"] <- "GD"
  res[GD_until_vals,"study_duration_value"] <- gsub("^(GD.*GD\\s+)(.*)","\\2",res[GD_until_vals,"study_duration_value"])

  GD_until_zero_vals <- grep("GD.*until.*[0]$", res$study_duration_value, ignore.case = T)
  res[GD_until_zero_vals,"study_duration_units"] <- res[GD_until_zero_vals,"study_duration_value"]
  res[GD_until_zero_vals,"study_duration_value"] <- ""

  #PND range vals
  PND_vals <- grep(".*PND\\s*.*[^0a-zA-Z]$", res$study_duration_value, ignore.case = T)
  res[PND_vals,"study_duration_units"] <- "PND"
  res[PND_vals,"study_duration_value"] <- gsub("^(.*PND\\s*)(\\d+)","\\2",res[PND_vals,"study_duration_value"])
  res[which(res$study_duration_value == "2-15" & res$study_duration_units == "PND"),"study_duration_value"] <- gsub("(\\d+\\-)(\\d+)","\\2",res[which(res$study_duration_value == "2-15" & res$study_duration_units == "PND"),"study_duration_value"])
  PND_vals <- grep(".*PND\\s*[^0]\\d+$", res$study_duration_value, ignore.case = T)
  res[PND_vals,"study_duration_units"] <- "PND"
  res[PND_vals,"study_duration_value"] <- gsub("^(.*PND\\s*)(\\d+)(.*?)","\\2",res[PND_vals,"study_duration_value"])
  res[which(res$study_duration_value == "21, not PND 0" & res$study_duration_units == "PND"),"study_duration_value"] <- gsub("(\\d+)(\\,.*)","\\1",res[which(res$study_duration_value == "21, not PND 0" & res$study_duration_units == "PND"),"study_duration_value"])
  # GD or PND zero vals
  zero_vals <- grep("PND0|GD0", res$study_duration_value, ignore.case = T)
  res[zero_vals,"study_duration_units"] <- res[zero_vals,"study_duration_value"]
  res[zero_vals,"study_duration_value"] <- ""
  # 1 OR 2 years vals
  or_vals <- grep("or", res$study_duration_value, ignore.case = T)
  res[or_vals,"study_duration_units"] <- gsub("(.*or\\s+)(\\d+)(\\s+)(\\w+)","\\4",res[or_vals,"study_duration_value"])
  res[or_vals,"study_duration_value"] <- gsub("(.*or\\s+)(\\d+)(\\s+)(\\w+)","\\2",res[or_vals,"study_duration_value"])
  # PND 3-10 vals
  PND_vals <- grep("PND", res$study_duration_value, ignore.case = T)
  res[PND_vals,"study_duration_units"] <-"PND"
  res[PND_vals,"study_duration_value"] <- gsub("(PND.*\\-)(\\d+)","\\2",res[PND_vals,"study_duration_value"])
  res[which(res$study_duration_value == "-"),"study_duration_value"] <- ""

  res$study_duration_value <- as.numeric(res$study_duration_value)


  #####################################################################
  cat("Collapse duplicated that just differ by critical effect \n")
  #####################################################################
  res2 = res[,!names(res)%in%c("critical_effect","source_id","endpoint_url_original","endpoint_url","target")]
  cat(nrow(res),"\n")
  res2$hashkey = NA
  for(i in 1:nrow(res2)) {
    hashkey = digest::digest(paste0(res2[i,],collapse=""), serialize = FALSE)
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

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  # Fix toxval_units unicode
  res$toxval_units = fix.replace.unicode(res$toxval_units)

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"

  # Add version date. Can be converted to a mutate statement as needed
  res$source_version_date <- src_version_date
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db=db,
                       source=source,
                       table=source_table,
                       res=res,
                       do.reset=do.reset,
                       do.insert=do.insert,
                       chem.check.halt=chem.check.halt)
}

