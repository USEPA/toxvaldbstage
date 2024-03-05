#--------------------------------------------------------------------------------------
#' @description Load ATSDR PFAS 2021 Source into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param indir The path for all the input xlsx files ./atsdr_pfas_2021/atsdr_pfas_2021_files
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
#'  \code{\link[openxlsx]{read.xlsx}}
#'  \code{\link[stringr]{str_split}}, \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_replace}}, \code{\link[stringr]{str_extract}}
#'  \code{\link[stats]{na.fail}}
#'  \code{\link[dplyr]{bind}}
#' @rdname import_atsdr_pfas_2021_source
#' @export
#' @importFrom openxlsx read.xlsx
#' @importFrom stringr str_split str_trim str_replace_all str_extract_all
#' @importFrom stats na.omit
#' @importFrom dplyr bind_rows
#--------------------------------------------------------------------------------------
import_atsdr_pfas_2021_source <- function(db,
                                          chem.check.halt=F) {
  printCurrentFunction(db)
  indir = paste0(toxval.config()$datapath,"atsdr_pfas/atsdr_pfas_2021/atsdr_pfas_2021_files/")
  #####################################################################
  cat("Build list of dataframes with all 6 ATSDR_TP_2021 files\n")
  #####################################################################
  files.list <- list.files(indir,pattern = "ATSDR_TP_2021.*.xlsx")
  files.list <- paste0( indir, '/',files.list)
  any_temp_files <- grep("^\\~\\$.*", files.list, value = T)
  files.list <- files.list[! files.list %in% any_temp_files]
  res <- lapply(files.list,openxlsx::read.xlsx)
  indir = paste0(indir,"/")
  #####################################################################
  cat("ATSDR_TP_2021_Perfluoroalkyls_Oral\n")
  ######################################################################
  res1 <- res[[1]][8:269,c(1:22)]
  name.list <- c("source_name_sid", "casrn","name","source_url","subsource","source_name_cid","key_to_figure","study_type_exposure_route", "short_name", "short_ref","species_strain","sex","duration","doses","parameters_monitored","endpoint","NOAEL","LOAEL_less_serious","LOAEL_serious","effect","comments","long_ref")
  names(res1) <- name.list
  ###runInsertTable(res1,"atsdr_tp_2021_perfluoroalkyls_oral",db,do.halt=T,verbose=F)

  # extract exposure_route and study type info from study_type_exposure_route
  res1$exposure_route_original <- res1$study_type_exposure_route
  res1$exposure_route <- gsub("(.*\\-\\s+)(.*)","\\2",res1$study_type_exposure_route)
  res1$study_type_original <- res1$study_type_exposure_route
  res1$study_type <- tolower(gsub("(.*)(\\s+\\-\\s+.*)","\\1",res1$study_type_exposure_route))
  # extract sps and strain info from species_strain
  res1$species_original <- res1$species_strain
  res1$species <- gsub("(.*?)(\\s+.*)","\\1",res1$species_strain)
  res1$strain_original <- res1$species_strain
  res1$strain <- gsub("(.*?\\s+)(.*)","\\2",res1$species_strain)
  res1$strain <- gsub("^\\s+|\\s+$","",res1$strain)
  res1$strain <- gsub("(^\\()(.*)(\\)$)","\\2", res1$strain)
  res1$strain[grep("\\([^\\)][A-Za-z]+$", res1$strain)] <- gsub("(.*)($)","\\1)", res1$strain[grep("\\([^\\)][A-Za-z]+$", res1$strain)])
  # extract sex info from sex original
  res1$sex_original <- res1$sex

  res1[grep("F", res1$sex, ignore.case = T),"sex"] <- "female(s)"
  res1[grep("M", res1$sex_original, ignore.case = T),"sex"] <- paste("M", res1[grep("M", res1$sex_original, ignore.case = T),"sex"] , sep = '/')
  res1$sex <- gsub("(^M)(\\/+.*M$)","\\1",res1$sex)
  res1$sex[res1$sex != "M"& res1$sex != "female(s)"& res1$sex != "M/female(s)"] <- "-"
  res1$sex[which(is.na(res1$sex))] <- "-"

  #extract document name from source url
  res1$document_name <- gsub("(.*\\/)(.*)","\\2", res1$source_url)

  #extract study duration value, units and exposure_method from duration
  res1$exposure_method <- gsub("(.*\\s*)(\\(.*\\))(\\s*.*)","\\2",res1$duration)
  res1$exposure_method[grep("\\(.*\\)", res1$exposure_method, invert = T)] <- "-"
  res1$exposure_method_original <- res1$exposure_method

  res1$study_duration_value_original <- gsub("(.*\\s*)(\\(.*\\))(\\s*.*)","\\1",res1$duration)
  res1$study_duration_units_original <- res1$study_duration_value_original
  res1[grep("^[0-9]+\\s+[A-Za-z]+\\s+.*",res1$duration),"study_duration_value"] <- gsub("(^[0-9]+)(\\s+[A-Za-z]+\\s+.*)","\\1",res1[grep("^[0-9]+\\s+[A-Za-z]+\\s+.*",res1$duration),"duration"])
  res1[grep("^[0-9]+\\s+[A-Za-z]+\\s+.*",res1$duration),"study_duration_units"] <- gsub("(^[0-9]+\\s+)([A-Za-z]+)(\\s+.*)","\\2",res1[grep("^[0-9]+\\s+[A-Za-z]+\\s+.*",res1$duration),"duration"])

  # fix range
  res1[grep("^[0-9]+\\-[0-9]+\\s+[A-Za-z]+\\s+.*",res1$duration),"study_duration_value"] <- gsub("(^[0-9]+\\-)([0-9]+)(\\s+[A-Za-z]+\\s+.*)","\\2",res1[grep("^[0-9]+\\-[0-9]+\\s+[A-Za-z]+\\s+.*",res1$duration),"duration"])
  res1[grep("^[0-9]+\\-[0-9]+\\s+[A-Za-z]+\\s+.*",res1$duration),"study_duration_units"] <- gsub("(^[0-9]+\\-[0-9]+\\s+)([A-Za-z]+)(\\s+.*)","\\2",res1[grep("^[0-9]+\\-[0-9]+\\s+[A-Za-z]+\\s+.*",res1$duration),"duration"])

  #fix GD
  res1[grep("^GDs",res1$duration),"study_duration_value"] <- gsub("(GDs)(\\s+)(\\d+\\-)(\\d+)(\\s+.*)","\\4",res1[grep("^GDs",res1$duration),"duration"])
  res1[grep("^GDs",res1$duration),"study_duration_units"] <- gsub("(GDs)(\\s+)(\\d+\\-)(\\d+)(\\s+.*)","\\1",res1[grep("^GDs",res1$duration),"duration"])

  # fix m/f , gen info in duration
  res1[grep("\\:",res1$duration),"study_duration_value"] <- gsub("(.*?)([0-9]+)(\\s+)([A-Za-z]+)(\\s+.*)","\\2",res1[grep("\\:",res1$duration),"duration"])
  res1[grep("\\:",res1$duration),"study_duration_units"] <- gsub("(.*?)([0-9]+)(\\s+)([A-Za-z]+)(\\s+.*)","\\4",res1[grep("\\:",res1$duration),"duration"])

  # fix PND
  res1[grep("PND\\s+",res1$duration),"study_duration_value"] <- gsub(".*(PND)(\\s+)([0-9]+)(\\s+.*)","\\3",res1[grep("PND\\s+",res1$duration),"duration"])
  res1[grep("PND\\s+",res1$duration),"study_duration_units"] <- gsub(".*(PND)(\\s+)([0-9]+)(\\s+.*)","\\1",res1[grep("PND\\s+",res1$duration),"duration"])

  # fix once
  res1[grep("^Once\\s+\\(.*\\)$",res1$duration),"study_duration_value"] <- 1
  res1[grep("^Once\\s+\\(.*\\)$",res1$duration),"study_duration_units"] <- "days"

  # weeks and days
  res1[which(is.na(res1$study_duration_value)),"study_duration_units"] <- gsub("(.*)([0-9]+)(\\s+)(days|weeks)(.*)","\\4",res1[which(is.na(res1$study_duration_value)),"duration"])
  res1[which(res1$duration == "1 time/week 4 weeks"), "study_duration_value"] <- gsub("(.*)([0-9]+)(\\s+weeks)","\\2", res1[which(res1$duration == "1 time/week 4 weeks"), "duration"])

  res1[grep("^[0-9]+\\s+[A-Za-z]+$",res1$duration),"study_duration_value"] <- gsub("(^[0-9]+)(\\s+[A-Za-z]+$)","\\1",res1[grep("^[0-9]+\\s+[A-Za-z]+$",res1$duration),"duration"])
  res1[which(is.na(res1$study_duration_value)),"study_duration_value"] <- gsub("([0-9]+)([^[:alnum:]]+)([0-9]+)(\\s+.*)","\\3",res1[which(is.na(res1$study_duration_value)),"duration"])

  res1$study_duration_value <- as.character(res1$study_duration_value)
  # change weeks and days plural to singular
  res1[grep("s$",res1$study_duration_units), "study_duration_units"] <- gsub("(.*)(s$)","\\1",res1[grep("s$",res1$study_duration_units), "study_duration_units"])

  # critical effect change na to hyphen
  res1$critical_effect <- res1$effect
  res1[which(is.na(res1$critical_effect)),"critical_effect"] <- "-"

  # create key desc table
  key <- res[[1]][279:291,7]
  key <- paste(key, collapse = " ")

  key_desc_table <- data.frame(unlist(stringr::str_split(key, ";")), stringsAsFactors = F)
  names(key_desc_table) <- "original_keys"
  key_desc <- data.frame(do.call('rbind', strsplit(as.character(key_desc_table$original_keys),'=',fixed=TRUE)), stringsAsFactors = F)
  rm(key_desc_table, key)

  names(key_desc) <- c("key","description")
  key_desc[nrow(key_desc) + 1,] = c("BW","body weight")
  key_desc[grep("Bd wt", key_desc$key),"key"] <- "Bd wt"

  key_desc[nrow(key_desc) + 1,] = c("HE","hematological")
  key_desc[grep("Hemato", key_desc$key),"key"] <- "Hemato"

  key_desc$key <- stringr::str_trim(as.character(key_desc[,"key"]))
  key_desc$description <- stringr::str_trim(as.character(key_desc[,"description"]))

  #### key replacements
  for(i in 1:nrow(key_desc)) {
    valold <- key_desc[i,1]
    valnew <- key_desc[i,2]
    res1[generics::is.element(res1$endpoint,valold),"endpoint"] <- valnew
  }

  for (i in 1:nrow(key_desc)){
    for ( j in 1:nrow(res1)){
      res1$parameters_monitored[j] <- stringr::str_replace_all(res1$parameters_monitored[j],paste("\\b",key_desc[,"key"],"\\b", sep  = "")[i],key_desc[,"description"][i])

    }
  }

  for (i in 1:nrow(key_desc)){
    for ( j in 1:nrow(res1)){
      res1$sex[j] <- stringr::str_replace_all(res1$sex[j],paste("\\b",key_desc[,"key"],"\\b", sep  = "")[i],key_desc[,"description"][i])

    }
  }

  for (i in 1:nrow(key_desc)){
    for ( j in 1:nrow(res1)){
      res1$exposure_method[j] <- stringr::str_replace_all(res1$exposure_method[j],paste("\\b",key_desc[,"key"],"\\b", sep  = "")[i],key_desc[,"description"][i])

    }
  }

  res1$exposure_method[grep("^\\(.*\\)$", res1$exposure_method)] <- gsub("(^\\(+)(.*)(\\)+)","\\2",res1$exposure_method[grep("^\\(.*\\)$", res1$exposure_method)])

  # extract year info from short_ref
  res1$year <- gsub("(.*)([0-9]{4})(.*)","\\2",res1$short_ref)

  # assign toxval_units
  res1$toxval_units <- "mg/kg/day"

  # fix long ref
  short_ref_key <- unique(res1[which(is.na(res1$long_ref)),"short_ref"])
  short_ref_key_table <- data.frame(short_ref_key, stringsAsFactors = "F")
  short_ref_key_table$description <- "-"

  for (i in 1:23){
    short_ref_key_table$description[i] <-unique(stats::na.omit(res1[which(res1$short_ref %in% short_ref_key_table$short_ref_key[i]),"long_ref"]))
  }

  short_ref_key_table$description[24] <- unique(stats::na.omit(res1[which(res1$short_ref %in% short_ref_key_table[24,1]),"long_ref"]))[1]
  short_ref_key_table$description[25] <- unique(stats::na.omit(res1[which(res1$short_ref %in% short_ref_key_table[24,1]),"long_ref"]))[2]

  for (i in 26:39){
    short_ref_key_table$description[i] <-unique(stats::na.omit(res1[which(res1$short_ref %in% short_ref_key_table$short_ref_key[i]),"long_ref"]))
  }

  for(i in 1:nrow(short_ref_key_table)) {
    valold <- short_ref_key_table[i,1]
    valnew <- short_ref_key_table[i,2]
    res1[generics::is.element(res1$short_ref,valold),"long_ref"] <- valnew
  }

  names(res1)[names(res1) == "comments"] <- "toxval_details"

  #### combine NOAEL, serious and less serious LOAEL values to create toxval_numeric and toxval_type

  h1 <- res1[,c(1:7,9:10,12,14:16,17,41,20:40)]
  names(h1)[names(h1) == "NOAEL"] <- "toxval_numeric"
  h1$toxval_type <- "NOAEL"

  h2 <- res1[,c(1:7,9:10,12,14:16,18,41,20:40)]
  names(h2)[names(h2) == "LOAEL_less_serious"] <- "toxval_numeric"
  h2$toxval_type <- "LOAEL_less_serious"

  h3 <- res1[,c(1:7,9:10,12,14:16,19,41,20:40)]
  names(h3)[names(h3) == "LOAEL_serious"] <- "toxval_numeric"
  h3$toxval_type <- "LOAEL_serious"

  res1_new <- rbind(h1,h2,h3)
  names(res1_new)[names(res1_new) == "effect"] <- "critical_effect_original"
  # remove plus sign from the beginning of long ref
  res1_new$long_ref <-  gsub("(^[^[:alnum:]])(.*)","\\2",res1_new$long_ref)

  # subset by removing rows with empty numeric values
  res1_new <- res1_new[which(res1_new$toxval_numeric != ""),]
  # replace the occurance of sex info in numeric values
  res1_new[grep("[A-Za-z]+",res1_new$toxval_numeric),"toxval_numeric"] <- gsub("(.*)(\\s*[A-Za-z]+.*)","\\1",res1_new[grep("[A-Za-z]+",res1_new$toxval_numeric),"toxval_numeric"])
  res1_new[grep(".*\\s+$",res1_new$toxval_numeric),"toxval_numeric"] <- gsub("(.*?)(\\s+.*)","\\1",res1_new[grep(".*\\s+$",res1_new$toxval_numeric),"toxval_numeric"])

  #####################################################################
  cat("ATSDR_TP_2021_PFNA_Inhalation\n")
  #####################################################################
  res2 <- res[[2]][8:11,c(1:21)]
  name.list <- c("source_name_sid", "casrn","name","source_url","subsource","source_name_cid","key_to_figure","study_type_exposure_form", "short_name", "short_ref","species_strain","sex","duration","doses","parameters_monitored","endpoint","NOAEL","LOAEL_less_serious","LOAEL_serious","effect","long_ref")
  names(res2) <- name.list
  ###runInsertTable(res2,"atsdr_tp_2021_pfna_inhalation",db,do.halt=T,verbose=F)

  # extract exposure_form and study type info from study_type_exposure_form
  res2$exposure_form_original <- res2$study_type_exposure_form
  res2$exposure_form <- gsub("(.*\\s+\\(+)(.*)(\\s*\\)+)","\\2",res2$study_type_exposure_form)
  res2$study_type_original <- res2$study_type_exposure_route
  res2$study_type <- tolower(gsub("(.*)(\\s+\\(+.*)","\\1",res2$study_type_exposure_form))
  # extract sps and strain info from species_strain
  res2$species_original <- res2$species_strain
  res2$species <- gsub("(.*?)(\\s+.*)","\\1",res2$species_strain)
  res2$strain_original <- res2$species_strain
  res2$strain <- gsub("(.*?\\s+)(.*)","\\2",res2$species_strain)
  res2$strain <- gsub("^\\s+|\\s+$","",res2$strain)
  res2$strain <- gsub("(^\\()(.*)(\\)$)","\\2", res2$strain)
  # extract sex info from sex original
  res2$sex_original <- res2$sex

  res2[grep("F", res2$sex, ignore.case = T),"sex"] <- "female(s)"
  res2[grep("M", res2$sex_original, ignore.case = T),"sex"] <- paste("M", res2[grep("M", res2$sex_original, ignore.case = T),"sex"] , sep = '/')
  res2$sex <- gsub("(^M)(\\/+.*M$)","\\1",res2$sex)
  res2$sex[res2$sex != "M"& res2$sex != "female(s)"& res2$sex != "M/female(s)"] <- "-"
  res2$sex[which(is.na(res2$sex))] <- "-"

  #extract document name from source url
  res2$document_name <- gsub("(.*\\/)(.*)","\\2", res2$source_url)

  #extract study duration value, units from duration

  res2$study_duration_value_original <- res2$duration
  res2$study_duration_units_original <- res2$duration
  res2$study_duration_value <- gsub("(^[0-9]+)(\\s+.*)","\\1",res2$duration)
  res2$study_duration_units <- gsub("(^[0-9]+\\s+)(.*)","\\2",res2$duration)
  # change weeks and days plural to singular
  res2[grep("s$",res2$study_duration_units), "study_duration_units"] <- gsub("(.*)(s$)","\\1",res2[grep("s$",res2$study_duration_units), "study_duration_units"])

  # critical effect change na to hyphen
  res2$critical_effect <- res2$effect
  res2[which(is.na(res2$critical_effect)),"critical_effect"] <- "-"

  # shift parameters monitored present in doses
  res2[grep("[a-zA-Z]+",res2$doses), "parameters_monitored"] <- gsub("(.*[0-9]+\\s+)([A-Za-z]+.*)","\\2",res2[grep("[a-zA-Z]+",res2$doses), "doses"])
  res2[grep("[a-zA-Z]+",res2$doses), "doses"] <- gsub("(.*[0-9]+)(\\s+[A-Za-z]+.*)","\\1",res2[grep("[a-zA-Z]+",res2$doses), "doses"])

  # create key desc table
  key <- res[[2]][15:16,7]
  key <- paste(key, collapse = " ")

  key_desc_table <- data.frame(unlist(stringr::str_split(key, ";")), stringsAsFactors = F)
  names(key_desc_table) <- "original_keys"
  key_desc <- data.frame(do.call('rbind', strsplit(as.character(key_desc_table$original_keys),'=',fixed=TRUE)), stringsAsFactors = F)
  rm(key_desc_table, key)

  names(key_desc) <- c("key","description")
  key_desc[nrow(key_desc) + 1,] = c("BW","body weight")
  key_desc[grep("Bd wt", key_desc$key),"key"] <- "Bd wt"

  key_desc$key <- stringr::str_trim(as.character(key_desc[,"key"]))
  key_desc$description <- stringr::str_trim(as.character(key_desc[,"description"]))

  #### key replacements
  for(i in 1:nrow(key_desc)) {
    valold <- key_desc[i,1]
    valnew <- key_desc[i,2]
    res2[generics::is.element(res2$endpoint,valold),"endpoint"] <- valnew
  }

  for (i in 1:nrow(key_desc)){
    for ( j in 1:nrow(res2)){
      res2$parameters_monitored[j] <- stringr::str_replace_all(res2$parameters_monitored[j],paste("\\b",key_desc[,"key"],"\\b", sep  = "")[i],key_desc[,"description"][i])

    }
  }

  for (i in 1:nrow(key_desc)){
    for ( j in 1:nrow(res2)){
      res2$sex[j] <- stringr::str_replace_all(res2$sex[j],paste("\\b",key_desc[,"key"],"\\b", sep  = "")[i],key_desc[,"description"][i])

    }
  }

  # extract year info from short_ref
  res2$year <- gsub("(.*)([0-9]{4})(.*)","\\2",res2$short_ref)

  # assign toxval_units
  res2$toxval_units <- "mg/m3"

  # fix long ref
  short_ref_key <- unique(res2[which(is.na(res2$long_ref)),"short_ref"])
  short_ref_key_table <- data.frame(short_ref_key, stringsAsFactors = "F")
  short_ref_key_table$description <- "-"

  short_ref_key_table$description[1] <- unique(stats::na.omit(res2[which(res2$short_ref %in% short_ref_key_table[1,1]),"long_ref"]))[1]

  for(i in 1:nrow(short_ref_key_table)) {
    valold <- short_ref_key_table[i,1]
    valnew <- short_ref_key_table[i,2]
    res2[generics::is.element(res2$short_ref,valold),"long_ref"] <- valnew
  }

  #### combine NOAEL, serious and less serious LOAEL values to create toxval_numeric and toxval_type
  h1 <- res2[,c(1:7,9:10,12,14:16,17,37,20:36)]
  names(h1)[names(h1) == "NOAEL"] <- "toxval_numeric"
  h1$toxval_type <- "NOAEL"

  h2 <- res2[,c(1:7,9:10,12,14:16,18,37,20:36)]
  names(h2)[names(h2) == "LOAEL_less_serious"] <- "toxval_numeric"
  h2$toxval_type <- "LOAEL_less_serious"

  h3 <- res2[,c(1:7,9:10,12,14:16,19,37,20:36)]
  names(h3)[names(h3) == "LOAEL_serious"] <- "toxval_numeric"
  h3$toxval_type <- "LOAEL_serious"

  res2_new <- rbind(h1,h2,h3)
  names(res2_new)[names(res2_new) == "effect"] <- "critical_effect_original"
  res2_new$long_ref <-  gsub("(^[^[:alnum:]])(.*)","\\2",res2_new$long_ref)

  # subset by removing rows with empty numeric values
  res2_new <- res2_new[which(res2_new$toxval_numeric != ""),]

  #####################################################################
  cat("ATSDR_TP_2021_PFOA_Dermal\n")
  #####################################################################
  res3 <- res[[3]][7:28,c(1:21)]
  name.list <- c("source_name_sid", "casrn","name","source_url","subsource","source_name_cid","study_type_exposure_route", "short_name", "short_ref","species_strain","sex","duration","doses","parameters_monitored","endpoint","NOAEL","LOAEL_less_serious","LOAEL_serious","effect","comments","long_ref")
  names(res3) <- name.list
  ###runInsertTable(res3,"atsdr_tp_2021_pfoa_dermal",db,do.halt=T,verbose=F)

  # extract exposure_route and study type info from study_type_exposure_route
  res3$exposure_route_original <- res3$study_type_exposure_route
  res3$exposure_route <- gsub("(.*\\-\\s+)(.*)","\\2",res3$study_type_exposure_route)
  res3$study_type_original <- res3$study_type_exposure_route
  res3$study_type <- tolower(gsub("(.*)(\\s+\\-\\s+.*)","\\1",res3$study_type_exposure_route))
  # extract sps and strain info from species_strain
  res3$species_original <- res3$species_strain
  res3$species <- gsub("(.*)(\\s+\\(+.*)","\\1",res3$species_strain)
  res3$strain_original <- res3$species_strain
  res3$strain <- gsub("(.*?\\s+)(.*)","\\2",res3$species_strain)
  res3$strain <- gsub("^\\s+|\\s+$","",res3$strain)
  res3$strain <- gsub("(^\\()(.*)(\\)$)","\\2", res3$strain)
  # extract sex info from sex original
  res3$sex_original <- res3$sex

  res3[grep("F", res3$sex, ignore.case = T),"sex"] <- "female(s)"
  res3[grep("M", res3$sex_original, ignore.case = T),"sex"] <- paste("M", res3[grep("M", res3$sex_original, ignore.case = T),"sex"] , sep = '/')
  res3$sex <- gsub("(^M)(\\/+.*M$)","\\1",res3$sex)
  res3$sex[res3$sex != "M"& res3$sex != "female(s)"& res3$sex != "M/female(s)"] <- "-"
  res3$sex[which(is.na(res3$sex))] <- "-"

  #extract document name from source url
  res3$document_name <- gsub("(.*\\/)(.*)","\\2", res3$source_url)

  #extract study duration value, units from duration
  res3$study_duration_value_original <- res3$duration
  res3$study_duration_units_original <- res3$duration
  res3[grep("^[0-9]+\\s+[A-Za-z]+\\s+.*",res3$duration),"study_duration_value"] <- gsub("(^[0-9]+)(\\s+[A-Za-z]+\\s+.*)","\\1",res3[grep("^[0-9]+\\s+[A-Za-z]+\\s+.*",res3$duration),"duration"])
  res3[grep("^[0-9]+\\s+[A-Za-z]+\\s+.*",res3$duration),"study_duration_units"] <- gsub("(^[0-9]+\\s+)([A-Za-z]+)(\\s+.*)","\\2",res3[grep("^[0-9]+\\s+[A-Za-z]+\\s+.*",res3$duration),"duration"])


  # fix once
  res3[grep("^Once.*",res3$duration),"study_duration_value"] <- 1
  res3[grep("^Once.*",res3$duration),"study_duration_units"] <- "days"

  # change weeks and days plural to singular
  res3[grep("s$",res3$study_duration_units), "study_duration_units"] <- gsub("(.*)(s$)","\\1",res3[grep("s$",res3$study_duration_units), "study_duration_units"])

  # critical effect change na to hyphen
  res3$critical_effect <- res3$effect
  res3[which(is.na(res3$critical_effect)),"critical_effect"] <- "-"

  #extract parameters monitored info shifted to doses

  for (i in 1:length(grep(".*\\,$",res3$doses))){
    res3[grep(".*\\,$",res3$doses),"parameters_monitored"][i] <- paste(gsub("(.*day\\s+)(.*)","\\2",res3[grep(".*\\,$",res3$doses),"doses"])[i], res3[grep(".*\\,$",res3$doses),"parameters_monitored"][i], collapse = ", ")
  }

  res3[grep(".*\\,$",res3$doses),"doses"] <- gsub("(.*day)(\\s+.*)","\\1",res3[grep(".*\\,$",res3$doses),"doses"])

  # create key desc table
  key <- res[[3]][29,4]
  key <- paste(key, collapse = " ")

  key_desc_table <- data.frame(unlist(stringr::str_split(key, ";")), stringsAsFactors = F)
  names(key_desc_table) <- "original_keys"
  key_desc <- data.frame(do.call('rbind', strsplit(as.character(key_desc_table$original_keys),'=',fixed=TRUE)), stringsAsFactors = F)
  rm(key_desc_table, key)

  names(key_desc) <- c("key","description")
  key_desc[nrow(key_desc) + 1,] = c("BW","body weight")
  key_desc[grep("Bd wt", key_desc$key),"key"] <- "Bd wt"

  key_desc[nrow(key_desc) + 1,] = c("HE","hematological")
  key_desc[grep("Hemato", key_desc$key),"key"] <- "Hemato"

  key_desc$key <- stringr::str_trim(as.character(key_desc[,"key"]))
  key_desc$description <- stringr::str_trim(as.character(key_desc[,"description"]))

  #### key replacements

  for(i in 1:nrow(key_desc)) {
    valold <- key_desc[i,1]
    valnew <- key_desc[i,2]
    res3[generics::is.element(res3$endpoint,valold),"endpoint"] <- valnew
  }

  for (i in 1:nrow(key_desc)){
    for ( j in 1:nrow(res3)){
      res3$parameters_monitored[j] <- stringr::str_replace_all(res3$parameters_monitored[j],paste("\\b",key_desc[,"key"],"\\b", sep  = "")[i],key_desc[,"description"][i])

    }
  }

  for (i in 1:nrow(key_desc)){
    for ( j in 1:nrow(res3)){
      res3$sex[j] <- stringr::str_replace_all(res3$sex[j],paste("\\b",key_desc[,"key"],"\\b", sep  = "")[i],key_desc[,"description"][i])

    }
  }

  # extract year info from short_ref
  res3$year <- gsub("(.*)([0-9]{4})(.*)","\\2",res3$short_ref)

  # assign toxval_units derived from doses
  res3$toxval_units <- gsub("(.*[0-9]+\\s+)([A-Za-z]+.*)","\\2",res3$doses)

  # fix long ref
  short_ref_key <- unique(res3[which(is.na(res3$long_ref)),"short_ref"])
  short_ref_key_table <- data.frame(short_ref_key, stringsAsFactors = "F")
  short_ref_key_table$description <- "-"

  for (i in 1:2){
    short_ref_key_table$description[i] <-unique(stats::na.omit(res3[which(res3$short_ref %in% short_ref_key_table$short_ref_key[i]),"long_ref"]))
  }

  for(i in 1:nrow(short_ref_key_table)) {
    valold <- short_ref_key_table[i,1]
    valnew <- short_ref_key_table[i,2]
    res3[generics::is.element(res3$short_ref,valold),"long_ref"] <- valnew
  }

  names(res3)[names(res3) == "comments"] <- "toxval_details"

  #### combine NOAEL, serious and less serious LOAEL values to create toxval_numeric and toxval_type

  h1 <- res3[,c(1:6,8:9,11,13:15,16,38,19:37)]
  names(h1)[names(h1) == "NOAEL"] <- "toxval_numeric"
  h1$toxval_type <- "NOAEL"

  h2 <- res3[,c(1:6,8:9,11,13:15,17,38,19:37)]
  names(h2)[names(h2) == "LOAEL_less_serious"] <- "toxval_numeric"
  h2$toxval_type <- "LOAEL_less_serious"

  h3 <- res3[,c(1:6,8:9,11,13:15,18,38,19:37)]
  names(h3)[names(h3) == "LOAEL_serious"] <- "toxval_numeric"
  h3$toxval_type <- "LOAEL_serious"

  res3_new <- rbind(h1,h2,h3)
  names(res3_new)[names(res3_new) == "effect"] <- "critical_effect_original"
  res3_new$long_ref <-  gsub("(^[^[:alnum:]])(.*)","\\2",res3_new$long_ref)

  # subset by removing rows with empty numeric values
  res3_new <- res3_new[which(res3_new$toxval_numeric != ""),]
  # replace the occurance of sex info in numeric values
  res3_new[grep("[A-Za-z]+",res3_new$toxval_numeric),"toxval_numeric"] <- gsub("(.*?)(\\s+[A-Za-z]+.*)","\\1",res3_new[grep("[A-Za-z]+",res3_new$toxval_numeric),"toxval_numeric"])
  res3_new[grep("\\,",res3_new$toxval_numeric),"toxval_numeric"] <- gsub("(.*)(\\,)(.*)","\\1\\3",res3_new[grep("\\,",res3_new$toxval_numeric),"toxval_numeric"])

  #####################################################################
  cat("ATSDR_TP_2021_PFOA_Inhalation\n")
  #####################################################################
  res4 <- res[[4]][7:33,c(1:21)]
  name.list <- c("source_name_sid", "casrn","name","source_url","subsource","source_name_cid","key_to_figure","study_type", "short_ref", "short_name","species_strain","sex","duration","doses","parameters_monitored","endpoint","NOAEL","LOAEL_less_serious","LOAEL_serious","effect","long_ref")
  names(res4) <- name.list
  ###runInsertTable(res4,"atsdr_tp_2021_pfoa_inhalation",db,do.halt=T,verbose=F)

  # extract study type info from study_type
  res4$study_type_original <- res4$study_type
  res4$study_type <- tolower(res4$study_type)
  res4[is.na(res4$study_type),"study_type"] <- "-"

  # extract sps and strain info from species_strain
  res4$species_original <- res4$species_strain
  res4$species <- gsub("(.*?)(\\s+.*)","\\1",res4$species_strain)
  res4[is.na(res4$species),"species"] <- "-"

  res4$strain_original <- res4$species_strain
  res4$strain <- gsub("(.*?\\s+)(.*)","\\2",res4$species_strain)
  res4$strain <- gsub("^\\s+|\\s+$","",res4$strain)
  res4$strain <- gsub("(^\\()(.*)(\\)$)","\\2", res4$strain)
  res4[is.na(res4$strain),"strain"] <- "-"

  # extract sex info from sex original
  res4$sex_original <- res4$sex

  res4[grep("F", res4$sex, ignore.case = T),"sex"] <- "female(s)"
  res4[grep("M", res4$sex_original, ignore.case = T),"sex"] <- paste("M", res4[grep("M", res4$sex_original, ignore.case = T),"sex"] , sep = '/')
  res4$sex <- gsub("(^M)(\\/+.*M$)","\\1",res4$sex)
  res4$sex[res4$sex != "M"& res4$sex != "female(s)"& res4$sex != "M/female(s)"] <- "-"
  res4$sex[which(is.na(res4$sex))] <- "-"

  #extract document name from source url
  res4$document_name <- gsub("(.*\\/)(.*)","\\2", res4$source_url)

  #extract study duration value, units from duration

  res4$study_duration_value_original <- res4$duration
  res4$study_duration_units_original <- res4$duration
  res4[grep("^[0-9]+\\s+[A-Za-z]+\\s*.*",res4$duration),"study_duration_value"] <- gsub("(^[0-9]+)(\\s+[A-Za-z]+\\s*.*)","\\1",res4[grep("^[0-9]+\\s+[A-Za-z]+\\s*.*",res4$duration),"duration"])
  res4[grep("^[0-9]+\\s+[A-Za-z]+\\s*.*",res4$duration),"study_duration_units"] <- gsub("(^[0-9]+\\s+)([A-Za-z]+)(\\s*.*)","\\2",res4[grep("^[0-9]+\\s+[A-Za-z]+\\s*.*",res4$duration),"duration"])

  #fix GD
  res4[grep("^GDs",res4$duration),"study_duration_value"] <- gsub("(GDs)(\\s+)(\\d+\\-)(\\d+)(.*)","\\4",res4[grep("^GDs",res4$duration),"duration"])
  res4[grep("^GDs",res4$duration),"study_duration_units"] <- gsub("(GDs)(\\s+)(\\d+\\-)(\\d+)(.*)","\\1",res4[grep("^GDs",res4$duration),"duration"])

  res4$study_duration_units[which(is.na(res4$study_duration_units))] <- "-"
  res4$study_duration_value[which(is.na(res4$study_duration_value))] <- ""


  # change weeks and days plural to singular
  res4[grep("s$",res4$study_duration_units), "study_duration_units"] <- gsub("(.*)(s$)","\\1",res4[grep("s$",res4$study_duration_units), "study_duration_units"])


  # critical effect change na to hyphen
  res4$critical_effect <- res4$effect
  res4[which(is.na(res4$critical_effect)),"critical_effect"] <- "-"

  # create key desc table
  key <- res[[4]][35:39,7]
  key <- paste(key, collapse = " ")

  key_desc_table <- data.frame(unlist(stringr::str_split(key, ";")), stringsAsFactors = F)
  names(key_desc_table) <- "original_keys"
  key_desc <- data.frame(do.call('rbind', strsplit(as.character(key_desc_table$original_keys),'=',fixed=TRUE)), stringsAsFactors = F)
  rm(key_desc_table, key)

  names(key_desc) <- c("key","description")
  key_desc[nrow(key_desc) + 1,] = c("BW","body weight")
  key_desc[grep("Bd wt", key_desc$key),"key"] <- "Bd wt"

  key_desc[nrow(key_desc) + 1,] = c("HE","hematological")
  key_desc[grep("Hemato", key_desc$key),"key"] <- "Hemato"

  key_desc$key <- stringr::str_trim(as.character(key_desc[,"key"]))
  key_desc$description <- stringr::str_trim(as.character(key_desc[,"description"]))

  #### key replacements
  for(i in 1:nrow(key_desc)) {
    valold <- key_desc[i,1]
    valnew <- key_desc[i,2]
    res4[generics::is.element(res4$endpoint,valold),"endpoint"] <- valnew
  }

  res4[which(is.na(res4$endpoint)),"endpoint"] <- "-"

  for (i in 1:nrow(key_desc)){
    for ( j in 1:nrow(res4)){
      res4$parameters_monitored[j] <- stringr::str_replace_all(res4$parameters_monitored[j],paste("\\b",key_desc[,"key"],"\\b", sep  = "")[i],key_desc[,"description"][i])

    }
  }

  res4[which(is.na(res4$parameters_monitored)),"parameters_monitored"] <- "-"

  for (i in 1:nrow(key_desc)){
    for ( j in 1:nrow(res4)){
      res4$sex[j] <- stringr::str_replace_all(res4$sex[j],paste("\\b",key_desc[,"key"],"\\b", sep  = "")[i],key_desc[,"description"][i])

    }
  }

  res4[which(is.na(res4$sex)),"sex"] <- "-"

  # extract year info from short_ref
  res4$year <- gsub("(.*)([0-9]{4})(.*)","\\2",res4$short_ref)

  # assign toxval_units
  res4$toxval_units <- "mg/m3"

  #### combine NOAEL, serious and less serious LOAEL values to create toxval_numeric and toxval_type

  h1 <- res4[,c(1:10,12,14:16,17,35,20:34)]
  names(h1)[names(h1) == "NOAEL"] <- "toxval_numeric"
  h1$toxval_type <- "NOAEL"

  h2 <- res4[,c(1:10,12,14:16,18,35,20:34)]
  names(h2)[names(h2) == "LOAEL_less_serious"] <- "toxval_numeric"
  h2$toxval_type <- "LOAEL_less_serious"

  h3 <- res4[,c(1:10,12,14:16,19,35,20:34)]
  names(h3)[names(h3) == "LOAEL_serious"] <- "toxval_numeric"
  h3$toxval_type <- "LOAEL_serious"

  res4_new <- rbind(h1,h2,h3)
  names(res4_new)[names(res4_new) == "effect"] <- "critical_effect_original"
  res4_new$long_ref <-  gsub("(^[^[:alnum:]])(.*)","\\2",res4_new$long_ref)

  res4_new <- res4_new[which(res4_new$toxval_numeric != ""),]

  # replace the occurance of sex info in numeric values
  res4_new[grep("[A-Za-z]+",res4_new$toxval_numeric),"toxval_numeric"] <- gsub("(.*)(\\s+[A-Za-z]+.*)","\\1",res4_new[grep("[A-Za-z]+",res4_new$toxval_numeric),"toxval_numeric"])

  #####################################################################
  cat("ATSDR_TP_2021_PFOA_Oral\n")
  #####################################################################
  res5 <- res[[5]][8:232,c(1:22)]
  name.list <- c("source_name_sid", "casrn","name","source_url","subsource","source_name_cid","key_to_figure","study_type_exposure_route", "short_name", "short_ref","species_strain","sex","duration","doses","parameters_monitored","endpoint","NOAEL","LOAEL_less_serious","LOAEL_serious","effect","comments","long_ref")
  names(res5) <- name.list
  ###runInsertTable(res5,"atsdr_tp_2021_pfoa_oral",db,do.halt=T,verbose=F)

  # extract exposure_route and study type info from study_type_exposure_route
  res5$exposure_route_original <- res5$study_type_exposure_route
  res5$exposure_route <- gsub("(.*\\-\\s+)(.*)","\\2",res5$study_type_exposure_route)
  res5$study_type_original <- res5$study_type_exposure_route
  res5$study_type <- tolower(gsub("(.*)(\\s+\\-\\s+.*)","\\1",res5$study_type_exposure_route))
  # extract sps and strain info from species_strain
  res5$species_original <- res5$species_strain
  # sex and species values are flipped in row 210
  res5[grep("2\\-5 F", res5$species_strain),"species_original"] <- res5[grep("2\\-5 F", res5$species_strain),"sex"]
  res5[grep("2\\-5 F", res5$species_strain),"sex"] <- res5[grep("2\\-5 F", res5$species_strain),"species_strain"]
  res5$species <- gsub("(.*?)(\\s+.*)","\\1",res5$species_original)

  res5$strain_original <- res5$species_original
  res5$strain <- gsub("(.*?\\s+)(.*)","\\2",res5$species_original)
  res5$strain <- gsub("^\\s+|\\s+$","",res5$strain)
  res5$strain <- gsub("(^\\()(.*)(\\)$)","\\2", res5$strain)

  # extract sex info from sex original
  res5$sex_original <- res5$sex

  res5[grep("F", res5$sex, ignore.case = T),"sex"] <- "female(s)"
  res5[grep("M", res5$sex_original, ignore.case = T),"sex"] <- paste("M", res5[grep("M", res5$sex_original, ignore.case = T),"sex"] , sep = '/')
  res5$sex <- gsub("(^M)(\\/+.*M$)","\\1",res5$sex)
  res5$sex[res5$sex != "M"& res5$sex != "female(s)"& res5$sex != "M/female(s)"] <- "-"
  res5$sex[which(is.na(res5$sex))] <- "-"

  #extract document name from source url
  res5$document_name <- gsub("(.*\\/)(.*)","\\2", res5$source_url)

  #extract study duration value, units and exposure_method from duration
  res5[grep("\\(+",res5$duration),"exposure_method"] <- gsub("(.*\\s*)(\\([A-Z]+\\)*)(\\s*.*)","\\2",res5$duration[grep("\\(+",res5$duration)])
  # Add closing parentheses where needed
  res5[!grepl("\\)", res5$exposure_method) & !is.na(res5$exposure_method), "exposure_method"] <- paste0(res5[!grepl("\\)", res5$exposure_method) & !is.na(res5$exposure_method), "exposure_method"], ")")
  res5[is.na(res5$exposure_method), "exposure_method"] <- "-"
  res5$exposure_method_original <- res5$exposure_method

  # Remove exposure_method parentheses
  res5$study_duration_value_original <- gsub("\\s*\\([^\\)]+\\)","",res5$duration) %>%
    gsub("\\(GW", "", .) %>%
    gsub("\\(G", "",.)
  res5$study_duration_units_original <- res5$study_duration_value_original

  # 5 days/week 4 weeks starting at PND 21 (GW), 5 days/week 0, 5 4 weeks (GW)
  # dose assigned per original document - tp200-c2_T2-3.xlsx
  res5[grep("^5 days.*at PND 21 \\(GW\\)", res5$duration),"doses"] <- "0, 1, 5, 10"
  res5[grep("^5 days.*0\\, 5 4 weeks \\(GW\\)", res5$duration),"doses"] <- "0, 5"

  res5[grep("^5 days.*0\\, 5 4 weeks \\(GW\\)", res5$duration),"duration"] <- "5 days/week 4 weeks (GW)"
  res5[grep("^5 days\\/week 4 weeks.*", res5$duration),"study_duration_value"] <- 4
  res5[grep("^5 days\\/week 4 weeks.*", res5$duration),"study_duration_units"] <- "week"

  res5[grep("^[0-9]+\\s+[A-Za-z]+\\s+[^0-9].*",res5$duration),"study_duration_value"] <- gsub("(^[0-9]+)(\\s+[A-Za-z]+\\s+.*)","\\1",res5[grep("^[0-9]+\\s+[A-Za-z]+\\s+[^0-9].*",res5$duration),"duration"])
  res5[grep("^[0-9]+\\s+[A-Za-z]+\\s+[^0-9].*",res5$duration),"study_duration_units"] <- gsub("(^[0-9]+\\s+)([A-Za-z]+)(\\s+.*)","\\2",res5[grep("^[0-9]+\\s+[A-Za-z]+\\s+[^0-9].*",res5$duration),"duration"])

  # fix range
  res5[grep("^[0-9]+\\-[0-9]+",res5$duration),"study_duration_value"]<- gsub("(^[0-9]+\\-)([0-9]+)(\\s+)([A-Za-z]+)(\\s+.*)","\\2",res5[grep("^[0-9]+\\-[0-9]+",res5$duration),"duration"])
  res5[grep("^[0-9]+\\-[0-9]+",res5$duration),"study_duration_units"]<- gsub("(^[0-9]+\\-)([0-9]+)(\\s+)([A-Za-z]+)(\\s+.*)","\\4",res5[grep("^[0-9]+\\-[0-9]+",res5$duration),"duration"])

  #fix GD
  res5[grep("^GD.*\\-[0-9]+\\s+\\(.*\\)$",res5$duration),"study_duration_value"] <- gsub("(GDs)(.*\\-)([0-9]+)(.*)","\\3",res5[grep("^GD.*\\-[0-9]+\\s+\\(.*\\)$",res5$duration),"duration"])
  res5[grep("^GD.*\\-[0-9]+\\s+\\(.*\\)$",res5$duration),"study_duration_units"] <- gsub("(GDs)(.*\\-)([0-9]+)(.*)","\\1",res5[grep("^GD.*\\-[0-9]+\\s+\\(.*\\)$",res5$duration),"duration"])

  # GDs 1–17 1 time/day (GW) GD 7 to PND 22 (W); 3-generation study, GD 1 to PND 21 ad lib (W), GD 7 to PND 21 (F)
  res5[grep(".*PND [0-9]+.*",res5$duration), "study_duration_value"] <- gsub("(.*)(PND)(\\s+)([0-9]+)(.*)","\\4",res5[grep(".*PND [0-9]+.*",res5$duration), "duration"])
  res5[grep(".*PND [0-9]+.*",res5$duration), "study_duration_units"] <- gsub("(.*)(PND)(\\s+)([0-9]+)(.*)","\\2",res5[grep(".*PND [0-9]+.*",res5$duration), "duration"])

  #GDs 8-17; GDs 1217 (GW)
  res5[grep(".*1217.*",res5$duration), "study_duration_value"] <- gsub("(GDs)(\\s+[0-9]+\\-)([0-9]+)(\\;.*)","\\3",res5[grep(".*1217.*",res5$duration), "duration"])
  res5[grep(".*1217.*",res5$duration), "study_duration_units"] <- gsub("(GDs)(\\s+[0-9]+\\-)([0-9]+)(\\;.*)","\\1",res5[grep(".*1217.*",res5$duration), "duration"])

  # fix once
  res5[grep("^Once\\s+\\(.*\\)$",res5$duration),"study_duration_value"] <- 1
  res5[grep("^Once\\s+\\(.*\\)$",res5$duration),"study_duration_units"] <- "days"

  # time/day
  res5[is.na(res5$study_duration_value), "duration"] <- gsub("(.*)(1 time/day)(.*)","\\1\\3",res5[is.na(res5$study_duration_value), "duration"])
  res5[is.na(res5$study_duration_value), "study_duration_value"] <- sapply(stringr::str_extract_all(res5[is.na(res5$study_duration_value), "duration"], "\\d+"), function(x) max(as.numeric(x)))
  res5[res5$duration %in% grep("^GD",res5[is.na(res5$study_duration_units), "duration"], value = T), "study_duration_units"] <- "GD"
  res5[res5$duration %in% grep("day",res5[is.na(res5$study_duration_units), "duration"], value = T), "study_duration_units"] <- "day"
  res5[res5$duration %in% grep("week",res5[is.na(res5$study_duration_units), "duration"], value = T), "study_duration_units"] <- "week"

  res5$study_duration_value <- as.character(res5$study_duration_value)
  # change weeks and days plural to singular
  res5[grep("s$",res5$study_duration_units), "study_duration_units"] <- gsub("(.*)(s$)","\\1",res5[grep("s$",res5$study_duration_units), "study_duration_units"])

  # critical effect change na to hyphen
  res5$critical_effect <- res5$effect
  res5[which(is.na(res5$critical_effect)),"critical_effect"] <- "-"

  # create key desc table
  key <- res[[5]][235:244,7]
  key <- paste(key, collapse = " ")

  key_desc_table <- data.frame(unlist(stringr::str_split(key, ";")), stringsAsFactors = F)
  names(key_desc_table) <- "original_keys"
  key_desc <- data.frame(do.call('rbind', strsplit(as.character(key_desc_table$original_keys),'=',fixed=TRUE)), stringsAsFactors = F)
  rm(key_desc_table, key)

  names(key_desc) <- c("key","description")
  key_desc[nrow(key_desc) + 1,] = c("BW","body weight")
  key_desc[grep("Bd wt", key_desc$key),"key"] <- "Bd wt"

  key_desc[nrow(key_desc) + 1,] = c("HE","hematological")
  key_desc[grep("Hemato", key_desc$key),"key"] <- "Hemato"

  key_desc$key <- stringr::str_trim(as.character(key_desc[,"key"]))
  key_desc$description <- stringr::str_trim(as.character(key_desc[,"description"]))

  #### key replacements
  res5$endpoint <- gsub("^\\s+|\\s+$","",res5$endpoint)

  for(i in 1:nrow(key_desc)) {
    valold <- key_desc[i,1]
    valnew <- key_desc[i,2]
    res5[generics::is.element(res5$endpoint,valold),"endpoint"] <- valnew
  }

  res5$parameters_monitored <- gsub("^\\s+|\\s+$","",res5$parameters_monitored)

  for (i in 1:nrow(key_desc)){
    for ( j in 1:nrow(res5)){
      res5$parameters_monitored[j] <- stringr::str_replace_all(res5$parameters_monitored[j],paste("\\b",key_desc[,"key"],"\\b", sep  = "")[i],key_desc[,"description"][i])

    }
  }

  for (i in 1:nrow(key_desc)){
    for ( j in 1:nrow(res5)){
      res5$sex[j] <- stringr::str_replace_all(res5$sex[j],paste("\\b",key_desc[,"key"],"\\b", sep  = "")[i],key_desc[,"description"][i])

    }
  }

  for (i in 1:nrow(key_desc)){
    for ( j in 1:nrow(res5)){
      res5$exposure_method[j] <- stringr::str_replace_all(res5$exposure_method[j],paste("\\b",key_desc[,"key"],"\\b", sep  = "")[i],key_desc[,"description"][i])

    }
  }

  res5$exposure_method[grep("^\\(+.*\\)+$", res5$exposure_method)] <- gsub("(^\\(+)(.*)(\\)+)","\\2",res5$exposure_method[grep("^\\(+.*\\)+$", res5$exposure_method)])

  # extract year info from short_ref
  res5$year <- gsub("(.*)([0-9]{4})(.*)","\\2",res5$short_ref)

  # assign toxval_units
  res5$toxval_units <- "mg/kg/day"

  # fix long ref
  res5$long_ref <- gsub("(.*)(\\.$)","\\1", res5$long_ref)
  res5$long_ref <- gsub("(^\\+)(.*)","\\2",res5$long_ref)

  short_ref_key <- unique(res5[which(is.na(res5$long_ref)),"short_ref"])
  short_ref_key_table <- data.frame(short_ref_key, stringsAsFactors = "F")
  short_ref_key_table$description <- "-"

  for (i in 1:23){
    short_ref_key_table$description[i] <-unique(stats::na.omit(res5[which(res5$short_ref %in% short_ref_key_table$short_ref_key[i]),"long_ref"]))
  }

  # no long ref corresponding to short ref
  short_ref_key_table$description[24] <- "-"

  for (i in 25:40){
    short_ref_key_table$description[i] <-unique(stats::na.omit(res5[which(res5$short_ref %in% short_ref_key_table$short_ref_key[i]),"long_ref"]))
  }

  for(i in 1:nrow(short_ref_key_table)) {
    valold <- short_ref_key_table[i,1]
    valnew <- short_ref_key_table[i,2]
    res5[generics::is.element(res5$short_ref,valold),"long_ref"] <- valnew
  }

  names(res5)[names(res5) == "comments"] <- "toxval_details"

  #### combine NOAEL, serious and less serious LOAEL values to create toxval_numeric and toxval_type
  h1 <- res5[,c(1:7,9:10,12,14:16,17,41,20:40)]
  names(h1)[names(h1) == "NOAEL"] <- "toxval_numeric"
  h1$toxval_type <- "NOAEL"

  h2 <- res5[,c(1:7,9:10,12,14:16,18,41,20:40)]
  names(h2)[names(h2) == "LOAEL_less_serious"] <- "toxval_numeric"
  h2$toxval_type <- "LOAEL_less_serious"

  h3 <- res5[,c(1:7,9:10,12,14:16,19,41,20:40)]
  names(h3)[names(h3) == "LOAEL_serious"] <- "toxval_numeric"
  h3$toxval_type <- "LOAEL_serious"

  res5_new <- rbind(h1,h2,h3)
  names(res5_new)[names(res5_new) == "effect"] <- "critical_effect_original"

  # subset by removing rows with empty numeric values
  res5_new <- res5_new[which(res5_new$toxval_numeric != ""),]
  # replace the occurance of sex info in numeric values
  res5_new[grep("M|F|\\(b\\)",res5_new$toxval_numeric),"toxval_numeric"] <- gsub("(.*?)(\\s+\\(*[A-Za-z]+.*)","\\1",res5_new[grep("M|F|\\(b\\)",res5_new$toxval_numeric),"toxval_numeric"])
  res5_new[grep("(.*)(\\,)(.*)",res5_new$toxval_numeric),"toxval_numeric"] <- gsub("(.*)(\\,)(.*)","\\1\\3",res5_new[grep("(.*)(\\,)(.*)",res5_new$toxval_numeric),"toxval_numeric"])

  #####################################################################
  cat("ATSDR_TP_2021_PFOS_Oral\n")
  #####################################################################
  res6 <- res[[6]][8:201,c(1:22)]
  name.list <- c("source_name_sid", "casrn","name","source_url","subsource","source_name_cid","key_to_figure","study_type_exposure_route", "short_name", "short_ref","species_strain","sex","duration","doses","parameters_monitored","endpoint","NOAEL","LOAEL_less_serious","LOAEL_serious","effect","long_ref","comments")
  names(res6) <- name.list
  ###runInsertTable(res6,"atsdr_tp_2021_pfos_oral",db,do.halt=T,verbose=F)

  # extract exposure_route and study type info from study_type_exposure_route
  res6$exposure_route_original <- res6$study_type_exposure_route
  res6$exposure_route <- gsub("(.*\\-\\s+)(.*)","\\2",res6$study_type_exposure_route)
  res6$study_type_original <- res6$study_type_exposure_route
  res6$study_type <- tolower(gsub("(.*)(\\s+\\-\\s+.*)","\\1",res6$study_type_exposure_route))
  # extract sps and strain info from species_strain
  res6$species_original <- res6$species_strain
  res6$species <- res6$species_original
  # name and key to figure info in species column value
  res6$species <- gsub("(potassium salt 10\\s+)(.*)","\\2",res6$species)
  res6$species <- gsub("(.*?)(\\s+\\(.*)","\\1",res6$species)

  res6$strain_original <- res6$species_original
  res6$strain <- gsub("(.*\\s+)(\\(.*)","\\2",res6$species_original)
  res6$strain <- gsub("(^\\()(.*)(\\)$)","\\2", res6$strain)
  res6$strain <- gsub("(^\\()(.*)","\\2",res6$strain)

  # extract sex info from sex original
  res6$sex_original <- res6$sex

  res6[grep("F", res6$sex, ignore.case = T),"sex"] <- "female(s)"
  res6[grep("M", res6$sex_original, ignore.case = T),"sex"] <- paste("M", res6[grep("M", res6$sex_original, ignore.case = T),"sex"] , sep = '/')
  res6$sex <- gsub("(^M)(\\/+.*M$)","\\1",res6$sex)
  res6$sex[res6$sex != "M"& res6$sex != "female(s)"& res6$sex != "M/female(s)"] <- "-"
  res6$sex[which(is.na(res6$sex))] <- "-"

  #extract document name from source url
  res6$document_name <- gsub("(.*\\/)(.*)","\\2", res6$source_url)

  #extract study duration value, units and exposure_method from duration
  res6[grep("\\(+",res6$duration),"exposure_method"] <- gsub("(.*\\s*)(\\([A-Z]+\\)*)(\\s*.*)","\\2",res6$duration[grep("\\(+",res6$duration)])
  # Add closing parentheses where needed
  res6[!grepl("\\)", res6$exposure_method) & !is.na(res6$exposure_method), "exposure_method"] <- paste0(res6[!grepl("\\)", res6$exposure_method) & !is.na(res6$exposure_method), "exposure_method"], ")")
  res6[is.na(res6$exposure_method), "exposure_method"] <- "-"
  res6$exposure_method_original <- res6$exposure_method

  res6$study_duration_value_original <- gsub("\\s*\\([^\\)]+\\)","",res6$duration) %>%
    gsub("\\(G", "",.)
  res6$study_duration_units_original <- res6$study_duration_value_original

  # extract the maximum duration value
  res6[grep("[0-9]+",res6$duration),"study_duration_value"] <- sapply(stringr::str_extract_all(res6[grep("[0-9]+",res6$duration),"duration"], "\\d+"), function(x) max(as.numeric(x)))
  # Once and Single dose values
  res6[grep("^Once \\(.*\\)$|^Single dose \\(.*\\)$", res6$duration),"study_duration_value"] <- 1
  res6[grep("^Once \\(.*\\)$|^Single dose \\(.*\\)$", res6$duration),"study_duration_units"] <- "day"
  # assign GD units
  res6[grep("GDs",res6$duration),"study_duration_units"] <- "GD"
  res6[grep("^[0-9\\-]+\\s+week",res6$duration),"study_duration_units"] <- "week"
  # assign day units
  res6[grep("^[0-9\\-]+\\s+day[s]*\\s+\\(.*|^[0-9\\-]+\\s+day\\s+1 time/day",res6$duration),"study_duration_units"] <- "day"

  # assign PND units
  res6[grep("^GD .*PND",res6$duration),"study_duration_units"] <- "PND"

  # assign month units
  res6[grep("month",res6$duration),"study_duration_units"] <- "month"

  # assign remaing day values
  res6[is.na(res6$study_duration_units), "study_duration_units"] <- "day"

  res6$study_duration_value <- as.character(res6$study_duration_value)

  # critical effect change na to hyphen
  res6$critical_effect <- res6$effect
  res6[which(is.na(res6$critical_effect)),"critical_effect"] <- "-"

  # create key desc table
  key <- res[[6]][206:215,7]
  key <- paste(key, collapse = " ")

  key_desc_table <- data.frame(unlist(stringr::str_split(key, ";")), stringsAsFactors = F)
  names(key_desc_table) <- "original_keys"
  key_desc <- data.frame(do.call('rbind', strsplit(as.character(key_desc_table$original_keys),'=',fixed=TRUE)), stringsAsFactors = F)
  rm(key_desc_table, key)

  names(key_desc) <- c("key","description")
  key_desc[nrow(key_desc) + 1,] = c("BW","body weight")
  key_desc[grep("Bd wt", key_desc$key),"key"] <- "Bd wt"

  key_desc[nrow(key_desc) + 1,] = c("HE","hematological")
  key_desc[grep("Hemato", key_desc$key),"key"] <- "Hemato"

  key_desc$key <- stringr::str_trim(as.character(key_desc[,"key"]))
  key_desc$description <- stringr::str_trim(as.character(key_desc[,"description"]))

  #### key replacements
  res6$endpoint <- gsub("^\\s+|\\s+$","",res6$endpoint)

  for(i in 1:nrow(key_desc)) {
    valold <- key_desc[i,1]
    valnew <- key_desc[i,2]
    res6[generics::is.element(res6$endpoint,valold),"endpoint"] <- valnew
  }

  res6$parameters_monitored <- gsub("^\\s+|\\s+$","",res6$parameters_monitored)

  for (i in 1:nrow(key_desc)){
    for ( j in 1:nrow(res6)){
      res6$parameters_monitored[j] <- stringr::str_replace_all(res6$parameters_monitored[j],paste("\\b",key_desc[,"key"],"\\b", sep  = "")[i],key_desc[,"description"][i])
    }
  }

  for (i in 1:nrow(key_desc)){
    for ( j in 1:nrow(res6)){
      res6$sex[j] <- stringr::str_replace_all(res6$sex[j],paste("\\b",key_desc[,"key"],"\\b", sep  = "")[i],key_desc[,"description"][i])
    }
  }

  for (i in 1:nrow(key_desc)){
    for ( j in 1:nrow(res6)){
      res6$exposure_method[j] <- stringr::str_replace_all(res6$exposure_method[j],paste("\\b",key_desc[,"key"],"\\b", sep  = "")[i],key_desc[,"description"][i])

    }
  }

  res6$exposure_method[grep("^\\(+.*\\)+$", res6$exposure_method)] <- gsub("(^\\(+)(.*)(\\)+)","\\2",res6$exposure_method[grep("^\\(+.*\\)+$", res6$exposure_method)])

  # extract year info from short_ref
  res6$year <- gsub("(.*)([0-9]{4})(.*)","\\2",res6$short_ref)

  # assign toxval_units
  res6$toxval_units <- "mg/kg/day"

  # fix long ref
  # remove leading plus signs and trailing periods from long ref
  res6$long_ref <- gsub("(.*)(\\.$)","\\1", res6$long_ref)
  res6$long_ref <- gsub("(^\\+)(.*)","\\2",res6$long_ref)

  short_ref_key <- unique(res6[which(is.na(res6$long_ref)),"short_ref"])
  short_ref_key_table <- data.frame(short_ref_key, stringsAsFactors = "F")
  short_ref_key_table$description <- "-"

  for (i in 1:37){
    short_ref_key_table$description[i] <-unique(stats::na.omit(res6[which(res6$short_ref %in% short_ref_key_table$short_ref_key[i]),"long_ref"]))
  }

  for(i in 1:nrow(short_ref_key_table)) {
    valold <- short_ref_key_table[i,1]
    valnew <- short_ref_key_table[i,2]
    res6[generics::is.element(res6$short_ref,valold),"long_ref"] <- valnew
  }

  names(res6)[names(res6) == "comments"] <- "toxval_details"

  #### combine NOAEL, serious and less serious LOAEL values to create toxval_numeric and toxval_type

  h1 <- res6[,c(1:7,9:10,12,14:16,17,41,20:40)]
  names(h1)[names(h1) == "NOAEL"] <- "toxval_numeric"
  h1$toxval_type <- "NOAEL"

  h2 <- res6[,c(1:7,9:10,12,14:16,18,41,20:40)]
  names(h2)[names(h2) == "LOAEL_less_serious"] <- "toxval_numeric"
  h2$toxval_type <- "LOAEL_less_serious"

  h3 <- res6[,c(1:7,9:10,12,14:16,19,41,20:40)]
  names(h3)[names(h3) == "LOAEL_serious"] <- "toxval_numeric"
  h3$toxval_type <- "LOAEL_serious"

  res6_new <- rbind(h1,h2,h3)
  names(res6_new)[names(res6_new) == "effect"] <- "critical_effect_original"

  # subset by removing rows with empty numeric values
  res6_new <- res6_new[which(res6_new$toxval_numeric != ""),]
  # replace the occurance of sex info in numeric values
  res6_new[grep("M|F|\\(b\\)",res6_new$toxval_numeric),"toxval_numeric"] <- gsub("(.*?)(\\s+\\(*[A-Za-z]+.*)","\\1",res6_new[grep("M|F|\\(b\\)",res6_new$toxval_numeric),"toxval_numeric"])
  # cross over data from endpoint field to toxval field
  res6_new[grep("Develop",res6_new$toxval_numeric),"endpoint"] <- "developmental"
  res6_new[grep("Develop",res6_new$toxval_numeric),"toxval_numeric"] <- ""

  #####################################################################
  cat("Build combined dataframe of all atsdr pfas 2021 sources \n")
  #####################################################################
  new_res <- dplyr::bind_rows(res1_new,res2_new,res3_new,res4_new,res5_new,res6_new)
  #write.xlsx(new_res, "atsdr_pfas_2021.xlsx")
  new_res$toxval_numeric <- as.numeric(new_res$toxval_numeric)
  new_res$study_duration_value <- as.numeric(new_res$study_duration_value)
  new_res$year <- as.integer(new_res$year)

  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="ATSDR PFAS 2021",table="source_atsdr_pfas_2021",res=new_res,F,T,T)
}

