# library("openxlsx")
# library(tibble)
# library(janitor)
# library(stringr)

#--------------------------------------------------------------------------------------
#' @description Load ECHA TSCA POC Source into dev_toxval_source_v4.
#' @param toxval.db The version of toxval into which the source is loaded.
#' @param infile The input file ./echa3/echa3_files/TSCA_POC_Chemical_Results_081220.xlsx
#' @param dict_toxval_units The input file ./echa3/echa3_files/toxval_units_dictionary.xlsx
#' @param dict_toxval_type The input file ./echa3/echa3_files/toxval_type_dictionary.xlsx
#' @param dict_study_type The input file ./echa3/echa3_files/study_type_dictionary.xlsx
#' @param dict_exposure_route The input file ./echa3/echa3_files/exposure_route_dictionary.xlsx
#' @param dict_exposure_method The input file ./echa3/echa3_files/exposure_method_dictionary.xlsx
#' @param dict_study_duration The input file ./echa3/echa3_files/study_duration_dictionary.xlsx
#' @param dict_species The input file ./echa3/echa3_files/echa3_species_dict.xlsx
#' @param dict_critical_effects The input file ./echa3/echa3_files/critical_effect_dictionary.xlsx
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
#'  \code{\link[janitor]{excel_numeric_to_date}}
#'  \code{\link[stringr]{str_extract}}, \code{\link[stringr]{modifiers}}, \code{\link[stringr]{str_split}}
#' @rdname import_echa3_source
#' @importFrom openxlsx read.xlsx
#' @importFrom janitor excel_numeric_to_date
#' @importFrom stringr str_extract_all regex str_split

#--------------------------------------------------------------------------------------
import_echa3_source <- function(toxval.db,infile, dict_toxval_units, dict_toxval_type,dict_study_type,
                                dict_exposure_route, dict_exposure_method,dict_study_duration,
                                dict_species, dict_critical_effects) {
  printCurrentFunction(toxval.db)

  #####################################################################
  cat("Build original_echa3 table\n")
  #####################################################################

  res <- openxlsx::read.xlsx(infile,2)
  dim(res)

  res$Publication_FirstSubmitted <- janitor::excel_numeric_to_date(as.numeric(as.character(res$Publication_FirstSubmitted)), date_system = "modern")
  res$Publication_FirstSubmitted <- format(res$Publication_FirstSubmitted, format = "%d-%b-%y")

  res$Publication_LastUpdate <- janitor::excel_numeric_to_date(as.numeric(as.character(res$Publication_LastUpdate)), date_system = "modern")
  res$Publication_LastUpdate <- format(res$Publication_LastUpdate, format = "%d-%b-%y")

  #runInsertTable(res,"original_echa3",toxval.db,do.halt=T,verbose=F)

  #####################################################################
  cat("extract relevent fields and process the data to fit toxval standards\n")
  #####################################################################

  res1 <- res
  res1$toxval_numeric_original <- res1$Results_EffectLevel
  res1$toxval_units_original <- res1$Results_EffectLevel
  res1$toxval_numeric_qualifier_original <- res1$Results_EffectLevel
  res1$toxval_type_original <- res1$Results_DoseDescriptor
  res1$study_type_original <- res1$AdminData_Endpoint
  res1$exposure_route_original <- res1$MaterialsMethods_Exposure_Route
  res1$exposure_method_original <- res1$MaterialsMethods_Exposure_Route
  res1$study_duration_value_original <- res1$MaterialsMethods_Exposure_Duration
  res1$study_duration_units_original <- res1$MaterialsMethods_Exposure_Duration
  res1$species_original <- res1$MaterialsMethods_TestAnimals_Species
  res1$strain_original <- res1$MaterialsMethods_TestAnimals_Strain
  res1$critical_effect_original <- res1$Results_Basis


  Data_type_list <- unique(res1$AdminData_Type)

  # keep only experimental study
  res1 <- res1[grep( "experimental study",res1$AdminData_Type, ignore.case = T) ,]
  ### Remove cell culture studies and genotoxicity studies
  endpoint_types <- unique(res1$AdminData_Endpoint)
  endpoint_excludes <- unique(grep("\\bcell\\b|\\bgene\\b|\\bcells\\b|\\bgenetic\\b|\\bgeno|\\bDNA\\b|cytogenicity", res1$AdminData_Endpoint, ignore.case = T, value = T))

  res1 <- res1[grep("\\bcell\\b|\\bgene\\b|\\bcells\\b|\\bgenetic\\b|\\bgeno|\\bDNA\\b|cytogenicity",res1$AdminData_Endpoint, ignore.case = T, invert = T),]

  res1 <- data.frame(lapply(res1, function(x) {gsub("µ","u",x)}),stringsAsFactors = F)

  #####################################################################
  cat("fix toxval numeric, toxval units and toxval numeric qualifier\n")
  #####################################################################

  quali_vals <- grep("^\\d+" ,res1$Results_EffectLevel, invert = T)
  res1[quali_vals,"toxval_numeric_qualifier"] <- gsub("\\s.*", "", res1[quali_vals, "Results_EffectLevel"])

  uniq_quali <- unique(res1$toxval_numeric_qualifier[!is.na(res1$toxval_numeric_qualifier)])

  res1$toxval_numeric <-  gsub(paste("^",uniq_quali, collapse = "|", sep = ""), "", res1$Results_EffectLevel )
  res1$toxval_numeric <- gsub("^\\s+","", res1$toxval_numeric)
  res1$toxval_numeric <- gsub("(\\d+)(\\s*)(.*\\d+)(\\s+\\w+.*)", "\\1\\3\\4", res1$toxval_numeric)

  res1$toxval_units <- gsub("(.*\\d+)(\\s*\\w+.*)","\\2", res1$toxval_numeric)
  res1$toxval_numeric <- gsub("(.*\\d+)(\\s*\\w+.*)","\\1", res1$toxval_numeric)

  unit_superscripts <-  grep("[a-zA-Z]+", res1$toxval_numeric)
  res1[unit_superscripts,"toxval_units"] <-  gsub("(\\d+\\.*\\d*)(\\s*.*)","\\2", res1[unit_superscripts,"toxval_numeric"])
  res1[unit_superscripts,"toxval_numeric"] <-  gsub("(\\d+\\.*\\d*)(\\s*.*)","\\1", res1[unit_superscripts,"toxval_numeric"])

  non_num_toxval <- grep("[a-zA-Z]+",res1$toxval_numeric)
  res1[non_num_toxval,"toxval_units"] <- res1[non_num_toxval,"Results_EffectLevel"]
  res1$toxval_units <- gsub("^\\s+|\\s+$", "", res1$toxval_units)
  # flag non numeric toxval numeric values as -999
  res1[non_num_toxval, "toxval_numeric"] <- -999

  non_alnum_char <- grep("[^a-zA-Z0-9.(-999)]", res1$toxval_numeric)

  res1[non_alnum_char, "toxval_numeric"] <-  gsub("(\\d+\\.*\\d*)([^[:alnum:]]+.*)","\\1",res1$toxval_numeric[non_alnum_char] )

  only_num_tox <- grep("^\\d+$", res1$Results_EffectLevel)

  res1[only_num_tox, "toxval_numeric"] <- res1[only_num_tox, "Results_EffectLevel"]
  # toxval values with only numeric elements and does not have units are flagged as other: no units
  res1[only_num_tox, "toxval_units"] <- rep("other: no units", length(only_num_tox))

  num_tox_units <- grep("^\\d+|^[^[:alnum:]]",res1$toxval_units)
  res1[num_tox_units,"toxval_numeric"] <- gsub("(^[^0-9]+)(\\s*.*)","\\2", res1[num_tox_units,"Results_EffectLevel"])
  res1[num_tox_units,"toxval_numeric"] <- gsub("(\\s+\\-.*)", "", res1[num_tox_units,"toxval_numeric"])
  res1[num_tox_units,"toxval_numeric"] <- gsub("(.*and\\s+)", "", res1[num_tox_units,"toxval_numeric"])
  res1[num_tox_units,"toxval_numeric"] <- gsub("(\\d+|\\))(\\s+)([a-zA-Z]+.*)", "\\1", res1[num_tox_units,"toxval_numeric"])
  res1[num_tox_units,"toxval_numeric"] <- gsub("\\s+", "", res1[num_tox_units,"toxval_numeric"])
  res1$toxval_numeric[grep("\\^", res1$toxval_numeric)] <-  "1e-09"

  res1[num_tox_units,"toxval_units"] <- gsub("(.*[^a-zA-Z]+\\d+)(\\s+)([a-zA-Z]+.*)", "\\3", res1[num_tox_units,"Results_EffectLevel"])
  res1[num_tox_units,"toxval_units"] <- gsub(paste(unique(res1$toxval_numeric_qualifier[!is.na(res1$toxval_numeric_qualifier)]), collapse = "|"), "",res1[num_tox_units,"toxval_units"] )
  res1[num_tox_units,"toxval_units"] <-gsub("^\\s*\\d+\\s*\\.*\\d*","", res1[num_tox_units,"toxval_units"] )
  res1[num_tox_units,"toxval_units"] <-gsub(".*\\-.*\\d+\\)*\\s*\\d*","",res1[num_tox_units,"toxval_units"] )
  res1$toxval_units[res1[,"toxval_units"] %in% ""]  <- NA
  other_units <- grep("other:", res1$Results_EffectLevel)
  res1[other_units,"toxval_units"] <- gsub("(.*\\s+)(other:.*)", "\\2", res1[other_units,"Results_EffectLevel"])
  bw_units <- grep("^\\s*bw$", res1$toxval_units)
  res1[bw_units, "toxval_units"] <- gsub("(.*\\d+\\s+)([a-zA-Z]+\\/.*)", "\\2", res1[bw_units,"Results_EffectLevel"])

  bw_per_day_units <- grep("^\\s*bw\\/day", res1$toxval_units)
  res1[bw_per_day_units, "toxval_units"] <-gsub("(.*\\d+\\s+)([a-zA-Z]+\\/.*)", "\\2", res1[bw_per_day_units,"Results_EffectLevel"])
  # remove other: prefix from toxval units
  res1$toxval_units <- gsub("other:\\s*", "", res1$toxval_units)
  ppm_nominal_units <- grep("\\)\\s+\\(", res1$toxval_units)
  #res1[ppm_nominal_units,"toxval_units"]
  res1[ppm_nominal_units,"toxval_units"] <- gsub("(.*\\))(\\s+\\(.*)","\\1", res1[ppm_nominal_units,"toxval_units"] )


  res1$toxval_numeric <- as.numeric(res1$toxval_numeric)
  # fix entries with just unit and no value
  #res1[grep("mg/kg", res1$toxval_numeric_qualifier), "Results_EffectLevel"]
  unit_in_quali <- grep("mg/kg", res1$toxval_numeric_qualifier)
  res1[unit_in_quali, "toxval_numeric_qualifier"] <- NA

  res1[grep("^\\(.*\\)$", res1$toxval_units),"toxval_units"] <- gsub("(^\\()(.*)(\\)$)", "\\2",res1[grep("^\\(.*\\)$", res1$toxval_units),"toxval_units"])
  res1[grep("10\\^\\(\\-9\\).*", res1$toxval_units),"toxval_units"] <- gsub("(10\\^\\(\\-9\\)\\s+)(.*)","\\2", res1[grep("10\\^\\(\\-9\\).*", res1$toxval_units),"toxval_units"])

  res1[grep("mg\\/kg\\s*\\(bw\\).*", res1$toxval_units),"toxval_units"] <- gsub("(mg\\/kg\\s*)(\\(bw\\))(.*\\s+.*)","\\1\\2", res1[grep("mg\\/kg\\s*\\(bw\\).*", res1$toxval_units),"toxval_units"])
  res1[grep("mg\\/kg\\s*\\(bw\\)", res1$toxval_units),"toxval_units"] <- gsub("(mg\\/kg\\s*)(\\()(bw)(\\))","\\1\\3", res1[grep("mg\\/kg\\s*\\(bw\\)", res1$toxval_units),"toxval_units"])
  #res1[grep("\\(.*\\)", res1$toxval_units),"toxval_units"] <- gsub("(.*)(\\s*\\(.*\\))","\\1",res1[grep("\\(.*\\)", res1$toxval_units),"toxval_units"])

  res1[grep("\\(\\d+|\\d+\\.\\d+\\w+\\/*\\w*\\)", res1$toxval_units),"toxval_units"]<- gsub("(.*)(\\s*\\(.*\\))","\\1",res1[grep("\\(\\d+|\\d+\\.\\d+\\w+\\/*\\w*\\)", res1$toxval_units),"toxval_units"])

  res1[which(res1$toxval_units == ""), "toxval_units"]  <- "-"

  dict <- openxlsx::read.xlsx(dict_toxval_units)

  x <- unique(res1$toxval_units)
  x <- x[!is.na(x)]

  x <- x[!generics::is.element(x,dict[,1])]
  cat("   missing values in dictionary:",length(x),"\n")

  if(length(x)>0) browser()

  for(i in 1:nrow(dict)) {
    valold <- dict[i,1]
    valnew <- dict[i,2]
    res1[generics::is.element(res1$toxval_units,valold),"toxval_units"] <- valnew
  }


  res1[which(is.na(res1$toxval_units)|(res1$toxval_units == "")), "toxval_units"] <- "-"
  res1[which(is.na(res1$toxval_numeric_qualifier)|(res1$toxval_numeric_qualifier == "")), "toxval_numeric_qualifier"] <- "-"

  #####################################################################
  cat("create source and subsource fields\n")
  #####################################################################

  res1$source <- "ECHA3"
  res1$subsource <- "TSCA POC ECHA"

  #####################################################################
  cat("fix toxval type\n")
  #####################################################################

  res1$toxval_type <- res1$Results_DoseDescriptor
  # Remove other: prefix from types
  other_tox_type <- grep("^other:", res1$toxval_type)
  res1[other_tox_type, "toxval_type"] <- gsub("^other:\\s*", "", res1[other_tox_type, "toxval_type"])

  MTC_values<- grep("max.*tol.*conc.*|max.*tol.*dose.*", res1$toxval_type,  ignore.case = T, value = T)
  res1$toxval_type <-  gsub(paste(MTC_values,collapse = "|"),"MTC", res1$toxval_type)

  type_extract <- grep("[A-Z]{2,}", res1$toxval_type)

  res1[type_extract,"toxval_type"] <- sapply(stringr::str_extract_all(res1[type_extract,"toxval_type"], "\\b[A-Z]{2,}[a-z]*\\s*\\d*"), paste, collapse= ' ')

  res1$toxval_type <- gsub("^\\s+|\\s+$", "", res1$toxval_type)

  res1[grep("[A-Z]+\\s+[A-Z]+\\s+[A-Z]", res1$toxval_type),"toxval_type"] <- res1[grep("[A-Z]+\\s+[A-Z]+\\s+[A-Z]", res1$toxval_type),"Results_DoseDescriptor"]
  res1[grep("ELISA", res1$toxval_type),"toxval_type"] <- res1[grep("ELISA", res1$toxval_type),"Results_DoseDescriptor"]
  res1[which(res1$toxval_type == ""), "toxval_type"]  <- "-"

  dict <- openxlsx::read.xlsx(dict_toxval_type)

  x <- unique(res1$toxval_type)
  x <- x[!is.na(x)]
  x <- x[!generics::is.element(x,dict[,"toxval_type"])]
  cat("   missing values in dictionary:",length(x),"\n")

  if(length(x)>0) browser()

  for(i in 1:nrow(dict)) {
    valold <- dict[i,"toxval_type"]
    valnew <- dict[i,"toxval_type_new"]
    res1[generics::is.element(res1$toxval_type,valold),"toxval_type"] <- valnew
  }

  res1[which(is.na(res1$toxval_type)|(res1$toxval_type == "")), "toxval_type"] <- "-"

  #####################################################################
  cat("fix study type\n")
  #####################################################################

  res1$study_type <- res1$AdminData_Endpoint

  common_types <- grep("chronic|acute|repeat.*dose|short-term", res1$study_type, ignore.case = T)
  res1[common_types, "study_type"] <- gsub("(.*\\s+)(toxicity)(\\:.*)","\\1",res1[common_types, "study_type"])
  res1[common_types, "study_type"] <- gsub("([a-zA-Z]+)(\\:\\s+)([a-zA-Z]+\\s+)([a-zA-Z]+)","\\3\\1",res1[common_types, "study_type"])
  res1[common_types, "study_type"] <- gsub("\\s+$","", res1[common_types, "study_type"])
  res1[common_types, "study_type"] <- gsub("([a-zA-Z]+)(\\:\\s+)([a-zA-Z]+\\-[a-zA-Z]+\\s+)([a-zA-Z]+)","\\3\\1",res1[common_types, "study_type"])

  repro_types <- grep("reproductive", res1$study_type, ignore.case = T)
  res1[repro_types, "study_type"] <- gsub("(.*)(reproductive\\s+.*)(toxicity)(.*)","\\2\\3",res1[repro_types, "study_type"])

  tox_types <- grep("toxicity", res1$study_type, ignore.case = T)
  res1[tox_types, "study_type"] <- gsub("\\s+toxicity", "", res1[tox_types, "study_type"])

  res1$study_type <- gsub("toxicity to reproduction", "reproductive", res1$study_type)

  metadata_study_types <- grep("\\:", res1$study_type)
  res1[metadata_study_types, "study_type"] <- gsub(":.*$","", res1[metadata_study_types, "study_type"])

  res1$study_type <- gsub("short-term repeated dose", "short-term", res1$study_type)
  res1$study_type <- gsub("repeated dose", "repeat dose", res1$study_type)

  res1$study_type <- gsub("sub-chronic","subchronic", res1$study_type)
  res1$study_type <- gsub("reproductive / developmental","reproductive developmental", res1$study_type)

  dict <- openxlsx::read.xlsx(dict_study_type)

  x <- unique(res1$study_type)
  x <- x[!generics::is.element(x,dict[,1])]
  cat("   missing values in dictionary:",length(x),"\n")

  if(length(x)>0) browser()

  for(i in 1:nrow(dict)) {
    valold <- dict[i,1]
    valnew <- dict[i,2]
    res1[generics::is.element(res1$study_type,valold),"study_type"] <- valnew
  }

  res1[which(is.na(res1$study_type)|(res1$study_type == "")), "study_type"] <- "-"

  #####################################################################
  cat("fix quality\n")
  #####################################################################

  res1$quality <- paste(res1$AdminData_Reliability,"(" ,res1$AdminData_ReliabilityDefinition, ")")

  res1[which(is.na(res1$quality)|(res1$quality == "")), "quality"] <- "-"

  #####################################################################
  cat("fix guideline\n")
  #####################################################################
  multiple_guidelines <- grep(";", res1$MaterialsMethods_Qualifier)

  res1$guideline <- paste(res1$MaterialsMethods_Qualifier,res1$MaterialsMethods_Guideline)

  for (i in 1:length(multiple_guidelines)){
    res1[multiple_guidelines[i],"guideline"] <- paste(unlist(strsplit(res1[multiple_guidelines[i],"MaterialsMethods_Qualifier"], ";")), unlist(strsplit(res1[multiple_guidelines[i],"MaterialsMethods_Guideline"], ";")), collapse = "|")
  }

  res1[grep("^NA",res1$guideline),"guideline"] <- gsub("^NA","",res1[grep("^NA",res1$guideline), "guideline"])
  res1$guideline  <- gsub("^\\s+","", res1$guideline)
  res1[grep("NA$",res1$guideline),"guideline"] <- gsub("NA$","",res1[grep("NA$",res1$guideline), "guideline"])
  res1$guideline  <- gsub("\\s+$","", res1$guideline)

  res1[which(is.na(res1$guideline)|(res1$guideline == "")), "guideline"] <- "-"

  #####################################################################
  cat("fix exposure route and exposure method\n")
  #####################################################################
  #remove other: from exposure_route
  res1$exposure_route <- res1$MaterialsMethods_Exposure_Route
  other_route <- grep("^other:", res1$exposure_route)

  res1[other_route,"exposure_route"] <- gsub("^other:", "",  res1[other_route,"exposure_route"])

  common_routes_vals <- c("oral","inhalation","dermal","intravenous","intratesticular","peroral","intrathoracic","intra-carotid","intratracheal","intraperitoneal","intramuscular","subcutaneous","intracardiac","\\bi\\.*v\\b","\\bi\\.*p\\b","\\bi\\.*m\\b","\\bs\\.*c\\b","Nasal","Intracarotid","nasogastric","intracerebral","Intragastrically")
  common_routes <- grep(paste(common_routes_vals,  sep = "", collapse = "|"), res1$exposure_route, ignore.case = T)

  res1[common_routes,"exposure_route"] <- sapply(stringr::str_extract_all(res1[common_routes,"exposure_route"], paste(common_routes_vals, sep = "", collapse = "|")), paste, collapse= ',')

  res1$exposure_method <- res1$MaterialsMethods_Exposure_Route
  other_route <- grep("^other:", res1$exposure_method)

  res1[other_route,"exposure_method"] <- gsub("^other:", "",  res1[other_route,"exposure_method"])

  res1[grep(":", res1$exposure_method), "exposure_method"] <- gsub("(.*\\:\\s*)(.*)","\\2",res1[grep(":", res1$exposure_method), "exposure_method"])
  common_method_vals <- c("gavage","vapour","feed","drinking water","gas","aerosol","drink","diet","dust","capsule","capsules","injection","injected","vapor","food","ingestion")

  common_method <- grep(paste(common_method_vals,  sep = "", collapse = "|"), res1$exposure_method, ignore.case = T)

  res1[common_method,"exposure_method"] <- sapply(stringr::str_extract_all(res1[common_method,"exposure_method"], paste(common_method_vals, sep = "", collapse = "|")), paste, collapse= ',')

  res1$exposure_route <- gsub("^\\s+|\\s+$", "", res1$exposure_route)
  res1$exposure_method <- gsub("^\\s+|\\s+$", "", res1$exposure_method)

  res1[which(res1$exposure_route == ""|is.na(res1$exposure_route)),"exposure_route"] <- res1[which(res1$exposure_route == ""|is.na(res1$exposure_route)), "EndpointCategory"]
  res1[grep("assay", res1$exposure_route), "exposure_route"] <- res1[grep("assay", res1$exposure_route), "MaterialsMethods_Exposure_Route"]
  other_route <- grep("^other:", res1$exposure_route)

  res1[other_route,"exposure_route"] <- gsub("^other:", "",  res1[other_route,"exposure_route"])
  res1$exposure_route <- gsub("^\\s+|\\s+$", "", res1$exposure_route)

  dict <- openxlsx::read.xlsx(dict_exposure_route)

  x <- unique(res1$exposure_route)
  x <- x[!generics::is.element(x,dict[,1])]

  cat("   missing values in dictionary:",length(x),"\n")

  if(length(x)>0) browser()

  for(i in 1:nrow(dict)) {
    valold <- dict[i,1]
    valnew <- dict[i,2]
    res1[generics::is.element(res1$exposure_route,valold),"exposure_route"] <- valnew
  }

  res1[which(res1$exposure_method == ""),"exposure_method"] <- "-"

  dict <- openxlsx::read.xlsx(dict_exposure_method)

  x <- unique(res1$exposure_method)
  x <- x[!is.na(x)]
  x <- x[!generics::is.element(x,dict[,1])]

  cat("   missing values in dictionary:",length(x),"\n")

  if(length(x)>0) browser()

  for(i in 1:nrow(dict)) {
    valold <- dict[i,1]
    valnew <- dict[i,2]
    res1[generics::is.element(res1$exposure_method,valold),"exposure_method"] <- valnew
  }

  res1[which(is.na(res1$exposure_route)|(res1$exposure_route == "")), "exposure_route"] <- "-"
  res1[which(is.na(res1$exposure_method)|(res1$exposure_method == "")), "exposure_method"] <- "-"

  #####################################################################
  cat("fix study duration value and study duration units\n")
  #####################################################################
  res1$study_duration_value <- res1$MaterialsMethods_Exposure_Duration
  res1$study_duration_units <- res1$MaterialsMethods_Exposure_Duration
  res1[grep("^\\d+\\s*\\w+$", res1$study_duration_value),"study_duration_value"] <- gsub("(^\\d+)(\\s*\\w+$)","\\1",res1[grep("^\\d+\\s*\\w+$", res1$study_duration_value),"study_duration_value"])
  res1[grep("^\\d+\\s*\\w+$", res1$study_duration_units),"study_duration_units"] <- gsub("(^\\d+\\s*)(\\w+$)","\\2",res1[grep("^\\d+\\s*\\w+$", res1$study_duration_units),"study_duration_units"])


  common_dur_vals <- c("hours","hour","\\bhr\\b","\\bh\\b","\\bH\\b","days","day","\\bd\\b","weeks","week", "\\bwks\\b","\\bwk\\b", "months","month","\\bm\\b","year","years","\\byrs\\b","\\by\\b","\\byr\\b","minutes","\\bmin\\b","\\bmins\\b","seconds", "\\bGD\\b", "Gestational Days", "Gestation Day")
  common_dur <- grep(paste(common_dur_vals,  sep = "", collapse = "|"), res1$study_duration_units, ignore.case = T)
  common_dur2 <- paste("\\b[0-9]*\\s*\\-*[0-9a-zA-Z]+\\s*",common_dur_vals,"\\b|\\b",common_dur_vals,"\\s*[0-9a-zA-Z]+\\-*\\s*[0-9]*\\b", sep = "", collapse = "|")
  common_dur2_vals <- grep(common_dur2,res1$study_duration_value, ignore.case = T)

  res1[common_dur,"study_duration_units"] <- sapply(stringr::str_extract_all(res1[common_dur,"study_duration_units"], paste(common_dur_vals, sep = "", collapse = "|")), paste, collapse= ',')

  res1[common_dur2_vals,"study_duration_value"] <- sapply(stringr::str_extract_all(res1[common_dur2_vals,"study_duration_value"], common_dur2),paste, collapse= ',')
  res1[common_dur2_vals,"study_duration_value"] <- gsub("^\\s+","",res1[common_dur2_vals,"study_duration_value"])

  res1[common_dur2_vals,"study_duration_units"] <- gsub("\\d+","",res1[common_dur2_vals,"study_duration_value"])
  res1[common_dur2_vals,"study_duration_value"] <- gsub("[^0-9 \\,\\.\\-]+","",res1[common_dur2_vals,"study_duration_value"])
  res1[common_dur2_vals,"study_duration_value"] <- gsub("^\\s+|\\s+$","",res1[common_dur2_vals,"study_duration_value"])

  study_dur_dict <- data.frame(res1$MaterialsMethods_Exposure_Duration, res1$study_duration_value, res1$study_duration_units, stringsAsFactors = F)

  study_dur_dict[common_dur,"res1.study_duration_units"] <- sapply(stringr::str_extract_all(study_dur_dict[common_dur,"res1.study_duration_units"], paste(common_dur_vals, sep = "", collapse = "|")), paste, collapse= ',')
  study_dur_dict[,"res1.study_duration_value"] <- sapply(stringr::str_extract_all(study_dur_dict$res1.MaterialsMethods_Exposure_Duration, "\\(?[0-9,.]+\\)?"), paste, collapse= ",")

  study_dur_dict <-  unique(study_dur_dict)
  names(study_dur_dict) <- c("original_exposure_duration", "study_duration_value","study_duration_units")

  #write.xlsx(study_dur_dict, "study_duration_dictionary.xlsx")
  #single exposure to 1 day, days to day, PND to day , GD to day, Gestation day to day

  dict <- openxlsx::read.xlsx(dict_study_duration)

  dict$study_duration_units <- gsub("s$", "", dict$study_duration_units)
  dict$study_duration_units <- gsub("PND", "day", dict$study_duration_units)
  dict$study_duration_units <- gsub("GD", "day", dict$study_duration_units)
  dict$study_duration_units <- gsub("Gestation day", "day", dict$study_duration_units, ignore.case = T)
  dict$study_duration_units <- gsub("hour per day", "hours per day", dict$study_duration_units, ignore.case = T)
  dict$study_duration_units <- gsub("life-time|lifespan|life time|Lifetime", "lifetime", dict$study_duration_units, ignore.case = T)
  dict$study_duration_units <- gsub("\\bmin\\b", "minute", dict$study_duration_units, ignore.case = T)

  dict$study_duration_units <- gsub("\\s+$|^\\s+", "", dict$study_duration_units)
  #print(unique(grep("\r\n|\n", res1$MaterialsMethods_Exposure_Duration, value = T)))
  res1$MaterialsMethods_Exposure_Duration <- gsub("\r","", res1$MaterialsMethods_Exposure_Duration)

  res1$MaterialsMethods_Exposure_Duration <- gsub("\n+"," ", res1$MaterialsMethods_Exposure_Duration)
  #print(unique(grep("\r\n|\n", dict$original_exposure_duration, value = T)))
  # changed exposure and applications units to day

  x <- unique(res1$MaterialsMethods_Exposure_Duration)
  x <- x[!is.na(x)]

  x <- x[!generics::is.element(x,dict[,1])]

  cat("   missing values in dictionary:",length(x),"\n")
  if(length(x)>0) browser()

  for(i in 1:nrow(dict)) {
    valold <- dict[i,1]
    valnew <- dict[i,2]
    res1[generics::is.element(res1$MaterialsMethods_Exposure_Duration,valold),"study_duration_value"] <- valnew
  }

  for(i in 1:nrow(dict)) {
    valold <- dict[i,1]
    valnew <- dict[i,3]
    res1[generics::is.element(res1$MaterialsMethods_Exposure_Duration,valold),"study_duration_units"] <- valnew
  }

  res1$study_duration_value <- as.numeric(res1$study_duration_value)

  res1[which(is.na(res1$study_duration_units)|(res1$study_duration_units == "")), "study_duration_units"] <- "-"

  #####################################################################
  cat("fix species\n")
  #####################################################################
  res1$species <- res1$MaterialsMethods_TestAnimals_Species

  res1$species <- tolower(res1$species)

  #remove other: from species
  other_species <- grep("^other:", res1$species)

  res1[other_species,"species"] <- gsub("^other:", "",  res1[other_species,"species"])

  res1[other_species,"species"] <- gsub("^\\s+", "",  res1[other_species,"species"])

  res1$species <- gsub("\\bhenn\\b", "hen", res1$species)


  species_dict <- data.frame(unique(res1$species), stringsAsFactors = F)


  common_species <- unique(grep("^\\w+$",species_dict$unique.res1.species., value = T))
  common_species <- append(common_species, c("bear", "bull","ram", "chick","ducklings","Drosophilia Melanogaster","calf","man",
                                             "swine","zebrafish","Sprague Dawley","Leydig cell","artemia salina","Bovine","Baboon","Northern bobwhite",
                                             "Yucatan Minipig"))

  common_species_combined <- paste("\\b",common_species,"\\b", sep = "")
  common_species_combined <- append(common_species_combined, paste("\\b", common_species, "s", "\\b", sep= ""))
  common_species_combined <- tolower(common_species_combined)
  common_species_combined <- unique(common_species_combined)


  species_dict$new_species <-  sapply(stringr::str_extract_all(species_dict$unique.res1.species., paste(common_species_combined, collapse = "|")), paste, collapse= ',')

  species_dict$new_species <- sapply(species_dict$new_species, function(x) { paste(gsub("s$","",strsplit(x[1], ',')[[1]]), collapse = ",")} )


  species_dict$new_species <- gsub("volunteer", "human", species_dict$new_species)
  species_dict$new_species <- gsub("sprague dawley", "rat", species_dict$new_species)


  species_dict$new_species <- sapply(species_dict$new_species, function(x) { paste(unique(strsplit(x[1], ',')[[1]]), collapse = ",")} )


  species_dict$new_species <- unname(sapply(species_dict$new_species, function(x) { paste(sort(trimws(strsplit(x[1], ',')[[1]])), collapse=',')} ))

  species_dict[species_dict$new_species == ""| species_dict$new_species == "NA" ,"new_species"] <- "-"

  names(species_dict) <- c("extracted_species", "new_species")

  #write.xlsx(species_dict,"echa3_species_dict.xlsx" )
  res1[which(is.na(res1$species)|(res1$species == "")), "species"] <- "-"
  dict <- openxlsx::read.xlsx(dict_species)

  x <- unique(res1$species)

  x <- x[!generics::is.element(x,dict[,1])]

  cat("   missing values in dictionary:",length(x),"\n")
  if(length(x)>0) browser()

  for(i in 1:nrow(dict)) {
    valold <- dict[i,1]
    valnew <- dict[i,2]
    res1[generics::is.element(res1$species,valold),"species"] <- valnew
  }



  #####################################################################
  cat("fix strain\n")
  #####################################################################
  #remove other: from strain
  res1$strain <- res1$MaterialsMethods_TestAnimals_Strain
  other_strain <- grep("^other:", res1$strain)

  res1[other_strain,"strain"] <- gsub("^other:", "",  res1[other_strain,"strain"])

  res1[other_strain,"strain"] <- gsub("^\\s+", "",  res1[other_strain,"strain"])

  res1[grep("n/a|^n\\.a\\.$",res1$strain, ignore.case = T),"strain"] <- "-"

  res1[grep("^not\\s+\\w+$",res1$strain, ignore.case = T),"strain"] <- "-"

  res1[which(is.na(res1$strain)|(res1$strain == "")), "strain"] <- "-"

  #####################################################################
  cat("fix critical_effect\n")
  #####################################################################
  res1$critical_effect <- res1$Results_Basis

  res1[which(is.na(res1$critical_effect)),"critical_effect"] <- res1[which(is.na(res1$critical_effect)),"Results_Remarks"]
  res1[grep("other\\s*:\\s*see \\'Remark\\'", res1$critical_effect), "critical_effect"] <- res1[grep("other\\s*:\\s*see \\'Remark\\'", res1$critical_effect), "Results_Remarks"]
  #remove other: from critical effect
  other_effect <- grep("(other\\:)+", res1$critical_effect, ignore.case = T)

  res1[other_effect,"critical_effect"] <- gsub("^other:", "",  res1[other_effect,"critical_effect"])

  res1[other_effect,"critical_effect"] <- gsub("^\\s+", "",  res1[other_effect,"critical_effect"])


  res1[grep("^\\-\\s*.*", res1$critical_effect),"critical_effect"] <- gsub("(^\\-\\s*)(.*)","\\2", res1[grep("^\\-\\s*.*", res1$critical_effect),"critical_effect"])

  #res1[grep("^\\(.*\\)$", res1$critical_effect),"critical_effect"] <- gsub("(^\\(\\s*)(.*)(\\s*\\)$)", "\\2", res1[grep("^\\(.*\\)$", res1$critical_effect),"critical_effect"])

  res1[grep("^\\(.*\\)$", res1$critical_effect),"critical_effect"] <- gsub("\\)\\s*\\(","\\.",res1[grep("^\\(.*\\)$", res1$critical_effect),"critical_effect"])
  res1[grep("^\\(.*\\)$", res1$critical_effect),"critical_effect"] <- gsub("\\)\\;","\\;",res1[grep("^\\(.*\\)$", res1$critical_effect),"critical_effect"])
  res1[grep("^\\(.*\\)$", res1$critical_effect),"critical_effect"] <- gsub("\\s+\\(\\~"," \\,\\~",res1[grep("^\\(.*\\)$", res1$critical_effect),"critical_effect"])
  res1[grep("^\\(.*\\)$", res1$critical_effect),"critical_effect"] <- gsub("(^\\(\\s*)(.*)(\\s*\\)$)", "\\2", res1[grep("^\\(.*\\)$", res1$critical_effect),"critical_effect"])


  res1[grep("^other:", res1$critical_effect), "critical_effect"] <- res1[grep("^other:", res1$critical_effect), "Results_Remarks"]
  res1[grep("see below", res1$critical_effect, ignore.case = T), "critical_effect"] <- res1[grep("see below", res1$critical_effect, ignore.case = T), "Results_Remarks"]
  res1[grep("executive summary|executive study", res1$critical_effect, ignore.case = T), "critical_effect"] <- res1[grep("executive summary|executive study", res1$critical_effect, ignore.case = T), "Results_Remarks"]
  res1[grep("see results below|See remarks|see remakt|see remark\\:*|^other:$", res1$critical_effect, ignore.case = T), "critical_effect"] <- res1[grep("see results below|See remarks|see remakt|see remark\\:*|^other:$", res1$critical_effect, ignore.case = T), "Results_Remarks"]


  res1[which(is.na(res1$critical_effect)|(res1$critical_effect == "")), "critical_effect"] <- "-"
  res1[which(res1$critical_effect == "-"),"critical_effect"] <- res1[which(res1$critical_effect == "-"),"Results_DoseDescriptor"]

  other_effects <- grep("(other\\:)+", res1$critical_effect, ignore.case = T)

  res1[other_effects,"critical_effect"] <- gsub("(other\\:)+", "",  res1[other_effects,"critical_effect"])

  res1[other_effects,"critical_effect"] <- gsub("^\\s+", "",  res1[other_effects,"critical_effect"])

  res1[grep('^[a-zA-Z]+[0-9]+$|^[a-zA-Z]{,2}$|^conc\\. level\\:$|^see\\s*.*\\"Remarks.*\\"$|^see\\:|^n\\.a\\.$|^L\\(Ct\\)50$|^LCLo$|^LDLo$|^NOEC$|^T10-09:TDlo$|^LDLow$|^LOAEC$|^NOAEL$|^LOEL$|^NOEL$|^NOAEC$|^LOAEL$|^dose level\\:$',res1$critical_effect),"critical_effect"] <- "-"

  res1[grep("^\\=", res1$critical_effect),"critical_effect"] <- gsub("(^\\=\\s*)(.*)","\\2",res1[grep("^\\=", res1$critical_effect),"critical_effect"])

  res1[grep("^\\[.*\\]$", res1$critical_effect),"critical_effect"] <- gsub("(^\\[+)(.*)(\\]+$)","\\2",res1[grep("^\\[.*\\]$", res1$critical_effect),"critical_effect"])
  res1[grep("^[^[:alnum:]]", res1$critical_effect), "critical_effect"] <- gsub('(^[\\\\\"])(.*)([\\\\\"]$)',"\\2",res1[grep("^[^[:alnum:]]", res1$critical_effect), "critical_effect"])
  res1[grep("^[^[:alnum:]]", res1$critical_effect), "critical_effect"] <- gsub('(^\\(+)(.*)(\\)+)(.*)',"\\2\\4",res1[grep("^[^[:alnum:]]", res1$critical_effect), "critical_effect"])
  res1[grep("^[^[:alnum:]]", res1$critical_effect), "critical_effect"] <- gsub('^\\.*$',"-",res1[grep("^[^[:alnum:]]", res1$critical_effect), "critical_effect"])
  res1[grep("^[^[:alnum:]]", res1$critical_effect), "critical_effect"] <- gsub('^\\%$',"percentage",res1[grep("^[^[:alnum:]]", res1$critical_effect), "critical_effect"])
  res1[grep("^[^[:alnum:]]", res1$critical_effect), "critical_effect"] <- gsub('(^\\=\\s*)(.*)',"equal to \\2",res1[grep("^[^[:alnum:]]", res1$critical_effect), "critical_effect"])
  res1[grep("^[^[:alnum:]]", res1$critical_effect), "critical_effect"] <- gsub('(^>\\s*)(.*)',"greater than \\2",res1[grep("^[^[:alnum:]]", res1$critical_effect), "critical_effect"])
  res1[grep("^[^[:alnum:]]", res1$critical_effect), "critical_effect"] <- gsub('(^±\\s*)(.*)',"plus or minus \\2",res1[grep("^[^[:alnum:]]", res1$critical_effect), "critical_effect"])
  res1[grep("^[^[:alnum:]]", res1$critical_effect), "critical_effect"] <- gsub('(^\\+\\s*or\\s*\\-\\s*)(.*)',"plus or minus \\2",res1[grep("^[^[:alnum:]]", res1$critical_effect), "critical_effect"])


  dict <- openxlsx::read.xlsx(dict_critical_effects)
  dict[1492,"new_critical_effect"] <- "Positive promoting activity after only 28 days of exposure .significant and time-dependent increase in incidence of focal hepatocellular proliferative lesions; significant increase in incidence of liver tumours at 168 days"
  dict[1496,"new_critical_effect"] <- "Positive promoting activity .increased  in incidence of liver tumours in mice given NDEA and DEHP compared with mice exposed to NDEA alone"
  dict[1726,"new_critical_effect"] <- "Increased liver and kidney weights increased incidence of non-neoplasic changes. increased liver and kidney weights increased incidence of non-neoplasic changes"
  dict[2165,"new_critical_effect"] <- "non-neoplastic effects in the nasal cavity in a. and b. with and without prior damage of nasal mucosa"
  dict[2164,"new_critical_effect"] <- "no effects related to formaldehyde exposure in a. and b. with and without prior damage of nasal mucosa"
  dict[1485,"new_critical_effect"] <- "Positive promoting activity .increased  in incidence of renal cell adenomas and adenocarcinomas and the number of tumours per kidney, in rats given DEHP after N-ethyl-N-hydroxyethylnitrosamine (EHEN)"

  x <- unique(res1$critical_effect)

  x <- x[!generics::is.element(x,dict[,4])]

  cat("   missing values in dictionary:",length(x),"\n")

  if(length(x)>0) browser()

  for(i in 1:nrow(dict)) {
    valold <- dict[i,4]
    valnew <- dict[i,5]
    res1[generics::is.element(res1$critical_effect,valold),"critical_effect"] <- valnew
  }


  res1[which(is.na(res1$critical_effect)|(res1$critical_effect == "")), "critical_effect"] <- "-"

  res1[10162,"critical_effect"] <- "16-21 mg/kg bw/d based on decreased sperm head counts and focal seminiferous tubule atrophy in F1 males at 1200 ppm (80-107 mg/kg bw/d). There were no effects on reproductive endpoints such as fertility, mating, gestation and birth index."
  res1[1137,"critical_effect"] <- "Since the administration of 1,4-dichlorobenzene to male rats produced histologic changes in the kidney which were judged to be life threatening at all doses in this study, the 13-week studies were repeated at lower doses (NTP-TR.319) (1987)."
  res1[4222,"critical_effect"] <- "Converted value (calculated with a density of 0.80 g/mL. Original LD50 value: 4.24 (2.52 to 7.12) ml/kg)"

  #####################################################################
  cat("fix generation\n")
  #####################################################################

  # assign generation info from critical_effect
  gen_info <- c("p0","p1","f1","f2","\\bp\\b","\\bmaternal\\b","\\bfetal\\b")
  res1$generation <- stringr::str_extract_all(res1$critical_effect, stringr::regex(paste(gen_info, collapse = "|"), ignore_case = T)) %>% sapply(., paste, collapse = ", ")

  res1[which(res1$generation == ""),"generation"] <- res1[which(res1$generation == ""),"Results_Generation"]
  # res1[grep("P1",res1$Results_EffectDescriptor),"generation"] <- res1[grep("P1",res1$Results_EffectDescriptor),"Results_EffectDescriptor"]
  # res1[grep("P0",res1$Results_EffectDescriptor),"generation"] <- res1[grep("P0",res1$Results_EffectDescriptor),"Results_EffectDescriptor"]
  res1[grep("maternal animals",res1$Results_EffectDescriptor),"generation"] <- res1[grep("maternal animals",res1$Results_EffectDescriptor),"Results_EffectDescriptor"]
  res1[grep("fetuses",res1$Results_EffectDescriptor),"generation"] <- res1[grep("fetuses",res1$Results_EffectDescriptor),"Results_EffectDescriptor"]


  res1$generation <- gsub("\\bgeneration\\b|\\bgeneration\\:\\b","",res1$generation, ignore.case = T)

  res1$generation <- gsub("^\\s+|\\s+$","",res1$generation)



  for (i in 1:nrow(res1)){
    res1$generation[i] <- paste(unique(unlist(stringr::str_split(res1$generation[i], ", "))), collapse = ", ")

  }

  res1$generation <- gsub("(^other:\\s+\\:*\\s*)(.*)","\\2",res1$generation, ignore.case = T)




  res1[which(is.na(res1$generation)|(res1$generation == "")), "generation"] <- "-"




  #####################################################################
  cat("create new_res\n")
  #####################################################################
  names.list <- c("CAS.Number","Chemical.Result","Url.Searched","Result.URL","DataSource_Title","DataSource_Author","DataSource_Year","DataSource_BibliographicSource","MaterialsMethods_GLPCompliance",
                  "species","strain","MaterialsMethods_TestAnimals_Sex","generation","critical_effect","toxval_numeric_qualifier","toxval_numeric",
                  "toxval_units","source","subsource","toxval_type","study_type","quality","guideline","exposure_route","exposure_method","study_duration_units","study_duration_value","Filename",
                  "toxval_numeric_original", "toxval_units_original", "toxval_numeric_qualifier_original","toxval_type_original",
                  "study_type_original","exposure_route_original","exposure_method_original","study_duration_value_original",
                  "study_duration_units_original","species_original","strain_original","critical_effect_original","AdminData_Type","Results_Remarks",
                  "MaterialsMethods_Qualifier","MaterialsMethods_Guideline","AdminData_Reliability","AdminData_ReliabilityDefinition")

  new_res <- res1[,names.list]

  names.list <- c("casrn","name","source_url","record_url","title","author","year","journal","glp","species","strain","sex","generation","critical_effect","toxval_numeric_qualifier","toxval_numeric", "toxval_units",
                  "source","subsource","toxval_type","study_type","quality","guideline","exposure_route","exposure_method","study_duration_units","study_duration_value","document_name","toxval_numeric_original", "toxval_units_original", "toxval_numeric_qualifier_original","toxval_type_original",
                  "study_type_original","exposure_route_original","exposure_method_original","study_duration_value_original",
                  "study_duration_units_original","species_original","strain_original","critical_effect_original","admin_data_type","critical_effect_original2",
                  "guideline1","guideline2", "quality1","quality2")

  names(new_res) <- names.list

  new_res[which(is.na(new_res$glp)|(new_res$glp == "")), "glp"] <- "-"
  new_res[which(is.na(new_res$journal)|(new_res$journal == "")), "journal"] <- "-"
  new_res[which(is.na(new_res$author)|(new_res$author == "")), "author"]<- "-"
  new_res[which(is.na(new_res$title)|(new_res$title == "")), "title"] <- "-"
  new_res[which(is.na(new_res$sex)|(new_res$sex == "")), "sex" ]<- "-"
  new_res$year <-  as.numeric(new_res$year)

  #write.xlsx(new_res, "../echa3/echa3_files/echa_tsca_poc.xlsx")

  #####################################################################
  cat("checks, finds and replaces non ascii characters in new_res with XXX\n")
  #####################################################################
  new_res <- fix.non_ascii(new_res)


  # #write.xlsx(new_res2, "../echa3/echa3_files/echa_tsca_poc2.xlsx")
  #

  new_res["echa3_id"] <- c(1:length(new_res[,1]))

  new_res <- new_res[c("echa3_id",names(new_res[-47]))]
  print(names(new_res))
  runInsertTable(new_res,"new_echa3",toxval.db,do.halt=T,verbose=F)

  #####################################################################
  cat("Build echa3_chemical_information table from new_res\n")
  #####################################################################
  chemical_information <- new_res[,c("casrn","name")]
  chemical_information <- unique(chemical_information[,1:2])
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  chemical_information <- chemical_information[c('chemical_id','name','casrn')]

  #runInsertTable(chemical_information,"echa3_chemical_information",toxval.db,do.halt=T,verbose=F)

}

