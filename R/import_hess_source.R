library(tidyr)
library(stringr)
library(openxlsx)
#--------------------------------------------------------------------------------------
#' Load hess Source into dev_toxval_source_v3.
#' @param db The version of toxval into which the source is loaded.
#' @param infile1 The input file ./hess/hess_files/hess_6_16_21.csv, extracted by Risa Sayre(SCDCD)
#' @param infile2 The input file ./hess/hess_files/hess_record_urls_from_clowder.xlsx
#--------------------------------------------------------------------------------------
import_hess_source <- function(db,
                               infile1="../hess/hess_files/hess_6_16_21.xlsx",
                               infile2="../hess/hess_files/hess_record_urls_from_clowder.xlsx",
                               chem.check.halt=T) {
  printCurrentFunction(db)
  #####################################################################
  cat("Build whole_hess_table from source file \n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile1)
  res1 <- res
  # factor cols to character
  fac_cols <- sapply(res1, is.factor)                          # Identify all factor columns
  res1[fac_cols] <- lapply(res1[fac_cols], as.character)  # Convert all factors to characters

  # strip begining and ending quotation marks
  res1 <- as.data.frame(sapply(res1, function(x) gsub("\\'", "", x)))
  # strip leading and trailing spaces
  res1 <- as.data.frame(sapply(res1, function(x) gsub("(.*)(\\s+$)", "\\1", x)))
  res1 <- as.data.frame(sapply(res1, function(x) gsub("(^\\s+)(.*)", "\\2", x)))

  # factor cols to character
  fac_cols <- sapply(res1, is.factor)                          # Identify all factor columns
  res1[fac_cols] <- lapply(res1[fac_cols], as.character)  # Convert all factors to characters

  # apply appropriate data types
  res1 <- lapply(res1, function(x) type.convert(as.character(x), as.is = T))
  res1 <- data.frame(res1, stringsAsFactors = F)

  # all na logi cols to character
  res1 <- data.frame(lapply(res1, function(x) if(is.logical(x)) {
    return(as.character(x))
  } else {
    return(x)
  }), stringsAsFactors=FALSE)

  names(res1) <- tolower(names(res1))
  #print(str(res1))

  ##runInsertTable(res1,"whole_hess_table",db,do.halt=T,verbose=F)
  #####################################################################
  cat("Build original_hess table from res \n")
  #####################################################################
  # names of all critical effect categories
  names.list <- c("deaths","clinical_observation","functional_observation_battery","body_weight_changes","food_consumption",
                  "water_consumption","urinalysis","hematology","blood_chemistry","thyroid_hormone","absolute_organ_weight",
                  "relative_organ_weight","organ_weight","necropsy","macroscopic_finding","histopathology","reproductive_tissue_evaluation",
                  "estrous_cycle_characterization","bone_marrow_cellularity_counts","liver_biochemistry","reproductive_endpoint","other_findings")
  # critical effect N/A Values to hyphen
  res1[ , names(res1)[ names(res1)%in% names.list] ][ res1[ , names(res1)[ names(res1)%in% names.list] ] == "N/A" ] <- "-"
  # combine column names with values
  res1[,names(res1)[ names(res1)%in% names.list]] <- Map(paste, names(res1)[ names(res1)%in% names.list], res1[,names(res1)[ names(res1)%in% names.list]], sep = ';')
  # convert values with just column names and column names attached with NA values to NAs
  res1[, names(res1)[ names(res1)%in% names.list]] <- lapply(res1[, names(res1)[ names(res1)%in% names.list]],
                                                             function(x) replace(x, grepl(paste0("^",names.list,";$", collapse="|"), x), NA))
  res1[, names(res1)[ names(res1)%in% names.list]] <- lapply(res1[, names(res1)[ names(res1)%in% names.list]],
                                                             function(x) replace(x, grepl(paste0("^",names.list,";-$", collapse="|"), x), NA))
  res1[, names(res1)[ names(res1)%in% names.list]] <- lapply(res1[, names(res1)[ names(res1)%in% names.list]],
                                                             function(x) replace(x, grepl(paste0("^",names.list,";NA$", collapse="|"), x), NA))
  # combine all critical effect columns
  res1 <- res1 %>% unite("critical_effect", names(res1)[ names(res1)%in% names.list],sep = "|", na.rm = TRUE, remove = FALSE)
  # remove question marks before critical effect values
  res1$critical_effect <- str_replace_all(res1$critical_effect, pattern = "\\?", replacement = "")
  # keep only death or death none information in critical effect
  res1$critical_effect <- gsub("(^deaths)(\\;[0-9]+)([^|]+)(.*)", "\\1\\4",res1$critical_effect)
  # remove multiple patterns(starting with semicolon, colon, comma) of sex info from critical effect
  res1$critical_effect <- str_replace_all(res1$critical_effect, pattern = "(\\;|\\:)+\\s*[^[:alnum:]]*[0-9]+\\s*(male|female)*\\s*(female|male)*", replacement = "")
  res1$critical_effect <- str_replace_all(res1$critical_effect, pattern = "\\,+\\s*[^[:alnum:]]*[0-9]+\\s*(male|female)*", replacement = "")
  # provide appropriate spacing between words
  res1$critical_effect <- gsub("(\\s*[a-zA-Z]{1}[a-z]+)([A-Z]{1}[a-z]*\\s*)", "\\1 \\2",res1$critical_effect)
  # provide seperaton for values within each critical effect category with commas
  res1$critical_effect <- gsub("(decrease|increase)(\\s+)", "\\1,\\2",res1$critical_effect)
  # remove trailing commas at the end of each critical effect category
  res1$critical_effect <- gsub("(\\,\\s+\\|)", "|",res1$critical_effect)
  # remove multiple patterns(starting with period, multiple sex info within each value, single sex info within each value) of sex info from critical effect
  res1$critical_effect <- gsub("((\\.|\\s+)[0-9]+\\s+(male|female)+)", "",res1$critical_effect)
  res1$critical_effect <- gsub("((male|female)+\\s*(male|female)*\\s*\\*+)", "",res1$critical_effect)
  res1$critical_effect <- gsub("((male|female)+\\s*)", "",res1$critical_effect)
  # convert encoding to utf8
  res1$critical_effect <- enc2utf8(res1$critical_effect)
  # convert encoding of critical effect value columns to utf8
  cols <- names(res1)
  for(i in seq_along(cols)){
    if(!is.character(res1[, cols[[i]]])) next
    Encoding(res1[, cols[[i]]]) <- "UTF-8"
  }
  res1$sex <- "-"

  # columns containing female sex info
  female_cols <- res1[apply(res1, 1, function(i) any(grepl("\\bfemale\\b", i))),]
  res1[rownames(female_cols),"sex"] <- "female"
  # columns containing male sex info
  male_cols <- res1[apply(res1, 1, function(i) any(grepl("\\bmale\\b", i))),]
  res1[rownames(male_cols),"sex"] <- paste("male", res1[rownames(male_cols),"sex"] , sep = '/')
  # remove trailing /- from sex
  res1$sex <- gsub("(.*)(\\/\\-$)","\\1",res1$sex)
  # assign duration value and units original from study type
  res1$study_duration_value <- res1$study_type
  res1$study_duration_units <- res1$study_type
  # assign species and strain original from subject type
  res1$species <- res1$subject_type
  res1$strain <- res1$subject_type
  # assign exposure_route and exposure_method original from route
  res1$exposure_route <- res1$route
  res1$exposure_method <- res1$route
  res1$noel_val <- res1$reported_noel
  res1$noel_units <- res1$reported_noel
  res1$noel_qual <- res1$reported_noel
  res1$noel_type <- "NOEL"
  res1$noael_val <- res1$reported_noael
  res1$noael_units <- res1$reported_noael
  res1$noael_qual <- res1$reported_noael
  res1$noael_type <- "NOAEL"
  res1$loel_val <- res1$reported_loel
  res1$loel_units <- res1$reported_loel
  res1$loel_qual <- res1$reported_loel
  res1$loel_type <- "LOEL"
  res1$loael_val <- res1$reported_loael
  res1$loael_units <- res1$reported_loael
  res1$loael_qual <- res1$reported_loael
  res1$loael_type <- "LOAEL"
  h1 <- res1[,c(1:51,52:55)]
  h2 <- res1[,c(1:51,56:59)]
  h3 <- res1[,c(1:51,60:63)]
  h4 <- res1[,c(1:51,64:67)]
  names(h1)[52] <- "toxval_numeric"
  names(h1)[53] <- "toxval_units"
  names(h1)[54] <- "toxval_numeric_qualifier"
  names(h1)[55] <- "toxval_type"
  names(h2)[52] <- "toxval_numeric"
  names(h2)[53] <- "toxval_units"
  names(h2)[54] <- "toxval_numeric_qualifier"
  names(h2)[55] <- "toxval_type"
  names(h3)[52] <- "toxval_numeric"
  names(h3)[53] <- "toxval_units"
  names(h3)[54] <- "toxval_numeric_qualifier"
  names(h3)[55] <- "toxval_type"
  names(h4)[52] <- "toxval_numeric"
  names(h4)[53] <- "toxval_units"
  names(h4)[54] <- "toxval_numeric_qualifier"
  names(h4)[55] <- "toxval_type"
  res1 <- rbind(h1,h2,h3,h4)
  rownames(res1) <- c()
  names(res1)[names(res1) == "chemical_name"] <- "name"
  res1$toxval_subtype <- res1$study_type
  res1$document_name <- "-"
  for (i in 1:nrow(res1)){
    res1$document_name[i] <- gsub("(.*HESS\\\\)(.*)(\\..*)","\\2",res1$filename[i])
  }
  for (i in 1:nrow(res1)){
    res1$document_name[i] <- paste(res1$document_name[i],".pdf", sep = "")
  }
  # assign document url from clowder to corresponding documents
  url_from_clowder <- openxlsx::read.xlsx(infile2)
  for(i in 1:nrow(url_from_clowder)) {
    valold <- url_from_clowder[i,2]
    valnew <- url_from_clowder[i,4]
    res1[is.element(res1$document_name,valold),"record_url"] <- valnew
  }
  #print(str(res1))
  # multiple casrn per chemical, erroneous casrn
  res1[grep("82832\\-73\\-3\\, 87625\\-09\\-0",res1$casrn),"casrn"]  <- "82832-73-3"
  res1[grep("82657\\-04\\-399267\\-18\\-2", res1$casrn),"casrn"] <- "99267-18-2"


  #####################################################################
  cat("Do the chemical checking\n")
  #####################################################################
  source = "HESS"
  res = as.data.frame(res1)
  res$clowder_id = "-"
  res = fix.non_ascii.v2(res,source)
  res = source_chemical.process(db,res,source,chem.check.halt,casrn.col="casrn",name.col="name",verbose=F)
  #####################################################################
  cat("Build the hash key and load the data \n")
  #####################################################################
  res = subset(res,select=-c(chemical_index))
  toxval_source.hash.and.load(db,source,"original_hess",F,F,res)
  browser()
  return(1)
  runInsertTable(res1,"original_hess",db,do.halt=T,verbose=F)

  #####################################################################
  cat("Build new_hess from res1\n")
  #####################################################################

  #extract toxval_subtype info from study_type

  res1[grep("^TG.*\\:",res1$toxval_subtype), "toxval_subtype"] <- gsub("(.*)(\\:.*)","\\1",res1[grep("^TG.*\\:",res1$toxval_subtype), "toxval_subtype"])
  res1[grep("\\:|\\;",res1$toxval_subtype), "toxval_subtype"] <- gsub("(.*)(\\:|\\;\\s+)(.*)(\\)$)","\\3",res1[grep("\\:|\\;",res1$toxval_subtype), "toxval_subtype"])
  res1[grep("TG.*\\)+",res1$toxval_subtype), "toxval_subtype"] <- gsub("(.*\\s*)(TG.*)(\\))","\\2",res1[grep("TG.*\\)+",res1$toxval_subtype), "toxval_subtype"])
  res1[grep("TG.*\\)+\\s*\\(+",res1$toxval_subtype), "toxval_subtype"] <- gsub("(TG.*)(\\).*\\(.*)","\\1",res1[grep("TG.*\\)+\\s*\\(+",res1$toxval_subtype), "toxval_subtype"])
  res1[grep("^Repeated.*TG.*",res1$toxval_subtype), "toxval_subtype"] <- gsub("(.*)(TG.*$)","\\2",res1[grep("^Repeated.*TG.*",res1$toxval_subtype), "toxval_subtype"])
  res1[grep("OECD.*combined",res1$toxval_subtype, ignore.case = T), "toxval_subtype"] <- gsub("(.*OECD\\s*\\d+)(\\s*.*)","\\1",res1[grep("OECD.*combined",res1$toxval_subtype, ignore.case = T), "toxval_subtype"])
  res1[grep("TG|OECD",res1$toxval_subtype, invert = T), "toxval_subtype"] <- "-"

  # extracting duration value and units from study type
  res1[grep("\\(",res1$study_duration_value),"study_duration_value"] <- gsub("(.*)(\\(.*)","\\2",res1[grep("\\(",res1$study_duration_value),"study_duration_value"])
  res1[grep("\\(",res1$study_duration_units),"study_duration_units"] <- gsub("(.*)(\\(.*)","\\2",res1[grep("\\(",res1$study_duration_units),"study_duration_units"])
  res1[grep("\\d+\\-*(hour|day|week|year)",res1$study_duration_value, ignore.case = T),"study_duration_value"] <- gsub("(.*)(\\b\\d+\\-*(hour|day|week|year)\\b)(.*)","\\2",res1[grep("\\d+\\-*(hour|day|week|year)",res1$study_duration_value, ignore.case = T),"study_duration_value"])
  res1[grep("\\d+\\-*(hour|day|week|year)",res1$study_duration_units, ignore.case = T),"study_duration_units"] <- gsub("(.*)(\\b\\d+\\-*(hour|day|week|year)\\b)(.*)","\\2",res1[grep("\\d+\\-*(hour|day|week|year)",res1$study_duration_units, ignore.case = T),"study_duration_units"])
  res1[grep("\\(|day|week",res1$study_duration_value, invert = T),"study_duration_value"] <- NA
  res1[grep("\\(|day|week",res1$study_duration_units, invert = T),"study_duration_units"] <- "-"
  res1[grep("\\d+M", res1$study_duration_value, ignore.case = T),"study_duration_value"] <- gsub("(.*[^0-9]+)([0-9]+)(M.*)","\\2",res1[grep("\\d+M", res1$study_duration_value, ignore.case = T),"study_duration_value"])
  res1[grep("\\d+M", res1$study_duration_units, ignore.case = T),"study_duration_units"] <- "month"
  res1[grep("\\d+w|\\d+\\s*week|\\d+\\-*week|[^\\/]week", res1$study_duration_units, ignore.case = T),"study_duration_units"] <- "week"
  res1[grep("\\d+w|\\d+\\s*week|\\d+\\-*week|[^\\/]week", res1$study_duration_value, ignore.case = T),"study_duration_value"]<- gsub("(\\,.*$|\\;.*$|\\:.*$)","",res1[grep("\\d+w|\\d+\\s*week|\\d+\\-*week|[^\\/]week", res1$study_duration_value, ignore.case = T),"study_duration_value"])
  res1[grep("\\d+w|\\d+\\s*week|\\d+\\-*week|[^\\/]week", res1$study_duration_value, ignore.case = T),"study_duration_value"]<-gsub("[^0-9]+(\\d+)[^0-9]+","\\1",res1[grep("\\d+w|\\d+\\s*week|\\d+\\-*week|[^\\/]week", res1$study_duration_value, ignore.case = T),"study_duration_value"])
  res1[grep("\\d+\\-*week", res1$study_duration_value, ignore.case = T),"study_duration_value"] <-gsub("^(\\d+)(.*)","\\1",res1[grep("\\d+\\-*week", res1$study_duration_value, ignore.case = T),"study_duration_value"])
  res1[grep("\\d+d|\\d+\\s*day|\\d+\\-*day|days\\/|day\\/", res1$study_duration_units, ignore.case = T),"study_duration_units"] <- "day"
  res1[grep("\\d+d|\\d+\\s*day|\\d+\\-*day|days\\/|day\\/", res1$study_duration_value, ignore.case = T),"study_duration_value"] <- gsub("(.+?)(d.*)","\\1",res1[grep("\\d+d|\\d+\\s*day|\\d+\\-*day|days\\/|day\\/", res1$study_duration_value, ignore.case = T),"study_duration_value"])
  res1[grep("^\\(TG.*\\)$",res1$study_duration_value),"study_duration_value"] <- NA
  res1[grep("^\\(TG.*\\)$",res1$study_duration_units),"study_duration_units"] <- "-"
  res1[grep("TG",res1$study_duration_value),"study_duration_units"] <- "week"
  res1[grep("TG",res1$study_duration_value),"study_duration_value"] <- gsub("(^\\(+)(\\d+)(\\s+.*)","\\2", res1[grep("TG",res1$study_duration_value),"study_duration_value"])
  res1[grep("\\( male \\) days)",res1$study_duration_value),"study_duration_value"] <-gsub("(.*\\-)(\\d+)(.*)","\\2",res1[grep("\\( male \\) days)",res1$study_duration_value),"study_type"])
  res1[grep("\\( male \\) days)",res1$study_duration_units),"study_duration_units"] <- "day"
  res1[grep("\\-\\s*\\d+",res1$study_duration_value),"study_duration_value"] <- gsub("(.*\\-)(.*)","\\2",res1[grep("\\-\\s*\\d+",res1$study_duration_value),"study_duration_value"])
  res1[grep("\\-\\s*\\d+",res1$study_duration_units),"study_duration_units"] <- "-"
  res1[grep("d\\/w",res1$study_duration_value),"study_duration_value"] <- gsub("(^\\(\\s+)(\\d+)(\\s+.*)","\\2",res1[grep("W",res1$study_duration_value),"study_duration_value"])
  res1[grep("\\(407\\)",res1$study_duration_value),"study_duration_value"] <- gsub("(.*\\()(\\d+)(\\s*day.*)","\\2",res1[grep("\\(407\\)",res1$study_duration_value),"study_type"])
  res1[grep("\\(407\\)",res1$study_duration_units),"study_duration_units"] <- gsub("(.*\\()(\\d+)(\\s*)(day)(.*)","\\4",res1[grep("\\(407\\)",res1$study_duration_units),"study_type"])
  res1$study_duration_value <- gsub("[^0-9]+","",res1$study_duration_value)
  res1[grep("weeeks",res1$study_duration_units),"study_duration_units"] <- "week"
  res1[grep("R14",res1$study_duration_units),"study_duration_value"] <- gsub("(.*\\()(\\d+)(\\s+)(day)(.*)","\\2",res1[grep("R14",res1$study_duration_units),"study_type"])
  res1[grep("R14",res1$study_duration_units),"study_duration_units"] <- gsub("(.*\\()(\\d+)(\\s+)(day)(.*)","\\4",res1[grep("R14",res1$study_duration_units),"study_type"])
  res1[grep("W",res1$study_duration_units),"study_duration_units"] <- "week"
  res1$study_duration_value <- as.numeric(res1$study_duration_value)
  # species and strain clean up
  res1[grep("Rat|Ra t",res1$species, ignore.case = T),"species"] <- "rat"
  res1[grep("Rat|Ra t",res1$strain, ignore.case = T),"strain"] <- gsub("Rat|Ra t","",res1[grep("Rat|Ra t",res1$strain, ignore.case = T),"strain"])
  res1$strain <- gsub("(^r*\\s+\\(+)(.*)(\\)+.*)","\\2",res1$strain)
  res1$strain <- gsub("^\\s+|\\s+$","",res1$strain)
  res1$strain <- gsub("(^\\()(.*)(\\)$)","\\2",res1$strain)
  res1$strain <- gsub("(.*)(\\).*\\(.*)","\\1",res1$strain)
  res1$strain <- gsub("(.*)(\\,*\\s+rats|male.*)","\\1",res1$strain)
  res1$strain <- gsub("(.*)(\\,\\s+$)","\\1",res1$strain)
  res1[grep("\\([A-Za-z]+$",res1$strain),"strain"] <- paste(res1[grep("\\([A-Za-z]+$",res1$strain),"strain"],")", sep = "")
  res1$strain <- gsub("^$","-",res1$strain)
  res1[grep("\\(", res1$exposure_route),"exposure_route"] <- gsub("(.*)(\\s*\\(.*\\))","\\1",res1[grep("\\(", res1$exposure_route),"exposure_route"])
  res1$exposure_route <- gsub("\\s+$","",res1$exposure_route)
  res1$exposure_route <- gsub("\\*$","",res1$exposure_route)
  res1[grep("\\(", res1$exposure_method),"exposure_method"] <- gsub("(\\s+\\(5days/week\\))","",res1[grep("\\(", res1$exposure_method),"exposure_method"])
  res1[grep("\\(", res1$exposure_method),"exposure_method"] <- gsub("(.*\\s*\\()(.*)(\\))","\\2",res1[grep("\\(", res1$exposure_method),"exposure_method"])
  res1$source_url <- "https://www.nite.go.jp/en/chem/qsar/hess_update-e.html"
  res1[grep("^\\d+\\-day",res1$study_type),"study_type"] <- gsub("(^\\d+\\-day\\s+)(.*)","\\2",res1[grep("^\\d+\\-day",res1$study_type),"study_type"])
  res1[grep("^Repeated\\s*\\-*dose",res1$study_type, ignore.case = T),"study_type"] <- "repeat dose"
  res1[grep("^Combined",res1$study_type, ignore.case = T),"study_type"] <- gsub("([^\\(])(\\s*\\(+.*)","\\1",res1[grep("^Combined",res1$study_type, ignore.case = T),"study_type"])
  res1[grep("\\:",res1$study_type),"study_type"] <- gsub("(.*\\:)(.*)","\\2",res1[grep("\\:",res1$study_type),"study_type"])
  res1[grep("\\(",res1$study_type),"study_type"] <- gsub("(\\(.*\\))","",res1[grep("\\(",res1$study_type),"study_type"])
  res1[grep("OECD",res1$study_type),"study_type"] <- gsub("(.*OECD\\s+\\d+\\s+)(.*)","\\2",res1[grep("OECD",res1$study_type),"study_type"])
  res1$study_type <- gsub("^\\s+|\\s+$","",res1$study_type)
  res1$study_type <- gsub("^$","-",res1$study_type)
  res1[grep("^\\d+",res1$toxval_numeric),"toxval_numeric"] <- gsub("(^\\d+\\.*\\d*)(\\s*.*)","\\1",res1[grep("^\\d+",res1$toxval_numeric),"toxval_numeric"])
  res1[grep("^[^[:alnum:]]+",res1$toxval_numeric),"toxval_numeric"] <- gsub("(^[^[:alnum:]]\\s*)(\\d+\\.*\\d*)(\\s*.*)","\\2",res1[grep("^[^[:alnum:]]+",res1$toxval_numeric),"toxval_numeric"])
  res1[grep("^[^[:alnum:]]+",res1$toxval_numeric_qualifier),"toxval_numeric_qualifier"] <- gsub("(^[^[:alnum:]])(\\s*\\d+\\.*\\d*)(\\s*.*)","\\1",res1[grep("^[^[:alnum:]]+",res1$toxval_numeric_qualifier),"toxval_numeric_qualifier"])
  res1[grep("^(\\-)",res1$toxval_numeric),"toxval_numeric"] <- NA
  res1[grep("^(\\-)",res1$toxval_numeric_qualifier),"toxval_numeric_qualifier"] <- "-"
  min_val <- function(x) if(all(is.na(x))) NA else min(x,na.rm = T)
  getmin_val <- function(col) str_extract_all(col, pattern = "[0-9\\.]+") %>%
    lapply(.,function(x)min_val(as.numeric(x)) ) %>%
    unlist()
  res1[grep("^[a-zA-Z]+\\s*[a-zA-Z]*\\s*\\:",res1$toxval_numeric),"toxval_numeric"] <- getmin_val(res1[grep("^[a-zA-Z]+\\s*[a-zA-Z]*\\s*\\:",res1$toxval_numeric),"toxval_numeric"])
  #res1[grep("^[a-zA-Z]+\\s*[a-zA-Z]*\\s*\\:",res1$toxval_numeric),"toxval_numeric"] <- gsub("(^[a-zA-Z]+\\s*[a-zA-Z]*\\s*\\:\\s*)(.*)","\\2",res1[grep("^[a-zA-Z]+\\s*[a-zA-Z]*\\s*\\:",res1$toxval_numeric),"toxval_numeric"])
  #res1[grep("^[^[:alnum:]]+",res1$toxval_numeric),"toxval_numeric_qualifier"] <- gsub("(^[^[:alnum:]])(\\s*.*)","\\1",res1[grep("^[^[:alnum:]]+",res1$toxval_numeric),"toxval_numeric"])
  #res1[grep("^[^[:alnum:]]+",res1$toxval_numeric),"toxval_numeric"] <- gsub("(^[^[:alnum:]]\\s*)(.*)","\\2",res1[grep("^[^[:alnum:]]+",res1$toxval_numeric),"toxval_numeric"])
  res1[grep("^(ca\\.)",res1$toxval_numeric, ignore.case = T),"toxval_numeric"] <- gsub("(ca\\.\\s+)(\\d+)(\\s+.*)","\\2",res1[grep("^(ca\\.)",res1$toxval_numeric_qualifier, ignore.case = T),"toxval_numeric"])
  res1[grep("^(ca\\.)",res1$toxval_numeric_qualifier, ignore.case = T),"toxval_numeric_qualifier"] <- gsub("(^ca\\.)(\\s+.*)","\\1",res1[grep("^(ca\\.)",res1$toxval_numeric_qualifier, ignore.case = T),"toxval_numeric_qualifier"])
  # not reported qalifier
  res1[grep("^not", res1$toxval_numeric_qualifier, ignore.case = T), "toxval_numeric_qualifier"] <- gsub("(.*\\()([^[:alnum:]])(\\d+.*)","\\2",res1[grep("^not", res1$toxval_numeric_qualifier, ignore.case = T), "toxval_numeric_qualifier"])
  # not reported units
  res1[grep("^not", res1$toxval_units, ignore.case = T), "toxval_units"] <- gsub("(.*\\()([^[:alnum:]])(\\d+\\.*\\d*)(\\s*)(.*)(\\)\\s*)","\\5",res1[grep("^not", res1$toxval_units, ignore.case = T), "toxval_units"])
  # not reported numeric value
  res1[grep("^not", res1$toxval_numeric, ignore.case = T), "toxval_numeric"] <- gsub("(.*\\()([^[:alnum:]])(\\d+\\.*\\d*)(\\s*.*)","\\3",res1[grep("^not", res1$toxval_numeric, ignore.case = T), "toxval_numeric"])
  # no description qualifier as hyphen
  res1[grep("^no\\s+", res1$toxval_numeric_qualifier, ignore.case = T), "toxval_numeric_qualifier"] <- "-"
  # no description units values as hyphen
  res1[grep("^no\\s+", res1$toxval_units, ignore.case = T), "toxval_units"] <- "-"
  # no description numeric values as empty
  res1[grep("^no\\s+", res1$toxval_numeric, ignore.case = T), "toxval_numeric"] <- NA
  res1[grep("^male female\\s+\\:\\s+[^[:alnum:]]", res1$toxval_numeric_qualifier, ignore.case = T), "toxval_numeric_qualifier"] <- gsub("(^male female\\s+\\:\\s+)([^[:alnum:]])(.*)","\\2",res1[grep("^male female\\s+\\:\\s+[^[:alnum:]]", res1$toxval_numeric_qualifier, ignore.case = T), "toxval_numeric_qualifier"])
  res1[grep("\\(", res1$toxval_numeric_qualifier, ignore.case = T), "toxval_numeric_qualifier"] <- "="
  res1[grep("\\;", res1$toxval_numeric), "toxval_numeric"] <- getmin_val(res1[grep("\\;", res1$toxval_numeric), "toxval_numeric"])
  res1[grep("mg/kg/day", res1$toxval_numeric), "toxval_numeric"] <- NA
  res1[which(res1$toxval_numeric == ""), "toxval_numeric"] <- NA
  res1$toxval_numeric <- as.numeric(res1$toxval_numeric)
  res1[grep("<", res1$toxval_numeric_qualifier, ignore.case = T), "toxval_numeric_qualifier"] <- "<"
  res1[grep("\\?", res1$toxval_numeric_qualifier), "toxval_numeric_qualifier"] <- ">="
  res1[grep("\\d+", res1$toxval_numeric_qualifier), "toxval_numeric_qualifier"] <- "="
  res1[which(res1$toxval_numeric_qualifier == ""), "toxval_numeric_qualifier"] <- "-"
  # value with only unit(mg/kg/day), has no numeric val or qualifier
  res1[which(res1$toxval_numeric_qualifier == "mg/kg/day"), "toxval_numeric_qualifier"] <- "-"
  #res1[grep("^\\d+", res1$toxval_numeric), "toxval_numeric"] <- gsub("(^\\d+\\.*\\d*)(\\s*)(.*)","\\1",res1[grep("^\\d+", res1$toxval_numeric), "toxval_numeric"])
  res1[grep("^\\d+", res1$toxval_units), "toxval_units"] <- gsub("(^\\d+\\.*\\d*)(\\s*)(.*)","\\3",res1[grep("^\\d+", res1$toxval_units), "toxval_units"])
  # <2.5 ( male : 230,  female : 290 mg/kg) units unkown as hyphen
  res1[grep("^[^[:alnum:]]\\d+\\.*\\d*\\s*\\(", res1$toxval_units), "toxval_units"] <- "-"
  res1[grep("titer", res1$toxval_units), "toxval_units"] <- gsub("(.*\\:\\s+\\d+\\s+)(.*)(\\(.*\\))(.*$)","\\2\\4", res1[grep("titer", res1$toxval_units), "toxval_units"])
  res1[grep("\\(", res1$toxval_units), "toxval_units"] <- gsub("(.*?)(\\(.*)","\\1",res1[grep("\\(", res1$toxval_units), "toxval_units"])
  res1$toxval_units <- gsub("\\s+$", "", res1$toxval_units)
  res1[grep("mg/kg/day$", res1$toxval_units), "toxval_units"] <- "mg/kg/day"
  res1[grep("ppm", res1$toxval_units), "toxval_units"] <- "ppm"
  res1[grep("mg/kg$", res1$toxval_units), "toxval_units"] <- "mg/kg"
  res1[grep("\\%$", res1$toxval_units), "toxval_units"] <- "%"
  res1[grep("\\d+$", res1$toxval_units), "toxval_units"] <- "-"
  res1$toxval_units <- gsub("\\-$", "", res1$toxval_units)
  res1[grep("\\d+", res1$toxval_units), "toxval_units"] <- gsub("(.*\\d+\\s+)(.*)","\\2",res1[grep("\\d+", res1$toxval_units), "toxval_units"])
  res1[which(res1$toxval_units == ""), "toxval_units"] <- "-"
  names.list <- c("name","institution","year","associated_publication","document_identifier","casrn","study_type",
                  "vehicle","critical_effect","sex","study_duration_value","study_duration_units","species","strain","exposure_route",
                  "exposure_method","toxval_numeric","toxval_units","toxval_numeric_qualifier","toxval_type","source_url", "toxval_subtype","document_name","record_url")
  res1 <- res1[,(names(res1)%in% names.list)]
  names(res1)[is.element(names(res1),"institution")] <- "subsource"
  names(res1)[is.element(names(res1),"associated_publication")] <- "long_ref"
  names(res1)[is.element(names(res1),"document_identifier")] <- "source_study_id"
  names(res1)[is.element(names(res1),"vehicle")] <- "media"
  #print(str(res1))
  runInsertTable(res1,"new_hess",db,do.halt=T,verbose=F)

  # #####################################################################
  # cat("Build hess_chemical_information table from res1\n")
  # #####################################################################
  # chemical_information <- res1[,c("casrn","name")]
  # chemical_information <- unique(chemical_information[,1:2])
  # chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  # chemical_information <- chemical_information[c('chemical_id','name','casrn')]
  #
  # runInsertTable(chemical_information,"hess_chemical_information",db,do.halt=T,verbose=F)
  #

}

