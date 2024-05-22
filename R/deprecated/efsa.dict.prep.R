#-------------------------------------------------------------------------------------
#' Create dictionaries for preparing EFSA_combined.xlsx produced from efsa.prep.R
#' @param dir The directory where the input data sits, ../efsa2/efsa2_files/

#--------------------------------------------------------------------------------------
efsa.dict.prep <- function(dir) {
  printCurrentFunction()

  #####################################################################
  file <- paste0(dir,"merge2/EFSA_combined.xlsx")
  res <- read.xlsx(file)
  res1 <- res
  #####################################################################
  cat("fix toxval numeric, toxval units and toxval numeric qualifier\n")
  #####################################################################
  E_num <-grep(".*E-.*", res1$toxval_numeric)
  res1[E_num, "toxval_numeric"] <- as.numeric(res1[E_num, "toxval_numeric"])

  res1[is.na(res1$toxval_numeric),"toxval_numeric"] <- 0

  comma_vals <- grep("\\([0-9]+\\,[0-9]+\\)", res1$toxval_numeric, value = T)
  res1[res1$toxval_numeric %in% comma_vals, "toxval_numeric"] <- gsub("\\s*\\([^\\)]+\\)","",res1[res1$toxval_numeric %in% comma_vals, "toxval_numeric"])

  res1[res1$toxval_numeric == "M: 14,9 ml/kg bw; F: 15.6 ml/kg bw", "toxval_numeric"] <- "M: 14.9 ml/kg bw; F: 15.6 ml/kg bw"

  comma_vals <- grep("[0-9]+\\,[0-9]+", res1$toxval_numeric, value = T)
  res1[res1$toxval_numeric %in% comma_vals, "toxval_numeric"] <- gsub("\\,","",res1[res1$toxval_numeric %in% comma_vals, "toxval_numeric"])

  res1$toxval_numeric <- gsub("^\\s+|\\s+$", "", res1$toxval_numeric)

  not_det_vals <- grep("Not detected", res1$toxval_numeric)
  res1[not_det_vals, "toxval_numeric"] <- gsub(".*\\s+\\((.*)\\)", "\\1", res1[not_det_vals, "toxval_numeric"])


  approx_qual <- grep("Approx", res1$toxval_numeric, ignore.case = T)
  res1[approx_qual,"toxval_numeric_qualifier"] <- "~"

  approx_qual <-grep("~", res1$toxval_numeric)
  res1[approx_qual,"toxval_numeric_qualifier"] <- "~"

  res1$toxval_numeric <- gsub("\\[.*\\]",0, res1$toxval_numeric)

  num_qual <- grep("^[^0-9a-zA-Z\\-]+", res1$toxval_numeric)
  res1[num_qual, "toxval_numeric_qualifier"] <- gsub("(^[^[:alnum:]])(\\s*.*)","\\1",res1[num_qual, "toxval_numeric"])

  alt_approx <- grep("98-54-4",res1$casrn)
  res1[alt_approx, "toxval_numeric_qualifier"][which(res1[alt_approx, "toxval_numeric_qualifier"] != "=")] <- "~"


  no_effect <- grep("No adverse effect measured at the only tested dose",res1$toxval_numeric)
  res1[no_effect, "toxval_numeric"] <- 0

  no_effect_high_dose <- grep("No adverse effect measured at the highest tested dose",res1$toxval_numeric)
  res1[no_effect_high_dose, "toxval_numeric"] <- sub("(.*\\s+\\()(.*)(\\)\\s+\\(.*\\))", "\\2", res1[no_effect_high_dose,"toxval_numeric"])

  no_val_num <- grep("^\\bNO\\b\\s+|^\\bNOT\\b|^\\bNR\\b|^\\bND\\b|^\\bNONE\\b|A NOAEL was not determined\\s*|A NOAEL was not determied\\s*|Study not suitable for derivation of a NOAEL", res1$toxval_numeric, ignore.case = T)
  res1[no_val_num, "toxval_numeric"] <- 0

  ml_unit <- grep("ml/kg", res1$toxval_numeric)
  res1[ml_unit, "toxval_units"] <- "ml/kg"

  ppm_unit <- grep("ppm", res1$toxval_numeric)
  ppm_unit <- grep("\\(", res1[ppm_unit,"toxval_numeric"], value = T, invert = T)
  ppm_unit <- which(res1$toxval_numeric %in% ppm_unit)
  res1[ppm_unit,"toxval_units"] <- "ppm"

  for (i in 1:length(res1[ppm_unit,"toxval_numeric"])) {
    res1[ppm_unit,"toxval_numeric"][i] <- min(as.numeric(unlist(regmatches(res1[ppm_unit,"toxval_numeric"][i],gregexpr('[0-9]+',res1[ppm_unit,"toxval_numeric"][i])))))

  }

  mg_unit <- grep("\\d+\\s*mg\\s+", res1$toxval_numeric)
  mg_unit <- grep("Fe",res1[mg_unit,"toxval_numeric"], value = T,invert = T)
  mg_unit <- which(res1$toxval_numeric %in% mg_unit)
  res1[mg_unit, "toxval_units"] <- "mg"
  res1[mg_unit, "toxval_numeric"] <- gsub("(.*\\s*)(\\d+)(\\s+mg)(\\s+.*)", "\\2", res1[mg_unit, "toxval_numeric"])


  paran_num <- grep("\\([a-zA-Z0-9]{1}\\)",res1$toxval_numeric)
  res1[paran_num,"toxval_numeric"] <- gsub("(.*)(\\([a-zA-Z0-9]{1}\\))(.*)", "\\1\\3", res1[paran_num,"toxval_numeric"])
  res1[paran_num,"toxval_numeric"] <- gsub("(.+)(\\([0-9\\.]+\\))(.*$)", "\\1\\3",res1[paran_num,"toxval_numeric"])
  res1[paran_num,"toxval_numeric"] <-gsub("(.+)(\\([a-zA-Z]+\\))(.*$)", "\\1\\3",res1[paran_num,"toxval_numeric"])


  sep_num <- grep("\\;", res1$toxval_numeric)

  sep_num1 <- grep("mg",res1[sep_num,"toxval_numeric"], value = T)
  sep_num1 <- which(res1$toxval_numeric %in% sep_num1)
  res1[sep_num1,"toxval_numeric"] <- gsub("\\(\\d+\\)", "", res1[sep_num1,"toxval_numeric"])
  res1[sep_num1,"toxval_numeric"] <- gsub(".*(\\(.*\\))(\\s*\\;)(.*)(\\(.*\\))","\\1\\2\\4", res1[sep_num1,"toxval_numeric"])
  res1[sep_num1,"toxval_numeric"]<- gsub("^\\d+.*\\(","", res1[sep_num1,"toxval_numeric"])

  for (i in 1:length(res1[sep_num1,"toxval_numeric"])) {
    res1[sep_num1,"toxval_numeric"][i] <- min(as.numeric(unlist(regmatches(res1[sep_num1,"toxval_numeric"][i],gregexpr('[0-9]+',res1[sep_num1,"toxval_numeric"][i])))))

  }


  percent_unit <- grep("%", res1$toxval_numeric)

  mg_with_percent <- grep("mg", res1[percent_unit,"toxval_numeric"], value = T)
  mg_with_percent <- which(res1$toxval_numeric %in% mg_with_percent)
  res1[mg_with_percent,"toxval_numeric"] <- sub(".*?,|.*\\(","", res1[mg_with_percent,"toxval_numeric"])
  res1[mg_with_percent,"toxval_numeric"] <- gsub("(.*\\s+)(\\d+\\s*%)(.*)", "\\1\\3", res1[mg_with_percent,"toxval_numeric"])

  for (i in 1:length(res1[mg_with_percent,"toxval_numeric"])) {
    res1[mg_with_percent,"toxval_numeric"][i] <- min(as.numeric(unlist(regmatches(res1[mg_with_percent,"toxval_numeric"][i],gregexpr('[0-9]+',res1[mg_with_percent,"toxval_numeric"][i])))))

  }

  percent_unit <- grep("%", res1$toxval_numeric)
  res1[percent_unit,"toxval_numeric"] <- gsub("\\s*\\([^\\)]+\\)", "", res1[percent_unit,"toxval_numeric"])
  percent_unit <- grep("%", res1$toxval_numeric)
  res1[percent_unit,"toxval_units"] <- "%"
  res1[percent_unit,"toxval_numeric"] <- as.numeric(unlist(regmatches(res1[percent_unit,"toxval_numeric"],gregexpr('[0-9]+\\.*[0-9]*',res1[percent_unit,"toxval_numeric"]))))


  sep_num <- grep("\\;", res1$toxval_numeric)
  res1[sep_num,"toxval_numeric"] <- gsub("\\([^)]*\\)","",res1[sep_num,"toxval_numeric"])


  sep_num_min <- regmatches(res1[sep_num,"toxval_numeric"],gregexpr('[0-9]+\\.*[0-9]*',res1[sep_num,"toxval_numeric"]))

  for (i in 1:length(sep_num_min)){
    sep_num_min[[i]] <- as.numeric(sep_num_min[[i]])
  }

  for (i in 1:length(sep_num_min)){
    if (length(sep_num_min[[i]]>1))
      sep_num_min[[i]] <-min(sep_num_min[[i]])
    else
      sep_num_min[[i]] <- sep_num_min[[i]]
  }

  for(i in 1:length(sep_num_min)){
    if(length(sep_num_min[[i]] != 0))
      sep_num_min[[i]] <- sep_num_min[[i]]
    else
      sep_num_min[[i]] <- 0
  }

  sep_num_min <- unlist(sep_num_min)
  res1[sep_num,"toxval_numeric"] <- sep_num_min


  special_alpha <- grep("\\s+g/kg", res1$toxval_numeric, value = T)
  res1[res1$toxval_numeric %in% special_alpha, "toxval_numeric"] <- gsub("\\s*[0-9]+\\.*[0-9]*\\s+g/kg", "",res1[res1$toxval_numeric %in% special_alpha, "toxval_numeric"])

  special_alpha <- grep("\\d+\\s+ppm", res1$toxval_numeric, value = T)
  res1[res1$toxval_numeric %in% special_alpha, "toxval_numeric"] <- gsub("\\d+\\s+ppm", "", res1[res1$toxval_numeric %in% special_alpha, "toxval_numeric"])

  special_alpha <- grep("ca.", res1$toxval_numeric, value = T)
  res1[res1$toxval_numeric %in% special_alpha, "toxval_numeric"] <- gsub("\\(.*", "",res1[res1$toxval_numeric %in% special_alpha, "toxval_numeric"])

  special_alpha <- grep("corresponding to|based on", res1$toxval_numeric, value = T)
  res1[res1$toxval_numeric %in% special_alpha, "toxval_numeric"] <- gsub("[0-9]+\\.*[0-9]*\\s+ml/kg|[0-9]+\\.*[0-9]*\\s+g/ml", "",res1[res1$toxval_numeric %in% special_alpha, "toxval_numeric"])
  res1[res1$toxval_numeric %in% special_alpha, "toxval_units"] <- "mg/kg"

  special_alpha <- grep("\\([0-9 \\,\\-]+\\)", res1$toxval_numeric, value = T)
  res1[res1$toxval_numeric %in% special_alpha, "toxval_numeric"] <- gsub("\\(.*$","", res1[res1$toxval_numeric %in% special_alpha, "toxval_numeric"])

  hypen_num <- grep("^\\s*\\-\\s*$", res1$toxval_numeric, value = T)
  res1[res1$toxval_numeric %in% hypen_num, "toxval_numeric"] <- 0

  M_F_num <- grep("\\(.*[A-Z]{1}\\,.*\\)", res1$toxval_numeric, value = T)
  res1[res1$toxval_numeric %in% M_F_num, "toxval_numeric"] <- gsub("\\(.*","",res1[res1$toxval_numeric %in% M_F_num, "toxval_numeric"])


  toxval_numeric <- regmatches(res1$toxval_numeric,gregexpr('[0-9]+\\.*[0-9]*',res1$toxval_numeric))

  for (i in 1:length(toxval_numeric)){
    toxval_numeric[[i]] <- as.numeric(toxval_numeric[[i]])
  }

  for (i in 1:length(toxval_numeric)){
    if (length(toxval_numeric[[i]]>1))
      toxval_numeric[[i]] <-min(toxval_numeric[[i]])
    else
      toxval_numeric[[i]] <- toxval_numeric[[i]]
  }

  for(i in 1:length(toxval_numeric)){
    if(length(toxval_numeric[[i]] != 0))
      toxval_numeric[[i]] <- toxval_numeric[[i]]
    else
      toxval_numeric[[i]] <- 0
  }

  toxval_numeric <- unlist(toxval_numeric)
  toxval_numeric <- as.numeric(toxval_numeric)
  toxval_numeric[toxval_numeric == 0] <- as.numeric(-1)
  res1$toxval_numeric <- toxval_numeric
  res1$toxval_numeric <- as.numeric(res1$toxval_numeric)

  #####################################################################
  cat("fix species ,strain  and sex\n")
  #####################################################################
  res1$species <- gsub('(.*)\\;.*',"\\1", res1$species_original)
  sex_vals <- grep("m/f", res1$sex,  ignore.case = T)
  res1[sex_vals,"sex"]<- gsub("(.*\\s*)(\\w+)(\\/)(\\w+)","\\1/\\2,\\4",res1[sex_vals,"sex"])
  sex_vals <- grep("\\;", res1$sex)
  res1[sex_vals,"sex"]<- gsub(".*\\;(.*)", "\\1", res1[sex_vals,"sex"])
  sex_vals <- grep("\\/group", res1$sex)
  res1[sex_vals,"sex"]<- gsub("(.*\\s*)(\\d+\\/.*)", "/\\1", res1[sex_vals,"sex"])
  sex_vals <-grep(".*\\,.*\\,.*", res1$sex)
  res1[sex_vals,"sex"]<- gsub("(.*)(\\,)(\\s+\\w+\\,.*)", "/\\3", res1[sex_vals,"sex"])
  sex_vals <- grep("\\/", res1$sex)
  res1[sex_vals,"sex"]<- gsub("(.*\\/)(.*)", "\\2", res1[sex_vals,"sex"])
  sex_vals <- grep("\\d+", res1$sex)
  res1[sex_vals,"sex"]<- gsub("(^\\s*[a-zA-Z]{,2})(\\s*\\(*\\d+.*)", "\\1", res1[sex_vals,"sex"])
  sex_vals <-grep("\\(", res1$sex)
  res1[sex_vals,"sex"]<- gsub("(^\\s*[a-zA-Z]{1})(\\s+\\(.*)", "\\1", res1[sex_vals,"sex"])

  non_sex_vals <- grep("\\bm\\b|\\bf\\b|female|male", res1$sex,  invert  = T)
  res1[non_sex_vals,"sex"]<- "-"
  res1$sex <- gsub("female|females", "f",res1$sex)
  res1$sex <- gsub("male|males", "m",res1$sex)
  res1$sex <- gsub("\\b[a-zA-Z]{2,}\\b","", res1$sex)
  res1$sex <- gsub("\\:|\\(|\\)","", res1$sex)
  res1$sex <- gsub("^\\s+|\\s+$","", res1$sex)
  res1$sex <- gsub("\\,\\s+d","", res1$sex)
  res1$sex <- gsub("(\\w+\\s*)\\,(\\s*\\w+)","\\1/\\2", res1$sex)
  res1$sex <- gsub("(\\w+\\s*)\\+(\\s*\\w+)","\\1/\\2", res1$sex)
  res1$sex <- gsub("(\\w+\\s*)\\&(\\s*\\w+)","\\1/\\2", res1$sex)
  res1$sex <- gsub("\\s*\\,\\s*","", res1$sex)
  res1$sex <- gsub("(\\w)\\s+(\\w)","\\1/\\2", res1$sex)
  res1$sex <- gsub("\\s+","", res1$sex)
  res1$sex <- gsub("f/m","m/f", res1$sex)

  res1$species <- gsub("\\bf\\b|\\bm\\b|female|male|nr|^http:.*","",res1$species)
  res1$species <- gsub("(.*)(\\s+\\(.*\\))","\\1",res1$species)

  res1$species <- gsub("[^[:alnum:]]+$", "", res1$species)
  res1$species <- gsub("not given", "-", res1$species)
  res1$species[is.na(res1$species)|res1$species == ""] <- "-"

  strn_vals <- grep("strain", res1$species)
  res1[strn_vals,"strain"] <- gsub("(.*\\,\\s*)(.*)(\\s+strain)", "\\2", res1[strn_vals,"species"])
  res1[strn_vals,"species"] <- gsub("(.*)(\\,\\s*)(.*)(\\s+strain)", "\\1", res1[strn_vals,"species"])


  uniq_sps <- unique(grep("^[a-zA-Z0-9]+$", res1$species, value = T))
  uniq_sps <- c(uniq_sps, "guinea pig")
  uniq_sps <- uniq_sps[uniq_sps != "beagle"]


  res1$strain <- gsub(paste("\\b",uniq_sps,"\\b",collapse = "|",sep = ""), "",res1$species)
  res1$strain <- gsub("\\,","",res1$strain)
  res1$strain[res1$strain == ""] <- "-"
  res1$strain <- gsub("(.*\\(\\w+$)", "\\1)", res1$strain)
  res1$strain <- gsub("^\\s+|\\s+$", "", res1$strain)

  res1$strain[res1$species == "beagle"] <- "beagle"
  res1$species[res1$species == "beagle"] <- "dog"

  res1$strain[res1$species == "sprague-dawley"] <- "sprague-dawley"
  res1$species[res1$species == "sprague-dawley"] <- "rat"

  mult_sps <- grep("\\,", res1$species)
  res1[mult_sps,"strain"] <- gsub(paste("\\b",uniq_sps,"\\b",collapse = "|",sep = ""), "",res1[mult_sps,"species"])
  res1[mult_sps,"strain"] <- gsub("^[^[:alnum:]]|[^[:alnum:]]+$", "",res1[mult_sps,"strain"])
  res1[mult_sps,"strain"][res1[mult_sps,"strain"] == ""] <- "-"

  res1[mult_sps,"species"] <- sapply( str_extract_all( res1[mult_sps,"species"], paste(uniq_sps,collapse = "|",sep = "")), paste0, collapse=",")
  res1[mult_sps,"species"] <- gsub("\\b(\\w+)\\,\\1\\b","\\1", res1[mult_sps,"species"])

  dose_sps <- grep("dose",res1$species)
  res1[dose_sps,"species"] <- gsub("(.*\\:.*)(\\:.*)","\\1",res1[dose_sps,"species"])
  res1$strain <- gsub("^\\s+|\\s+$", "", res1$strain)


  res1$species <- sapply( str_extract_all( res1$species, paste(uniq_sps,collapse = "|",sep = "")), paste0, collapse=",")
  res1$species[res1$species == ""] <- "-"
  res1$species[res1$species == "mice"] <- "mouse"

  res1[grep("crl:cd.*\\(sd\\)", res1$strain),"strain"] <- "crl:cd(sd)"

  #####################################################################
  cat("fix exposure route and exposure method\n")
  #####################################################################
  uniq_route <- c("oral","injection", "inhalation")
  res1$exposure_method <- gsub(paste("\\b",uniq_route,"\\b",collapse = "|",sep = ""), "",res1$exposure_route)
  res1$exposure_method[is.na(res1$exposure_method)|res1$exposure_method ==""] <- "-"
  res1$exposure_method[grep("\\bip\\b|intraperitonel|i\\.p\\.",res1$exposure_method)] <- "intraperitoneal"
  res1$exposure_method[grep("gavage|gavaga",res1$exposure_method)] <- "gavage"
  res1$exposure_method[grep("\\bnr\\b",res1$exposure_method)] <- "-"
  res1$exposure_method[grep("subcutaneos|\\bsc\\b",res1$exposure_method)] <- "subcutaneous"
  res1$exposure_method[grep("diet",res1$exposure_method)] <- "diet"
  res1$exposure_method[grep("feed",res1$exposure_method)] <- "feed"
  res1$exposure_method[grep("^\\s+\\(1\\)$|^\\.$", res1$exposure_method)] <- "oral"
  res1$exposure_method[grep("^\\s+\\(given on a separate dish\\)$", res1$exposure_method)] <- "feed"
  res1$exposure_method <- gsub("\\(|\\)", "", res1$exposure_method)
  res1$exposure_method <- gsub("\\s+\\bin\\b\\s+", "", res1$exposure_method)
  res1$exposure_method[grep("oral",res1$exposure_method)] <- "oral"
  res1$exposure_method <-gsub("^\\s+|\\s+$|^[^[:alnum:]]\\s+","", res1$exposure_method)
  res1$exposure_method[grep("inhalation",res1$exposure_route)] <- "inhalation"

  res1$exposure_route <- sapply( str_extract_all( res1$exposure_route, paste(uniq_route,collapse = "|",sep = "")), paste0, collapse=",")
  res1$exposure_route[res1$exposure_route ==""|res1$exposure_route == "NA"]  <- "-"
  res1$exposure_route[res1$exposure_method == "gavage"] <- "oral"
  res1$exposure_route[res1$exposure_method == "intraperitoneal"] <- "injection"
  res1$exposure_route[res1$exposure_method == "feed"] <- "oral"
  res1$exposure_route[res1$exposure_method == "diet"] <- "oral"
  res1$exposure_route[res1$exposure_method == "drinking water"] <- "oral"
  res1$exposure_route[res1$exposure_method == "drinking"] <- "oral"
  res1$exposure_route[res1$exposure_method == "capsule"] <- "oral"
  res1$exposure_route[res1$exposure_method == "cutaneous application"] <- "topical"
  res1$exposure_route[res1$exposure_method == "tablet"] <- "oral"
  res1$exposure_route[res1$exposure_method == "subcutaneous"] <- "injection"
  res1$exposure_route[res1$exposure_method == "lactation/ drinking water"] <- "oral"
  res1$exposure_route[res1$exposure_method == "gel capsule"] <- "oral"

  #####################################################################
  cat("fix study type\n")
  #####################################################################

  res1$study_type[grep(".*Repro.*Dev|.*Dev.*Repro", res1$study_type, ignore.case = T)] <- "developmental reproductive"
  dev_tox <- grep("Dev", res1$study_type, ignore.case = T)
  res1$study_type[dev_tox][res1$study_type[dev_tox] != "developmental reproductive"] <- "developmental"
  repro_tox <- grep("Repro", res1$study_type, ignore.case = T)
  res1$study_type[repro_tox][res1$study_type[repro_tox] != "developmental reproductive"] <- "reproductive"
  res1$study_type[grep("uterotrophic", res1$study_type, ignore.case = T)] <- "uterotrophic"
  res1$study_type[grep("teratogenicity", res1$study_type, ignore.case = T)] <- "developmental"
  res1$study_type[grep("Dose range-finding", res1$study_type, ignore.case = T)] <- "developmental reproductive"
  res1$study_type[grep("breeding|mating", res1$study_type, ignore.case = T)] <- "reproductive"
  res1$study_duration_value[grep("One generation study", res1$study_type, ignore.case = T)] <- "1"
  res1$study_duration_units[grep("One generation study", res1$study_type, ignore.case = T)] <- "generation"
  res1$study_type[grep("One generation study", res1$study_type, ignore.case = T)] <- "reproductive"
  res1$study_type[grep("Fertility screen:", res1$study_type, ignore.case = T)] <- "reproductive"
  res1$study_type[grep("Teratology", res1$study_type, ignore.case = T)] <- "developmental"
  res1$study_type[is.na(res1$study_type)] <- gsub("(.*\\d+\\s+)(.*)(\\s+toxicity.*)","\\2", res1[is.na(res1$study_type),"long_ref"])
  res1[grep("FACB",res1$study_type),"study_type"] <- gsub("(.*\\d+\\s+)(.*)(\\s+toxicity.*)","\\2", res1[grep("FACB",res1$study_type),"long_ref"])
  res1$study_type[grep(".*Repro.*Dev|.*Dev.*Repro", res1$study_type, ignore.case = T)] <- "developmental reproductive"

  #####################################################################
  cat("fix study duration value and study duration units\n")
  #####################################################################
  res1$study_duration_value[grep("Continuously in diet", res1$study_duration_units, ignore.case = T)]  <- "4"
  res1$study_duration_units[grep("Continuously in diet", res1$study_duration_units, ignore.case = T)]  <- "generation"
  res1$study_duration_value[grep("two generation", res1$study_duration_units, ignore.case = T)] <- "2"
  res1$study_duration_value[grep("Five generation", res1$study_duration_units, ignore.case = T)] <- "5"
  res1$study_duration_units[grep("generation", res1$study_duration_units, ignore.case = T)]  <- "generation"
  res1$study_duration_units[grep(".*year$|.*years$", res1$study_duration_units, ignore.case = T)] <- "year"
  res1$study_duration_value[grep("Toxicity\\s+\\(4\\)", res1$study_duration_units, ignore.case = T)] <- "4"
  res1$study_duration_units[grep("Toxicity\\s+\\(4\\)", res1$study_duration_units, ignore.case = T)] <- "month"
  res1$study_duration_units[grep("month", res1$study_duration_units, ignore.case = T)]  <- "month"

  res1$study_duration_value[grep("\\/", res1$study_duration_units, ignore.case = T)] <- gsub(".*(\\()(\\d+)(\\s+)(\\w+)(\\)$)","\\2",res1$study_duration_units[grep("\\/", res1$study_duration_units, ignore.case = T)])
  res1$study_duration_units[grep("\\/", res1$study_duration_units, ignore.case = T)] <- gsub(".*(\\()(\\d+)(\\s+)(\\w+)(\\)$)","\\4",res1$study_duration_units[grep("\\/", res1$study_duration_units, ignore.case = T)])
  res1$study_duration_value[grep("\\/", res1$study_duration_units, ignore.case = T)] <-gsub("^(\\d+)(\\s+)(\\w+).*","\\1",res1$study_duration_units[grep("\\/", res1$study_duration_units, ignore.case = T)])
  res1$study_duration_units[grep("\\/", res1$study_duration_units, ignore.case = T)] <-gsub("^(\\d+)(\\s+)(\\w+).*","\\3",res1$study_duration_units[grep("\\/", res1$study_duration_units, ignore.case = T)])


  res1$study_duration_value[grep("1 week before mating until 4 days post parturition", res1$study_duration_units, ignore.case = T)] <- "12"
  res1$study_duration_units[grep("1 week before mating until 4 days post parturition", res1$study_duration_units, ignore.case = T)] <- "week"
  res1$study_duration_value[grep("dose administration two weeks", res1$study_duration_units, ignore.case = T)] <- "9"
  res1$study_duration_units[grep("dose administration two weeks", res1$study_duration_units, ignore.case = T)] <- "week"

  mult_dur <- grep("\\d+\\s+\\w+\\s+\\(\\d+\\s+\\w+\\)$", res1$study_duration_units, ignore.case = T)
  res1$study_duration_value[mult_dur] <- gsub("^(\\d+)\\s+(\\w+)\\s+\\(.*", "\\1", res1$study_duration_units[mult_dur])
  res1$study_duration_units[mult_dur] <- gsub("^(\\d+)\\s+(\\w+)\\s+\\(.*", "\\2", res1$study_duration_units[mult_dur])

  res1$study_duration_units[grep("week", res1$study_duration_units, ignore.case = T)] <- "week"
  res1$study_duration_units[grep("wks", res1$study_duration_units, ignore.case = T)] <- "week"
  res1$study_duration_units[grep("Day", res1$study_duration_units, ignore.case = T)] <- "day"
  res1$study_duration_units[grep("^\\d+$", res1$study_duration_units, ignore.case = T)] <- "day"
  res1$study_duration_units[grep(".*[^\\(]\\d+[^\\)].*", res1$study_duration_units, ignore.case = T)] <- "day"

  res1$study_duration_value[grep("NR", res1$study_duration_units, ignore.case = T)] <- "-1"
  res1$study_duration_units[grep("NR", res1$study_duration_units, ignore.case = T)] <- "-"
  res1$study_duration_value[grep("Throughout pregnancy", res1$study_duration_units, ignore.case = T)] <- "-1"
  res1$study_duration_units[grep("Throughout pregnancy", res1$study_duration_units, ignore.case = T)] <- "-"
  res1$study_duration_value[grep("dosed during organogenesis", res1$study_duration_units, ignore.case = T)] <- "-1"
  res1$study_duration_units[grep("dosed during organogenesis", res1$study_duration_units, ignore.case = T)] <- "-"
  res1$study_duration_value[is.na(res1$study_duration_units)] <- "-1"
  res1$study_duration_units[is.na(res1$study_duration_units)] <- "-"

  sing_val <- grep("^\\d+\\s+\\w+$",  res1$study_duration_value)
  res1$study_duration_value[sing_val] <- gsub("^(\\d+)\\s+\\w+$", "\\1", res1$study_duration_value[sing_val])

  mult_vals <- grep("[^a-zA-Z0-9\\-]",  res1$study_duration_value)
  res1$study_duration_value[mult_vals] <- gsub("\\(\\d+\\)","",res1$study_duration_value[mult_vals])


  for (i in 1:length(res1[mult_vals,"study_duration_value"])) {
    res1[mult_vals,"study_duration_value"][i] <- max(as.numeric(unlist(regmatches(res1[mult_vals,"study_duration_value"][i],gregexpr('[0-9]+\\.*[0-9]*',res1[mult_vals,"study_duration_value"][i])))))

  }

  mult_vals <- grep("^\\d+\\.*\\d*$|^\\-\\d+$", res1$study_duration_value,  invert = T)
  for (i in 1:length(res1[mult_vals,"study_duration_value"])) {
    res1[mult_vals,"study_duration_value"][i] <- max(as.numeric(unlist(regmatches(res1[mult_vals,"study_duration_value"][i],gregexpr('[0-9]+\\.*[0-9]*',res1[mult_vals,"study_duration_value"][i])))))

  }

  res1$study_duration_value <- as.numeric(res1$study_duration_value)

  #####################################################################
  cat("building study duration dictionary\n")
  #####################################################################
  efsa2_study_duration_units_dict <- data.frame(res$study_duration_units, res1$study_duration_units, stringsAsFactors = F)
  efsa2_study_duration_units_dict <- unique(efsa2_study_duration_units_dict)
  efsa2_study_duration_value_dict <- data.frame(res$study_duration_value, res1$study_duration_value, stringsAsFactors = F)
  efsa2_study_duration_value_dict <- unique(efsa2_study_duration_value_dict)
  efsa2_study_duration_dict <- cbind(efsa2_study_duration_value_dict,efsa2_study_duration_units_dict)
  names(efsa2_study_duration_dict) <- c("original_study_duration_value", "new_study_duration_value", "original_study_duration_units", "new_study_duration_units")
  file <- paste0(dir,"merge2/efsa2_study_duration_dict.xlsx")
  write.xlsx(efsa2_study_duration_dict,file)

  rm(efsa2_study_duration_units_dict,efsa2_study_duration_value_dict)
  #####################################################################
  cat("building study type dictionary\n")
  #####################################################################
  efsa2_study_type_dict <- data.frame(res$study_type, res1$study_type, stringsAsFactors = F)
  efsa2_study_type_dict <- unique(efsa2_study_type_dict)
  names(efsa2_study_type_dict) <- c("original_study_type", "new_study_type")
  file <- paste0(dir,"merge2/efsa2_study_type_dict.xlsx")
  write.xlsx(efsa2_study_type_dict,file)

  #####################################################################
  cat("building species dictionary\n")
  #####################################################################
  efsa2_species_dict <- data.frame(res$species, res1$species, res1$strain, stringsAsFactors = F)
  efsa2_species_dict <- unique(efsa2_species_dict)
  names(efsa2_species_dict) <- c("original_species","new_species", "new_strain")
  file <- paste0(dir,"merge2/efsa2_species_dict.xlsx")
  write.xlsx(efsa2_species_dict,file)

  #####################################################################
  cat("building exposure  dictionary\n")
  #####################################################################
  efsa2_exposure_dict <- data.frame(res$exposure_route, res1$exposure_route,res1$exposure_method, stringsAsFactors = F)
  efsa2_exposure_dict <- unique(efsa2_exposure_dict)
  names(efsa2_exposure_dict) <- c("original_exposure_route", "new_exposure_route", "new_exposure_method")
  file <- paste0(dir,"merge2/efsa2_exposure_dict.xlsx")
  write.xlsx(efsa2_exposure_dict,file)

  #####################################################################
  cat("building species_sex dictionary\n")
  #####################################################################
  efsa2_sps_sex_dict <- data.frame(res$sex, res1$sex, stringsAsFactors = F)
  efsa2_sps_sex_dict <- unique(efsa2_sps_sex_dict)
  names(efsa2_sps_sex_dict) <- c("original_sex","new_sex")
  file <- paste0(dir,"merge2/efsa2_sps_sex_dict.xlsx")
  write.xlsx(efsa2_sps_sex_dict,file)

  #####################################################################
  cat("building toxval numeric dictionary\n")
  #####################################################################
  efsa2_tox_num_dict <- data.frame(res$toxval_numeric, res1$toxval_numeric, stringsAsFactors = F)
  names(efsa2_tox_num_dict) <- c("original_toxval_numeric", "new_toxval_numeric")
  efsa2_tox_num_dict <- unique(efsa2_tox_num_dict)
  efsa2_tox_num_dict <- efsa2_tox_num_dict[efsa2_tox_num_dict$original_toxval_numeric != efsa2_tox_num_dict$new_toxval_numeric, ]
  file <- paste0(dir,"merge2/efsa2_tox_num_dict.xlsx")
  write.xlsx(efsa2_tox_num_dict,file)

  #####################################################################
  cat("building toxval units dictionary\n")
  #####################################################################
  efsa2_tox_units_dict <- data.frame(res$toxval_units, res1$toxval_units, stringsAsFactors = F)
  names(efsa2_tox_units_dict) <- c("original_toxval_units", "new_toxval_units")
  efsa2_tox_units_dict$row_names <- row.names(efsa2_tox_units_dict)
  efsa2_tox_units_dict <- efsa2_tox_units_dict[efsa2_tox_units_dict$original_toxval_units != efsa2_tox_units_dict$new_toxval_units, ]
  file <- paste0(dir,"merge2/efsa2_tox_units_dict.xlsx")
  write.xlsx(efsa2_tox_units_dict,file)
  #####################################################################
  cat("building toxval numeric qualifier dictionary\n")
  #####################################################################
  efsa2_tox_num_qual_dict <- data.frame(res$toxval_numeric_qualifier, res1$toxval_numeric_qualifier, stringsAsFactors = F)
  names(efsa2_tox_num_qual_dict) <- c("original_toxval_numeric_qualifier", "new_toxval_numeric_qualifier")
  efsa2_tox_num_qual_dict$row_names <- row.names(efsa2_tox_num_qual_dict)
  efsa2_tox_num_qual_dict <- efsa2_tox_num_qual_dict[efsa2_tox_num_qual_dict$original_toxval_numeric_qualifier != efsa2_tox_num_qual_dict$new_toxval_numeric_qualifier, ]
  file <- paste0(dir,"merge2/efsa2_tox_num_qual_dict.xlsx")
  write.xlsx(efsa2_tox_num_qual_dict,file)
}
