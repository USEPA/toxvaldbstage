library("openxlsx")
library('stringr')
library(tibble)
library(janitor)
#--------------------------------------------------------------------------------------
#' Load dod Source into dev_toxval_source_v2. 
#' @param toxval.db The version of toxval into which the source is loaded.
#' @param infile The input file ./dod/dod_files/USACE_ERDC_ERED_database_12_07_2018.xlsx

#--------------------------------------------------------------------------------------

import_dod_ered_source <- function(toxval.db,infile) {
  printCurrentFunction(toxval.db)
  
  #####################################################################
  cat("Build original_dod_ered_table and create dataframe res1 \n")
  #####################################################################
  
  dod_table <- read.xlsx(infile,1)
  dod_table$Date.Modified <- excel_numeric_to_date(as.numeric(as.character(dod_table$Date.Modified)), date_system = "modern")
  dod_table$Date.Modified <- format(dod_table$Date.Modified, format = "%d-%b-%y")
  dod_table["dod_id"] <- c(1:length(dod_table[,1]))
  dod_table <- dod_table[c("dod_id",names(dod_table[-43]))]
  colnames(dod_table) <- c("dod_id","ered_id","ref_id","study_type","species",
                           "common_name","name_categories","life_stage","animal_source",
                           "name","casrn","chemical_group","mixed_chemical","spiked_chemical",
                           "media","exposure_route","exposure_conc","exposure_units","dose",
                           "study_duration","tissue_residue_conc","tissue_residue_conc_units",
                           "test_tissue_type","critical_effect","effect_trend","percentage_effect",
                           "risk","effect_significance","p_value","control_result","comments",
                           "data_source","data_year","phylum","phylum_desc","class","class_desc",
                           "order","order_desc","habitat_desc","environment","ered_date_modified",
                           "moisture_percentage")
  runInsertTable(dod_table,"original_dod_ered_table",toxval.db,do.halt=T,verbose=F)
  res1 <- runQuery("select * from original_dod_ered_table",toxval.db)
  #print(View(res1))
  
  
  #####################################################################
  cat("Build dod_ered_chemical_information table from res1\n")
  #####################################################################
  chemical_information <- res1[,c("name","casrn")]
  chemical_information <- unique(chemical_information[,1:2])
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  chemical_information <- chemical_information[c('chemical_id','name','casrn')]
  
  runInsertTable(chemical_information,"dod_ered_chemical_information",toxval.db,do.halt=T,verbose=F)
  
  #####################################################################
  cat("Build dod_ered_field_descriptions \n")
  #####################################################################
  
  field_descriptions <- read.xlsx(infile, 2)
  colnames(field_descriptions) <- c("field","description")
  field_descriptions["new_field_name"] <- names(res1[-1])
  field_descriptions <- field_descriptions[c("field","new_field_name","description")]
  colnames(field_descriptions) <- c("original_field_name","new_field_name","description")
  
  runInsertTable(field_descriptions,"dod_ered_field_descriptions",toxval.db,do.halt=T,verbose=F)
  
  #####################################################################
  cat("Build dod_ered_key_descriptions by parsing field description and also add the key generations\n")
  #####################################################################
  dod_key1 <- grep("\\:",field_descriptions$description, value = TRUE)
  dod_key2 <- gsub("^.*\\:","",dod_key1)
  dod_key3 <- grep("^.*\\(",dod_key2, value = TRUE)
  
  key_comment1 <- grep("^.*\\(",dod_key1, value = TRUE)
  key_comment <- gsub("\\:.*$","",key_comment1)
  
  
  dod_val1 <- str_match_all(dod_key3, "(?<=\\().+?(?=\\))")
  dod_val <- data.frame(matrix(unlist(dod_val1)), stringsAsFactors = F)
  colnames(dod_val) <- "key_descriptions"
  
  
  dod_key4 <- str_replace_all(dod_key3, "\\([^)]*\\)|\\;|\\,|(or)|\\.", "")
  dod_key5 <- strsplit(dod_key4,"  ")
  dod_keys <- data.frame(matrix(unlist(dod_key5)), stringsAsFactors = F)
  colnames(dod_keys) <- "dod_key"
  
  key_description <- cbind(dod_keys,dod_val)
  key_description[] <- lapply(key_description, as.character)
  dod_gen <- c('generations','generations')
  key_description <- rbind(key_description,dod_gen)
  key_description <- key_description[c(1:22,28,23:27),]
  rownames(key_description) <- 1:nrow(key_description)
  
  
  key_description[,"key_comments"] <- "-"
  
  for (i in 1:length(dod_key5)){
    for (k in 1:nrow(key_description)){
      key_description[k,3][key_description[k,1] %in% dod_key5[[i]]] <- key_comment[i]
    }
  }
  
  runInsertTable(key_description,"dod_ered_key_descriptions",toxval.db,do.halt=T,verbose=F)
  
  #####################################################################
  cat("Build new_dod table from res1\n")
  #####################################################################
  new_dod <- res1[,c(4,5,6,10,11,15,16,17,18,19,20,21,22,24,26,27,29,32,33)]
  names.list <- c("study_type","species","common_name","name","casrn","media","exposure_route","exposure_conc","exposure_units" ,
  "dose","study_duration","tissue_residue_conc","tissue_residue_conc_units","critical_effect","percentage_effect",
  "risk","p_value","data_source","data_year","life_stage")
  print(names(new_dod))
  new_dod <- res1[,(names(res1)%in% names.list)]
  
  new_dod["dod_id"] <- c(1:length(new_dod[,1]))
  new_dod <- new_dod[c('dod_id', names(new_dod[-20]))]
  #####################################################################
  cat("create fields source and source_url in new_dod table\n")
  #####################################################################
  new_dod[,"source"] <- "USACE_ERDC_ERED_database_12_07_2018"
  new_dod[,"subsource"] <- "USACE_ERDC_ERED_database_12_07_2018"
  new_dod[,"source_url"] <- "https://ered.el.erdc.dren.mil/"
  #####################################################################
  cat("Get unique risk values\n")
  #####################################################################
  risk_names <- gsub('[0-9]+','',as.character(new_dod$risk))
  dod_risk <- unique(gsub('\\s+',"",unique((risk_names))))
  #####################################################################
  cat("create a copy of study_duration and assign duration units for long term studies (NOEC,LOEC) as d (days)\n")
  #####################################################################
  new_dod$new_study_duration <- new_dod$study_duration
  dod_noec_loec <- new_dod[grep("NOEC|LOEC", new_dod$risk), c("risk","new_study_duration")]
  dod_noec_loec <- dod_noec_loec[!is.na(dod_noec_loec$new_study_duration),]
  dod_noec_loec$new_study_duration <- gsub(".*[a-zA-Z]+(|\\.)$","",dod_noec_loec$new_study_duration)
  dod_noec_loec <- dod_noec_loec[!(is.na(dod_noec_loec$new_study_duration)| dod_noec_loec$new_study_duration==""),]
  noec_loec_days <- paste(dod_noec_loec$new_study_duration, "d", sep = " ")
  new_dod[,"new_study_duration"][rownames(new_dod) %in% rownames(dod_noec_loec)] <- noec_loec_days
  #####################################################################
  cat("Assign duration units for short term studies (ED,LC) as d (days) if duration value > 4 and as h (hours) if <= 4\n")
  #####################################################################
  dod_ED_LC <- new_dod[grep("ED|LC", new_dod$risk), c("risk","new_study_duration")]
  dod_ED_LC$new_study_duration <- gsub(".*[a-zA-Z]+(|\\.)$","",dod_ED_LC$new_study_duration)
  dod_ED_LC <- dod_ED_LC[!(is.na(dod_ED_LC$new_study_duration)| dod_ED_LC$new_study_duration==""),]
  dod_ED_LC$new_study_duration <-gsub("\\(.*\\)","",dod_ED_LC$new_study_duration)
  dod_ED_LC[2] <- lapply(dod_ED_LC[2], as.numeric)
  dod_ED_LC <- dod_ED_LC[!(is.na(dod_ED_LC$new_study_duration)),]
  ed_lc_hours <- dod_ED_LC[which(dod_ED_LC[,2] > 4),]
  ed_lc_h <-  paste(ed_lc_hours$new_study_duration, "h", sep = " ")
  ed_lc_days <- dod_ED_LC[which(dod_ED_LC[,2] <= 4),]
  ed_lc_d <-  paste(ed_lc_days$new_study_duration, "d", sep = " ")
  dod_ED_LC[,2][rownames(dod_ED_LC) %in% rownames(ed_lc_hours)] <- ed_lc_h
  dod_ED_LC[,2][rownames(dod_ED_LC) %in% rownames(ed_lc_days)] <- ed_lc_d
  
  new_dod[,"new_study_duration"][rownames(new_dod) %in% rownames(dod_ED_LC)] <- dod_ED_LC$new_study_duration
  #####################################################################
  cat("Assign duration units for the rest of the short term studies as h (hours)\n")
  #####################################################################
  short_term_risk <- gsub("NOEC|LOEC|ED|LC", "" , dod_risk)
  short_term_risk <- short_term_risk[!(is.na(short_term_risk)| short_term_risk =="")]
  short_term_risk <- paste0("\\b", short_term_risk, "\\b")
  
  dod_short_term <- new_dod[grep(paste(short_term_risk, collapse = "|"), new_dod$risk), c("risk","new_study_duration")]
  dod_short_term$new_study_duration <- gsub(".*[a-zA-Z]+(|\\.)$","",dod_short_term$new_study_duration)
  dod_short_term <- dod_short_term[!(is.na(dod_short_term$new_study_duration)| dod_short_term$new_study_duration==""),]
  all_short_term <- paste(dod_short_term$new_study_duration, "h", sep = " ")
  new_dod[,"new_study_duration"][rownames(new_dod) %in% rownames(dod_short_term)] <- all_short_term
  #####################################################################
  cat("create fields study_duration_values and study_duration_units\n")
  #####################################################################
  new_dod[,"study_duration_value"] <- "-"
  new_dod[,"study_duration_units"] <- "-"
  sd <- gsub("\\s+","",as.character(new_dod$new_study_duration))
  num_sd <- gsub('[a-zA-Z]+|\\/','',sd)
  char_sd <-gsub('[0-9]+|\\.|[0-9]+\\s*[a-zA-Z]+\\s*[0-9]+|\\;|\\b(st)|\\(|\\)','',sd)
  new_dod$study_duration_value <- num_sd
  new_dod$study_duration_units <- char_sd
  #####################################################################
  cat("fix the cells with multiple values which had characters represented within them eg.4 of 6mo \n")
  #####################################################################
  sub_mid_char <- sd[grep('\\d+.*[a-zA-Z].*\\d+.*',sd)]
  sub_mid_char <- gsub(".*\\+.*", "", sub_mid_char)
  sub_mid_char <- sub_mid_char[!(is.na(sub_mid_char)| sub_mid_char =="")]
  sub_mid_char <- gsub("[a-zA-Z]+$", "", sub_mid_char)
  
  
  mid_char <- as.character(grep('\\d+.*[a-zA-Z].*\\d+.*',sd))
  mid_plus <- as.character(grep(".*\\+.*", sd))
  mid_char <- gsub(paste(mid_plus, collapse = "|"), "", mid_char)
  mid_char <- mid_char[!(is.na(mid_char)| mid_char =="")]
  new_dod[,"study_duration_value"][rownames(new_dod) %in% mid_char] <- sub_mid_char
  
  #####################################################################
  cat(" Convert Exposure concentration data type to numeric, keeping the lower conc values for occurences of multilples within each cell\n")
  #####################################################################
  non_num_expo_conc <- grep('.*\\-|<', new_dod$exposure_conc, value =T)
  lower_conc_value <- gsub('\\-.*|<', '', non_num_expo_conc)
  new_dod[,"exposure_conc_qualifier"] <- "-"
  qual_val <- grep('<', new_dod$exposure_conc, value =T)
  qual_symbols <- gsub('\\d|\\.', '', qual_val)
  new_dod$exposure_conc_qualifier[new_dod$exposure_conc %in% qual_val] <- qual_symbols
  new_dod$exposure_conc[new_dod$exposure_conc %in% non_num_expo_conc] <- lower_conc_value
  new_dod$exposure_conc[is.na(new_dod$exposure_conc)] <- ""
  print(names(new_dod))
  new_dod[,"exposure_conc"] <- sapply(new_dod[,"exposure_conc"], as.numeric)
  names.list1 <- c("dod_id","study_type","species","common_name","name","casrn","media","exposure_route","exposure_conc")
  names.list2 <- c("exposure_units","dose","study_duration","tissue_residue_conc","tissue_residue_conc_units","critical_effect","percentage_effect",        
                   "risk","p_value","data_source","data_year","subsource","source_url","new_study_duration","study_duration_value","study_duration_units")
  new_dod <- new_dod[,(names(new_dod) %in% c(names.list1,"exposure_conc_qualifier", names.list2))]
  #####################################################################
  cat(" rename columns in accordance with toxval\n")
  #####################################################################
  names(new_dod)[names(new_dod)== "exposure_conc"] <- "toxval_numeric"
  names(new_dod)[names(new_dod)== "exposure_conc_qualifier"] <- "toxval_numeric_qualifier"
  names(new_dod)[names(new_dod)== "exposure_units"] <- "toxval_units"
  names(new_dod)[names(new_dod)== "source"] <- "subsource"
  names(new_dod)[names(new_dod)== "risk"] <- "toxval_type"
  new_dod$toxval_type <- gsub("\\s+","", new_dod$toxval_type)
  new_dod$toxval_type <-  gsub(".*\\/.*", "-",new_dod$toxval_type)
  #####################################################################
  cat(" fix inconsistencies in study duration values and units to assign proper data types\n")
  #####################################################################
  new_dod$study_duration_value <- gsub(".*of|.*to|\\+.*","",new_dod$study_duration_value)
  new_dod$study_duration_value <- gsub("\\-\\(","",new_dod$study_duration_value)
  new_dod$study_duration_value <-gsub("\\(.*\\)","", new_dod$study_duration_value)
  new_dod$study_duration_value <-gsub("\\-{2}","",new_dod$study_duration_value)
  new_dod$study_duration_value <-gsub(".*\\-|.*;|\\.{2}|\\)","",new_dod$study_duration_value)
  new_dod$study_duration_value <-gsub("<1","1",new_dod$study_duration_value)
  new_dod$study_duration_value <-gsub("96114","96",new_dod$study_duration_value)
  
  new_dod$study_duration_value <- as.numeric(new_dod$study_duration_value)
  new_dod$study_duration_units <- gsub("\\bh\\b","hours", new_dod$study_duration_units)
  new_dod$study_duration_units <- gsub("\\bd\\b","days", new_dod$study_duration_units)
  new_dod$study_duration_units <- gsub("\\bmo\\b","months", new_dod$study_duration_units)
  new_dod$study_duration_units <- gsub("\\bwk\\b","weeks", new_dod$study_duration_units)
  names(new_dod)[names(new_dod)== "species"] <- "species_scientific_name"
  names(new_dod)[names(new_dod)== "common_name"] <- "species_original"
  names(new_dod)[names(new_dod)== "data_source"] <- "long_ref"
  names(new_dod)[names(new_dod)== "data_year"] <- "year"
  
  #print(names(new_dod))
  #print(View(new_dod))
  runInsertTable(new_dod,"new_dod_ered_table",toxval.db,do.halt=T,verbose=F)
  
  
}

