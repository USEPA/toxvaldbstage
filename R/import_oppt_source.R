library("openxlsx")
library("stringr")
#--------------------------------------------------------------------------------------
#' Load oppt Source Info into dev_toxval_source_v2. 
#' @param toxval.db The version of toxval into which the source info is loaded.
#' @param infile The input file ./oppt/oppt_files/OPPT_data_20181219.xlsx


#--------------------------------------------------------------------------------------

import_oppt_source <- function(toxval.db,infile) {
  printCurrentFunction(toxval.db)

  #####################################################################
  cat("Build original_oppt_table \n")
  #####################################################################
  res1 <- read.xlsx(infile, 1, startRow = 1)
  runInsertTable(res1,"original_oppt_table",toxval.db,do.halt=T,verbose=F)
  
  #####################################################################
  cat("create dataframe res\n")
  #####################################################################
  res <- res1[!(is.na(res1$toxval_numeric)| res1$toxval_numeric == ""),]

  res <- res[is.element(res[,"study_type"],c("Acute Dermal Toxicity",
                                             "Acute Inhalation Toxicity",
                                             "Acute Oral Toxicity",
                                             "Developmental Toxicity",
                                             "Repeated-Dose Toxicity",
                                             "Reproductive Toxicity",
                                             "Reproductive/Developmental Toxicity")),]

  
  # #####################################################################
  # cat("checks, finds and replaces non ascii characters in res with XXX\n")
  # #####################################################################
  # res <- fix.non_ascii(res)
  
  res[is.element(res,"#N/A")] <- "-"
  res$toxval_numeric <- gsub("~","",res$toxval_numeric)
  res$original_toxval_numeric <- res$toxval_numeric
  

  #####################################################################
  cat("keep only standard toxval types and replace unwanted metadata from toxval_type field into dash\n")
  #####################################################################
  unique_types <-unique(res$toxval_type)
  unique_types <- unique(gsub("\\s+", "", unique_types))
  toxval_type_values <- grep("^[A-Z]{0,6}[0-9]{0,6}$",unique_types ,value = T)
  non_toxval_type_values <- grep(paste(toxval_type_values, collapse = "|"), res$toxval_type, value = T, invert = T)
  res$toxval_type[res$toxval_type %in% non_toxval_type_values] <- "-"

  #####################################################################
  cat("remove entries with charcater values which are 'not established' from toxval_numeric\n")
  ####################################################################
  char_toxval_num <- grep("[a-zA-Z]", res$toxval_numeric, value =T)
  res <- res[! res$toxval_numeric %in% unique(char_toxval_num),]

  #####################################################################
  cat("convert percentages into numeric values in toxval_numeric and apply % as unit in corresponding toxval_units\n")
  ####################################################################
  percent_rows <- grep(".*%", res$toxval_numeric)
  res$toxval_units[res$oppt_id %in% percent_rows] <- "%"
  percent_values <- grep(".*%", res$toxval_numeric, value = T)
  percent_num_values <- as.numeric(sub("%","", percent_values))/100
  res$toxval_numeric[res$toxval_numeric %in% percent_values] <- percent_num_values

  #####################################################################
  cat("removing trailing special charcters from toxval_numeric values\n")
  ####################################################################
  res$toxval_numeric <- gsub("\\s+$", "",res$toxval_numeric)
  print(unique(grep(".*[^a-zA-Z0-9]+$", res$toxval_numeric, value = T)))
  res$toxval_numeric <- gsub("?|:|&|\\)", "", res$toxval_numeric)

  #####################################################################
  cat("Remove all comma within values\n")
  ####################################################################
  comma_values <- grep("\\,", res$toxval_numeric, value = T)
  res$toxval_numeric <- gsub("\\,", "", res$toxval_numeric)

  #####################################################################
  cat("remove trailing hyphen from toxval_numeric\n")
  ####################################################################
  res$toxval_numeric <- gsub("\\-$", "", res$toxval_numeric)

  #####################################################################
  cat("find casrn values represented in toxval_numeric and replace them with zero\n")
  ####################################################################
  res$toxval_numeric <- gsub(".*\\-.*\\-.*","0", res$toxval_numeric )

  #####################################################################
  cat("replace the toxval_numeric cell values containing multiple values seperated by backslash,
      standard hyphens and non standard hyphens with the lower concentration value \n")
  ####################################################################
  res$toxval_numeric <- gsub("\\s+", "", res$toxval_numeric)
  mult_toxval <- grep("^\\d+$|^\\d+\\.*\\d+$",res$toxval_numeric, value = T, invert = T)
  #print(unique(mult_toxval))
  mult_toxval_num <- gsub("\\.","a", mult_toxval)
  mult_toxval_num <- gsub("[^[:alnum:]]",",", mult_toxval_num)
  mult_toxval_num <- gsub("a", ".",mult_toxval_num)
  mult_toxval_num <- gsub("µ", "",mult_toxval_num)
  mult_toxval_num<- strsplit(mult_toxval_num, ",")
  mult_toxval_num <- data.frame(do.call(rbind, lapply(mult_toxval_num, unlist)),stringsAsFactors = F)
  mult_toxval_num[,1] <- as.numeric(as.character(mult_toxval_num$X1))
  mult_toxval_num[,2] <- as.numeric(as.character(mult_toxval_num$X2))
  low_conc_value <- apply(mult_toxval_num, 1, FUN = min)

  res$toxval_numeric[res$toxval_numeric %in% mult_toxval] <- low_conc_value

  #####################################################################
  cat("convert data types for toxval_numeric and study duration value as numeric\n")
  ####################################################################
  res$toxval_numeric <- as.numeric(res$toxval_numeric)
  res$study_duration_value <- as.numeric(res$study_duration_value)
  res["oppt_id"] <- c(1:length(res[,1]))
  res <- res[c("oppt_id",names(res[-19]))]
  #####################################################################
  cat("build whole_oppt_table\n")
  ####################################################################
  runInsertTable(res,"whole_oppt_table",toxval.db,do.halt=T,verbose=F)

  #####################################################################
  cat("Build oppt_chemical_information table from res\n")
  #####################################################################
  chemical_information <- res[,c("casrn","name")]
  chemical_information <- unique(chemical_information[,1:2])
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  chemical_information <- chemical_information[c('chemical_id','name','casrn')]
  runInsertTable(chemical_information,"oppt_chemical_information",toxval.db,do.halt=T,verbose=F)


  #####################################################################
  cat("Build oppt_species_dictionary \n")
  #####################################################################
  common_species <- c("rat", "rabbit", "guinea pig", "mouse", "mice", "dog", "cat", "hamster")
  exact_species <- paste0( ".*(",common_species, ").*")
  exact_species_2 <- paste0( "\\b",common_species)
  uniq_sps_names <- unique(grep(paste(exact_species_2, collapse = "|"), res$species, value = T, ignore.case = T))
  new_sps_names <- gsub(paste(exact_species, collapse = "|"), "\\1\\2\\3\\4\\5\\6\\7",uniq_sps_names, ignore.case = T)
  species_table <- data.frame(original_species = uniq_sps_names, new_species = new_sps_names, strain = "-", stringsAsFactors = F)

  sps_name_2 <- combn(common_species, 2, FUN = paste, collapse = ".*")
  mixed_names <- grep(paste(".*(",sps_name_2,").*", collapse = "|"), uniq_sps_names, value = T, ignore.case = T)
  mixed_sps <- list()

  for (i in 1:length(mixed_names)){

    mixed_sps[[i]] <- str_extract_all(mixed_names[i], common_species, simplify = T)

  }

  mixed_species <- paste(mixed_sps, collapse = ",")
  mixed_species <- gsub("(\\\")|\\,","", mixed_species)
  mixed_species <- str_extract_all(mixed_species,"\\(([^()]+)\\)")
  mixed_species <- as.data.frame(mixed_species, col.names = "mixed_species_names", stringsAsFactors = F)
  mixed_species$mixed_species_names <- gsub("\\(|\\)", "", mixed_species$mixed_species_names)
  mixed_species$mixed_species_names <- gsub("\\s+", ",", mixed_species$mixed_species_names)
  mixed_species$mixed_species_names <- gsub("guinea,pig", "guinea pig", mixed_species$mixed_species_names)
  mixed_species$mixed_species_names <- gsub("^\\,|\\,$", "", mixed_species$mixed_species_names)
  mixed_species[4,] <- unname(sapply(mixed_species[4,], function(x) paste(unique(unlist(str_split(x,","))), collapse = ",")))

  species_table$new_species[species_table$original_species %in% mixed_names] <- mixed_species$mixed_species_names
  species_table$new_species <- tolower(species_table$new_species)

  sps_strain <- paste0('(\\w+|\\d+|[^[:alnum:] ]+)?(\\s+|\\-+)?(\\w+|\\d+|[^[:alnum:] ]+)?(\\s+|\\-+)?(\\w+|\\d+|[^[:alnum:] ]+)(?=\\s+', common_species, ')')
  sps_strain1 <- str_extract(species_table$original_species, paste(sps_strain, collapse = "|"))
  sps_strain2 <- unique(gsub('^[^[:alnum:] ]+|^\\s+|\\bor\\b|\\bthe\\b|female|male', "", unique(sps_strain1), ignore.case = T))
  sps_strain3 <- unique(gsub(paste('(.*|.*\\-)?',sps_strain2[c(3,6,11,14,197,199)], collapse = "|"), "", sps_strain2))
  non_sps_strain <- sps_strain3[c(2,4,7,8,20,21,24,36,38,41,44,46,47,48,49,52,55,56,57,66,63,69,73,74,75,82,85,86,95,102,103,109,114,116,118,123,129,128,132,134,135,137,143,145)]

  species_strains <- sps_strain3[! sps_strain3 %in% non_sps_strain]
  species_strains <- gsub("^\\s+|\\s+$", "", species_strains)

  sps_strain_val <- grep(paste("\\b", species_strains, "\\b", collapse = "|", sep = ""), species_table$original_species)
  sps_strain_val1 <- grep(paste("\\b", species_strains, "\\b", collapse = "|", sep = ""), species_table$original_species, value = T)
  sps_strain_val1 <- str_extract(sps_strain_val1, paste(sps_strain, collapse = "|"))
  non_strain_val <- c("ether", "chloride", "acid", "Twelve","time-mated female","Inhalation","triazinetrione","Derivatives", "2-Dichloropropane","the in vivo","female","male","or",
                      "test", "assay", "Diethylenetriamine", "previously", "in", "dianhydride", "of", "mouse", "Cyclohexenecarbinol", "toxicity study in", "Trimethylpentane",
                      "above", "studies", "Six","Dichloropropane","Ethylene carbonate", "nitrobenzene", "Disulfide","Bishydrazine", "F0", "hydrogenated", "toluene", "oxide  Twelve",
                      "week", "Fasted", "dibutyldithiocarbamate", "ethylhexyl ester", "acrylate","Triazinetrione","Five","carbonate","salt","4", "Cumene  Oral", "4-nonanone","Six")

  sps_strain_val1 <- gsub(paste("^", non_strain_val, sep ="", collapse = "|"), "", sps_strain_val1, ignore.case = T)
  sps_strain_val1 <- gsub('^[^[:alnum:] ]+|^\\s+|^(1\\))|\\)$|(female)$', "", sps_strain_val1, ignore.case = T)

  species_table$strain[1:nrow(species_table) %in% sps_strain_val] <- sps_strain_val1

  mixed_sps_strain <- list()

  for (i in 1:length(mixed_names)){
    mixed_sps_strain[[i]] <- str_extract_all(mixed_names[i], paste(sps_strain, collapse = "|"), simplify = T)

  }

  mixed_species_strain <- paste(mixed_sps_strain, collapse = ",")
  mixed_species_strain <- grep(paste("\\b",species_strains, "\\b",sep = "", collapse = "|"),mixed_species_strain, value = T)
  mixed_species_strain <- gsub(paste(common_species, sep = "", collapse = "|"),"", mixed_species_strain)
  mixed_species_strain <- gsub("(\\\")|(s\\s+and)|(and)|(chloride)|(Split LD50s in)|(toxicity seen in)|(the in vivo)","", mixed_species_strain)
  mixed_species_strain <- str_extract_all(mixed_species_strain,"\\(([^()]+)\\)")
  mixed_species_table <- as.data.frame(mixed_species_strain, col.names = "mixed_species_strain_names", stringsAsFactors = F)

  mixed_species_table$mixed_species_strain_names <- gsub("\\(|\\)", "", mixed_species_table$mixed_species_strain_names)
  mixed_species_table$mixed_species_strain_names <- gsub("\\,\\s+\\,", ",", mixed_species_table$mixed_species_strain_names)

  mixed_species_table$mixed_species_strain_names <- gsub("^\\s+|\\s+$", "", mixed_species_table$mixed_species_strain_names)
  mixed_species_table$mixed_species_strain_names <- gsub("^\\,|\\,*$", "", mixed_species_table$mixed_species_strain_names)

  species_table$strain[species_table$original_species %in% mixed_names] <- mixed_species_table$mixed_species_strain_names
  species_table$strain[grep("^\\s+",species_table$strain)] <- gsub("^\\s+","",species_table$strain[grep("^\\s+",species_table$strain)])
  species_table$strain[grep("^\\bto\\b",species_table$strain)] <- gsub("^\\bto\\b","",species_table$strain[grep("^\\bto\\b",species_table$strain)])
  species_table$strain[grep("^\\band\\b",species_table$strain)] <- gsub("^\\band\\b","",species_table$strain[grep("^\\band\\b",species_table$strain)])
  species_table$strain[grep("Crl:CD BR",species_table$original_species, ignore.case = T)] <- "Crl:CD BR"
  species_table$strain[grep("Fischer 344/N",species_table$original_species, ignore.case = T)] <- "Fischer 344/N"
  

  runInsertTable(species_table,"oppt_species_dictionary",toxval.db,do.halt=T,verbose=F)

  #####################################################################
  cat("build new_oppt_table\n")
  ####################################################################

  new_res <- res[,-1]
  new_res$original_species <-new_res$species
  new_res$species[new_res$original_species %in% species_table$original_species] <- species_table$new_species[new_res$original_species %in% species_table$original_species]
  new_res$strain[new_res$original_species %in% species_table$original_species] <- species_table$strain[new_res$original_species %in% species_table$original_species]
  new_res$exposure_method <- "-"
  new_res$risk_assessment_class <- "-"
  for(i in 1:nrow(new_res)) {
    for(col in names(new_res)) {
      new_res[i,col] <- str_trim(new_res[i,col],"both")
    }
    er0 <- new_res[i,"exposure_route"]
    st0 <- new_res[i,"study_type"]
    if(is.element(er0,c("vapor","gavage","drinking water","10% solution","liquid diet"))) new_res[i,"exposure_method"] <- er0
    if(st0=="Acute Dermal Toxicity") {new_res[i,"study_type"]<-"acute";new_res[i,"exposure_route"]<-"dermal";new_res[i,"risk_assessment_class"]<-"acute"}
    if(st0=="Acute Inhalation Toxicity") {new_res[i,"study_type"]<-"acute";new_res[i,"exposure_route"]<-"inhalation";new_res[i,"risk_assessment_class"]<-"acute"}
    if(st0=="Acute Oral Toxicity") {new_res[i,"study_type"]<-"acute";new_res[i,"exposure_route"]<-"oral";new_res[i,"risk_assessment_class"]<-"acute"}
    if(st0=="Developmental Toxicity") {new_res[i,"study_type"]<-"developmental";new_res[i,"risk_assessment_class"]<-"developmental"}
    if(st0=="Repeated-Dose Toxicity") {new_res[i,"study_type"]<-"repeat dose";new_res[i,"risk_assessment_class"]<-"repeat dose"}
    if(st0=="Reproductive Toxicity") {new_res[i,"study_type"]<-"reproductive";new_res[i,"risk_assessment_class"]<-"reproductive"}
    if(st0=="Reproductive/Developmental Toxicity") {new_res[i,"study_type"]<-"reproductive developmental";new_res[i,"risk_assessment_class"]<-"reproductive developmental"}
    if(is.na(new_res[i,"exposure_route"])) {new_res[i,"exposure_route"] <- "-"; new_res[i,"exposure_method"] <- "-"}
    else if(new_res[i,"exposure_route"]=="Oral - other") {new_res[i,"exposure_route"] <- "oral"; new_res[i,"exposure_method"] <- "other"}
    else if(new_res[i,"exposure_route"]=="Oral diet") {new_res[i,"exposure_route"] <- "oral"; new_res[i,"exposure_method"] <- "diet"}
    else if(new_res[i,"exposure_route"]=="Oral drinking water") {new_res[i,"exposure_route"] <- "oral"; new_res[i,"exposure_method"] <- "drinking water"}
    else if(new_res[i,"exposure_route"]=="Oral gavage") {new_res[i,"exposure_route"] <- "oral"; new_res[i,"exposure_method"] <- "gavage"}
    else if(new_res[i,"exposure_route"]=="Other") {new_res[i,"exposure_route"] <- "other"; new_res[i,"exposure_method"] <- "other"}

    tvt <- new_res[i,"toxval_type"]
    if(contains(tvt,"LD50")) tvt <- "LD50"
    else if(contains(tvt,"LC50")) tvt <- "LC50"
    else if(contains(tvt,"LOAEC")) tvt <- "LOAEC"
    else if(contains(tvt,"LOAEL")) tvt <- "LOAEL"
    else if(contains(tvt,"NOAEC")) tvt <- "NOAEC"
    else if(contains(tvt,"NOAEL")) tvt <- "NOAEL"
    new_res[i,"toxval_type"] <- tvt

    tvu <- new_res[i,"toxval_units"]
    if(contains(tvu,"mg/L")) tvu <- "mg/L"
    else if(contains(tvu,"mg/kg")) tvu <- "mg/kg-day"
    else if(contains(tvu,"ppm")) tvu <- "ppm"
    new_res[i,"toxval_units"] <- tvu
    if(contains(new_res[i,"sex"],"sex")) new_res[i,"sex"] <- "male and female"
  }
  new_res <- new_res[is.element(new_res[,"toxval_type"],c("LD50","LC50","LOAEC","LOAEL","NOAEC","NOAEL")),]
  new_res <- new_res[is.element(new_res[,"toxval_units"],c("mg/L","mg/kg-day","ppm")),]
  new_res <- new_res[!is.na(new_res[,"toxval_numeric"]),]
  new_res[,"toxval_numeric"] <- as.numeric(new_res[,"toxval_numeric"])
  new_res["oppt_id"] <- c(1:length(new_res[,1]))
  new_res <- new_res[c("oppt_id",names(new_res[-23]))]
  runInsertTable(new_res,"new_oppt_table",toxval.db,do.halt=T,verbose=F)



}