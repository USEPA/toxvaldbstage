#--------------------------------------------------------------------------------------
#' Load HPVIS Source Info into toxval_source
#' @param db The version of toxval_source into which the source info is loaded.
#' @param filepath The path for all the input xlsx files ./hpvis/hpvis_files
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#--------------------------------------------------------------------------------------
import_hpvis_source <- function(db,
                                filepath="hpvis/hpvis_files",
                                chem.check.halt=T) {
  printCurrentFunction(db)
  filepath = paste0(toxval.config()$datapath,filepath)
  #####################################################################
  cat("build original_hpvis list of dataframes called res incorporating all the input source files \n")
  #####################################################################
  files.list <- list.files(filepath, pattern = "*.xlsx")
  files.list <- paste0( filepath, '/',files.list)
  res <- lapply(files.list,openxlsx::read.xlsx)

  res_names <- c("Ecotox Aquatic Invertebrate Acute","Ecotox Aquatic Invertebrate Chronic","Ecotox Aquatic Plant Acute","Ecotox Aquatic Plant Chronic",
                 "Ecotox Aquatic Vertebrate Acute","Ecotox Aquatic Vertebrate Chronic","Ecotox Soil Organism Chronic","Ecotox Terrestrial Nonmammalian",
                 "Eye Irritation","Genetox In Vitro","Genetox In Vivo","Mammalian Acute","Mammalian Carcinogenicity","Mammalian Devtox","Mammalian Immunotox",
                 "Mammalian Neurotox","Mammalian Repeat Dose","Mammalian Reprotox","search categories","Skin Irritation","Skin Sensitization")

  names(res) <- paste0("hpvis_", res_names)
  names(res) <- gsub(" ","_",names(res))
  names(res) <- tolower(names(res))
  # Add names to files.list so can map the file name at the end
  names(files.list) <- names(res)

  res <- lapply(res, function(x) setNames(x, gsub(x = names(x), pattern = "\\.", replacement = "_")))
  res <- lapply(res, function(x) setNames(x, gsub(x = names(x), pattern = "\\_$", replacement = "")))

  #####################################################################
  cat("fix casrn and chemical name and then form new list of dataframes called res1(excluding search_categories)\n")
  #####################################################################
  cas_categories <- grep("CAS_Number", names(res[[1]]), value = T)
  name_categories <- grep("Chemical_Name", names(res[[1]]), value = T)
  chemical_key_value <- c(cas_categories,name_categories)
  rm(cas_categories,name_categories)
  chemical_keys <- c("C_C_CAS_N", "S_C_CAS_N", "T_S_CAS_N","C_C_N","S_C_N","T_S_C_N")
  chemical_categories_key_table <- data.frame(chemical_keys,chemical_key_value, stringsAsFactors = F)
  rm(chemical_key_value,chemical_keys)

  cas_cols <- lapply(res, function(x) {
    grep("CAS_Number", names(x))
  })

  name_cols <- lapply(res, function(x) {
    grep("Chemical_Name", names(x))
  })

  subset_cas <-""
  for (i in 1:length(res)){
    subset_cas[i]<- lapply(res[i], "[", cas_cols[[i]])
  }

  subset_name <-""
  for (i in 1:length(res)){
    subset_name[i]<- lapply(res[i], "[", name_cols[[i]])
  }

  for (i in 1:length(subset_cas)){
    subset_cas[[i]] <- lapply(subset_cas[[i]], gsub, pattern ="\\n", replacement = "")
    subset_cas[[i]] <- as.data.frame(subset_cas[[i]]) %>% mutate(cas_key1 = ifelse(subset_cas[[i]]$Sponsored_Chemical_CAS_Number %in% subset_cas[[i]]$Test_Substance_CAS_Number,chemical_categories_key_table[2,1], chemical_categories_key_table[3,1]))
    subset_cas[[i]] <- as.data.frame(subset_cas[[i]]) %>% mutate(cas_key2 = ifelse(subset_cas[[i]]$Category_Chemical_CAS_Number %in% subset_cas[[i]]$Test_Substance_CAS_Number,chemical_categories_key_table[1,1], chemical_categories_key_table[3,1]))
    subset_cas[[i]]$cas_key <- paste(subset_cas[[i]]$cas_key1, subset_cas[[i]]$cas_key2, sep = "," )
    subset_cas[[i]] <- subset_cas[[i]][,c(-4,-5)]
  }

  for (i in 1:length(subset_name)){
    subset_name[[i]] <- lapply(subset_name[[i]], gsub, pattern ="\\n", replacement = "")
    subset_name[[i]] <- as.data.frame(subset_name[[i]]) %>% mutate(name_key1 = ifelse(subset_name[[i]]$Sponsored_Chemical_Name %in% subset_name[[i]]$Test_Substance_Chemical_Name,chemical_categories_key_table[5,1], chemical_categories_key_table[6,1]))
    subset_name[[i]] <- as.data.frame(subset_name[[i]]) %>% mutate(name_key2 = ifelse(subset_name[[i]]$Category_Chemical_Name %in% subset_name[[i]]$Test_Substance_Chemical_Name,chemical_categories_key_table[4,1], chemical_categories_key_table[6,1]))
    subset_name[[i]]$name_key <- paste(subset_name[[i]]$name_key1, subset_name[[i]]$name_key2, sep = "," )
    subset_name[[i]] <- subset_name[[i]][,c(-4,-5)]
  }
  chemical_subset <- mapply(cbind, subset_cas, subset_name, SIMPLIFY = F)

  for (i in 1:length(chemical_subset)){
    chemical_subset[[i]] <- chemical_subset[[i]] %>% mutate(casrn1 = ifelse(as.character(chemical_subset[[i]]$Sponsored_Chemical_CAS_Number) == as.character(chemical_subset[[i]]$Test_Substance_CAS_Number),as.character(chemical_subset[[i]]$Sponsored_Chemical_CAS_Number), as.character(chemical_subset[[i]]$Test_Substance_CAS_Number)))
    chemical_subset[[i]] <- chemical_subset[[i]] %>% mutate(casrn2 = ifelse(as.character(chemical_subset[[i]]$Category_Chemical_CAS_Number) == as.character(chemical_subset[[i]]$Test_Substance_CAS_Number),as.character(chemical_subset[[i]]$Category_Chemical_CAS_Number), as.character(chemical_subset[[i]]$Test_Substance_CAS_Number)))
  }
  chemical_subset <- chemical_subset[-19]

  for (i in 1:length(chemical_subset)){
    for ( j in 1:nrow(chemical_subset[[i]])) {
      if (is.na(chemical_subset[[i]]$casrn1[j])  & is.na(chemical_subset[[i]]$casrn2[j])) {
        chemical_subset[[i]]$casrn[j] <- "NA"
      } else if (chemical_subset[[i]]$casrn1[j] == chemical_subset[[i]]$casrn2[j]){
        chemical_subset[[i]]$casrn[j] <- chemical_subset[[i]]$casrn1[j]
      } else {
        chemical_subset[[i]]$casrn[j] <- paste(chemical_subset[[i]]$casrn1[j], chemical_subset[[i]]$casrn2[j], sep = ",")
      }
    }
  }

  for (i in 1:length(chemical_subset)){
    chemical_subset[[i]] <- chemical_subset[[i]] %>% mutate(name1 = ifelse(as.character(chemical_subset[[i]]$Sponsored_Chemical_Name) == as.character(chemical_subset[[i]]$Test_Substance_Chemical_Name),as.character(chemical_subset[[i]]$Sponsored_Chemical_Name), as.character(chemical_subset[[i]]$Test_Substance_Chemical_Name)))
    chemical_subset[[i]] <- chemical_subset[[i]] %>% mutate(name2 = ifelse(as.character(chemical_subset[[i]]$Category_Chemical_Name) == as.character(chemical_subset[[i]]$Test_Substance_Chemical_Name),as.character(chemical_subset[[i]]$Category_Chemical_Name), as.character(chemical_subset[[i]]$Test_Substance_Chemical_Name)))
  }

  for (i in 1:length(chemical_subset)){
    for ( j in 1:nrow(chemical_subset[[i]])) {
      if (is.na(chemical_subset[[i]]$name1[j])  & is.na(chemical_subset[[i]]$name2[j])) {
        chemical_subset[[i]]$name[j] <- "NA"
      } else if (chemical_subset[[i]]$name1[j] == chemical_subset[[i]]$name2[j]){
        chemical_subset[[i]]$name[j] <- chemical_subset[[i]]$name1[j]
      } else {
        chemical_subset[[i]]$name[j] <- paste(chemical_subset[[i]]$name1[j], chemical_subset[[i]]$name2[j], sep = ",")
      }
    }
  }

  for (i in 1:length(chemical_subset)){
    chemical_subset[[i]] <- chemical_subset[[i]][,c(-9,-10,-12,-13)]
  }
  res1 <- res[-19]
  for (i in 1:length(res1)) {
    res1[[i]] <- cbind(res1[[i]], chemical_subset[[i]][,c(9,4,10,8)])
  }

  #####################################################################
  cat("remove the ocuurences of newline character from res1 data  \n")
  #####################################################################
  for (i in 1:length(res1)){
    res1[[i]] <- lapply(res1[[i]], gsub, pattern ="\\n", replacement = "")
    res1[[i]] <- data.frame(res1[[i]], stringsAsFactors = F)
  }
  #####################################################################
  cat("incorporate new_column names and keys to search categories\n")
  #####################################################################
  res$hpvis_search_categories$new_column_name <- gsub(" ","_", res$hpvis_search_categories$name)
  chem_colname_table <- data.frame(useme = rep("-", 6), name = rep("-", 6), description = rep("-", 6), new_column_name = as.vector(chemical_categories_key_table$chemical_key_value), stringsAsFactors = F)
  res$hpvis_search_categories <- rbind(res$hpvis_search_categories, chem_colname_table)
  res$hpvis_search_categories$new_column_name_key <- gsub("[a-z\\:\\(\\)\\-\\'\\.]+", "", res$hpvis_search_categories$new_column_name)
  res$hpvis_search_categories$new_column_name_key <- gsub("_$|--", "",res$hpvis_search_categories$new_column_name_key)
  res$hpvis_search_categories$new_column_name_key <- gsub("__", "_",res$hpvis_search_categories$new_column_name_key)
  #runInsertTable(res$hpvis_search_categories, "new_hpvis_search_categories",db,do.halt=T,verbose=F )

  #####################################################################
  cat("build hpvis source names key table and include hpvis_source_key to res1\n")
  #####################################################################
  hpvis_source_keys_table <- data.frame(source_name = res_names[-19], source_name_key = gsub("[a-z ]+","",res_names[-19]), stringsAsFactors = F)
  hpvis_source_keys_table$source_name_key[c(10,11)] <- c("GIVT","GIVV")
  hpvis_source_keys_table$new_source_names <- names(res1)

  for (i in 1:length(res1)){
    if (names(res1)[i] %in% hpvis_source_keys_table$new_source_names[i]){
      res1[[i]]$hpvis_source_key <- rep(hpvis_source_keys_table$source_name_key[i], nrow(res1[[i]]))
    }
  }

  #runInsertTable(hpvis_source_keys_table, "hpvis_source_name_key_table",db,do.halt=T,verbose=F )

  #####################################################################
  cat("fix row displacement and remove empty columns from res1 dataframes\n")
  #####################################################################
  res1[[2]][176,c(96:133)] <- gsub("",NA,res1[[2]][176,c(96:133)])
  res1[[10]][,c(37:38)] <- gsub("",NA,res1[[10]][,c(37:38)])
  res1[[11]][,c(43:44)] <- gsub("",NA,res1[[11]][,c(43:44)])
  res1[[18]][,c(50:51)] <- gsub("",NA,res1[[18]][,c(50:51)])

  for (i in 1:length(res1)){
    res1[[i]][,grep("(^X$)|(^X\\d+$)", colnames(res1[[i]]))] <- res1[[i]][,grep("(^X$)|(^X\\d+$)", colnames(res1[[i]]))][!sapply(res1[[i]][,grep("(^X$)|(^X\\d+$)", colnames(res1[[i]]))], function(x) all(is.na(x)| x == ""))]
  }

  #####################################################################
  cat("remove the ocuurences of line ending character from res1 data  \n")
  #####################################################################
  for (i in 1:length(res1)){
    res1[[i]] <- lapply(res1[[i]], gsub, pattern ="\\r", replacement = "")
    res1[[i]] <- data.frame(res1[[i]], stringsAsFactors = F)
  }

  #####################################################################
  cat("fix multiple occurences of Exposure duration and exposure duration units by taking the largest duration value
      and its corresponding unit and also create an index name column to represent the source column for duration values \n")
  #####################################################################
  expo_dur_cols <- lapply(res1, function(x) {
    grep("(\\bExposure_Duration\\b|\\bExposure_Period\\b)", names(x))
  })
  expo_dur_units_cols <- lapply(res1, function(x) {
    grep("(\\bExposure_Units\\b|\\bExposure_Period_Units\\b)", names(x))
  })
  subset_expo_dur <-""
  for (i in 1:length(res1)){
    subset_expo_dur[i]<- lapply(res1[i], "[", expo_dur_cols[[i]])
  }

  subset_expo_dur_units <-""
  for (i in 1:length(res1)){
    subset_expo_dur_units[i]<- lapply(res1[i], "[", expo_dur_units_cols[[i]])
  }

  for (i in 1:length(subset_expo_dur)){
    if ( ncol(subset_expo_dur[[i]]) == 3 ) {
      subset_expo_dur[[i]]$combined_exposure_duration <-as.numeric(apply(subset_expo_dur[[i]],1,max))
      subset_expo_dur[[i]]$combined_exposure_duration_index <-as.numeric(apply(subset_expo_dur[[i]][1:3],1,which.max))
      subset_expo_dur[[i]]$combined_exposure_duration_index_name <- subset_expo_dur[[i]]$combined_exposure_duration_index
      subset_expo_dur[[i]]$combined_exposure_duration_index_name <- gsub("3","Exposure_Period", (subset_expo_dur[[i]]$combined_exposure_duration_index_name))
      subset_expo_dur[[i]]$combined_exposure_duration_index_name <- gsub("2","Exposure_Duration.1", (subset_expo_dur[[i]]$combined_exposure_duration_index_name))
      subset_expo_dur[[i]]$combined_exposure_duration_index_name <- gsub("^1$","Exposure_Duration", (subset_expo_dur[[i]]$combined_exposure_duration_index_name))
    }
  }

  for (i in 1:length(subset_expo_dur_units)){
    if ( ncol(subset_expo_dur_units[[i]]) == 3 ) {
      subset_expo_dur_units[[i]] <- subset_expo_dur_units[[i]][c("Exposure_Units.1","Exposure_Units","Exposure_Period_Units")]
    }
  }

  for (i in 1:length(subset_expo_dur_units)){
    if (ncol(subset_expo_dur_units[[i]]) == 3) {
      subset_expo_dur_units[[i]]$combined_exposure_duration_units <- subset_expo_dur_units[[i]][cbind(seq_along(as.numeric(subset_expo_dur[[i]]$combined_exposure_duration_index)), as.numeric(subset_expo_dur[[i]]$combined_exposure_duration_index))]
    }
  }

  hpvis_expo_duration <- mapply(cbind, subset_expo_dur, subset_expo_dur_units, SIMPLIFY = F)

  for (i in 1:length(res1)) {
    if (ncol(hpvis_expo_duration[[i]]) > 3){
      res1[[i]] <- cbind(res1[[i]], hpvis_expo_duration[[i]][,c(4,6,10)])
    }
  }

  #####################################################################
  cat("assign appropriate data types\n")
  #####################################################################
  for (i in 1:length(res1)) {
    res1[[i]] <- lapply(res1[[i]], function(x) type.convert(as.character(x), as.is = T))
    res1[[i]] <- data.frame(res1[[i]], stringsAsFactors = F)
  }

  #####################################################################
  cat("find data frames with seperate fields representing individual toxval type variables, then combine them to form
  single fields of toxval categories(numeric, units, qualifier etc) \n")
  #####################################################################
  tox_cols <- c("LOEC","LOAEC","LOELR","NOEC","NOAEC","NOELR")
  res3 <- res1[which(lapply(res1, function(x)  any(names(x) %in% tox_cols)) == TRUE)]

  for (i in 1:length(res3)){
    cols_2_rm <- grep(paste(c(tox_cols[c(-1,-2)]), collapse = "|"), names(res3[[i]]), value = T )
    tox_type1 <- res3[[i]][ , !(names(res3[[i]]) %in% cols_2_rm)]
    colnames(tox_type1)[which(colnames(tox_type1) == "LOEC")] <- c("toxval_numeric")
    colnames(tox_type1)[which(colnames(tox_type1) == "LOAEL.LOAEC_Units")] <- c("toxval_units")
    colnames(tox_type1)[which(colnames(tox_type1) == "LOEC_Value_Description")] <- c("toxval_numeric_qualifier")
    tox_type1["toxval_type"] <- c(rep("LOEC", nrow(tox_type1)))
    grep(paste(c(tox_cols[c(1,2)]), collapse = "|"), names(tox_type1), value = T )
    colnames(tox_type1)[which(colnames(tox_type1) == "LOEC_Basis_for_Concentration")] <- c("toxval_Basis_for_Concentration")
    colnames(tox_type1)[which(colnames(tox_type1) == "LOEC_Exposure_Duration")] <- c("toxval_Exposure_Duration")
    tox_type1["toxval_exposure_duration_name"] <- c(rep("LOEC_Exposure_Duration", nrow(tox_type1)))
    colnames(tox_type1)[which(colnames(tox_type1) == "LOEC_Exposure_Units")] <- c("toxval_Exposure_Units")
    colnames(tox_type1)[which(colnames(tox_type1) == "LOEC_Upper_Range")] <- c("toxval_Upper_Range")

    cols_2_rm <- grep(paste(c(tox_cols[-3]), collapse = "|"), names(res3[[i]]), value = T )
    tox_type2 <- res3[[i]][ , !(names(res3[[i]]) %in% cols_2_rm)]
    colnames(tox_type2)[which(colnames(tox_type2) == "LOELR")] <- c("toxval_numeric")
    colnames(tox_type2)[which(colnames(tox_type2) == "LOELR_Unit_of_Measure")] <- c("toxval_units")
    colnames(tox_type2)[which(colnames(tox_type2) == "LOELR_Value_Description")] <- c("toxval_numeric_qualifier")
    tox_type2["toxval_type"] <- c(rep("LOELR", nrow(tox_type2)))
    grep(paste(c(tox_cols[c(3)]), collapse = "|"), names(tox_type2), value = T )
    colnames(tox_type2)[which(colnames(tox_type2) == "LOELR_Basis_for_Concentration")] <- c("toxval_Basis_for_Concentration")
    colnames(tox_type2)[which(colnames(tox_type2) == "LOELR_Exposure_Duration")] <- c("toxval_Exposure_Duration")
    tox_type2["toxval_exposure_duration_name"] <- c(rep("LOELR_Exposure_Duration", nrow(tox_type2)))
    colnames(tox_type2)[which(colnames(tox_type2) == "LOELR_Exposure_Units")] <- c("toxval_Exposure_Units")
    colnames(tox_type2)[which(colnames(tox_type2) == "LOELR_Upper_Mean_Value")] <- c("toxval_Upper_Range")
    cols_2_rm <- grep(paste(c(tox_cols[c(-4,-5)]), collapse = "|"), names(res3[[i]]), value = T )
    tox_type3 <- res3[[i]][ , !(names(res3[[i]]) %in% cols_2_rm)]
    colnames(tox_type3)[which(colnames(tox_type3) == "NOEC")] <- c("toxval_numeric")
    colnames(tox_type3)[which(colnames(tox_type3) == "NOAEL.NOAEC_Units")] <- c("toxval_units")
    colnames(tox_type3)[which(colnames(tox_type3) == "NOEC_Value_Description")] <- c("toxval_numeric_qualifier")
    tox_type3["toxval_type"] <- c(rep("NOEC", nrow(tox_type3)))
    grep(paste(c(tox_cols[c(4,5)]), collapse = "|"), names(tox_type3), value = T )
    colnames(tox_type3)[which(colnames(tox_type3) == "NOEC_Concentration_Type")] <- c("toxval_Basis_for_Concentration")
    tox_type3["toxval_Exposure_Duration"] <- c(rep("", nrow(tox_type3)))
    tox_type3["toxval_exposure_duration_name"] <- c(rep("-", nrow(tox_type3)))
    tox_type3["toxval_Exposure_Units"] <- c(rep("-", nrow(tox_type3)))
    tox_type3["toxval_Upper_Range"] <- c(rep("", nrow(tox_type3)))
    cols_2_rm <- grep(paste(c(tox_cols[-6]), collapse = "|"), names(res3[[i]]), value = T )
    tox_type4 <- res3[[i]][ , !(names(res3[[i]]) %in% cols_2_rm)]
    colnames(tox_type4)[which(colnames(tox_type4) == "NOELR")] <- c("toxval_numeric")
    colnames(tox_type4)[which(colnames(tox_type4) == "NOELR_Units")] <- c("toxval_units")
    colnames(tox_type4)[which(colnames(tox_type4) == "NOELR_Value_Description")] <- c("toxval_numeric_qualifier")
    tox_type4["toxval_type"] <- c(rep("NOELR", nrow(tox_type4)))
    grep(paste(c(tox_cols[c(6)]), collapse = "|"), names(tox_type4), value = T )
    colnames(tox_type4)[which(colnames(tox_type4) == "NOELR_Basis_for_Concentration")] <- c("toxval_Basis_for_Concentration")
    colnames(tox_type4)[which(colnames(tox_type4) == "NOELR_Exposure_Duration")] <- c("toxval_Exposure_Duration")
    tox_type4["toxval_exposure_duration_name"] <- c(rep("NOELR_Exposure_Duration", nrow(tox_type4)))
    colnames(tox_type4)[which(colnames(tox_type4) == "NOELR_Exposure_Units")] <- c("toxval_Exposure_Units")
    colnames(tox_type4)[which(colnames(tox_type4) == "NOELR_Upper_Mean_Value")] <- c("toxval_Upper_Range")
    res3[[i]] <- rbind(tox_type1,tox_type2,tox_type3,tox_type4)
  }

  #####################################################################
  cat("combine concentration result type and concentration percentage to form toxval type\n")
  #####################################################################
  res3 <- lapply(res3, function(x) { x$new_toxval_type <- paste(x$Concentration_Result_Type, x$Concentration_Percentage, sep = ""); x })

  #####################################################################
  cat("assign toxval variables for dataframes in ecotox, which are currently represented as concentration variables, and incorporate new toxval variables which
  are absent from these data frames.\n")
  #####################################################################
  for (i in 1:length(res3)){
    cols_2_rm <- grep("^toxval", names(res3[[i]]), value = T )
    tox_type1 <- res3[[i]][ , !(names(res3[[i]]) %in% cols_2_rm)]
    colnames(tox_type1)[which(colnames(tox_type1) == "Concentration_Value")] <- c("toxval_numeric")
    colnames(tox_type1)[which(colnames(tox_type1) == "Concentration_Units")] <- c("toxval_units")
    colnames(tox_type1)[which(colnames(tox_type1) == "Concentration_Value_Description")] <- c("toxval_numeric_qualifier")
    colnames(tox_type1)[which(colnames(tox_type1) == "new_toxval_type")] <- c("toxval_type")
    colnames(tox_type1)[which(colnames(tox_type1) == "Basis_for_Concentration")] <- c("toxval_Basis_for_Concentration")
    tox_type1["toxval_Exposure_Duration"] <- c(rep("", nrow(tox_type1)))
    tox_type1["toxval_exposure_duration_name"] <- c(rep("-", nrow(tox_type1)))
    tox_type1["toxval_Exposure_Units"] <- c(rep("-", nrow(tox_type1)))
    colnames(tox_type1)[which(colnames(tox_type1) == "Concentration_Upper_Value")] <- c("toxval_Upper_Range")
    cols_2_rm <- grep("^Concentration|new_toxval_type", names(res3[[i]]), value = T )
    cols_2_rm <- c(cols_2_rm, "Basis_for_Concentration")
    tox_type2 <- res3[[i]][ , !(names(res3[[i]]) %in% cols_2_rm)]
    tox_type2["Concentration_Percentage"] <- c(rep("", nrow(tox_type2)))
    tox_type2["Concentration_Result_Type"] <- c(rep("-", nrow(tox_type2)))
    res3[[i]] <- rbind(tox_type1,tox_type2)
  }

  #####################################################################
  cat("build duration and duration units from combined exposure duration and toxval exposure duration.
      hpvis_expo_duration2 has the indexes representing the source field name of the duration value and its corresponding unit\n")
  #####################################################################
  expo_dur_cols2 <- lapply(res3, function(x) {
    grep("_Exposure_Duration$|index_name|_exposure_duration_name$", names(x), ignore.case = T)
  })

  subset_expo_dur2 <-""
  for (i in 1:length(res3)){
    subset_expo_dur2[i]<- lapply(res3[i], "[", expo_dur_cols2[[i]])
  }

  cols <- c("combined_exposure_duration","toxval_Exposure_Duration")
  for (i in 1:length(subset_expo_dur2)){
    if ( ncol(subset_expo_dur2[[i]]) == 4 ) {
      subset_expo_dur2[[i]]$duration <- as.numeric(apply((subset_expo_dur2[[i]])[names(subset_expo_dur2[[i]]) %in% cols], 1, max))
      subset_expo_dur2[[i]]$duration <- ifelse(!is.na(subset_expo_dur2[[i]]$combined_exposure_duration)  & is.na(subset_expo_dur2[[i]]$toxval_Exposure_Duration), subset_expo_dur2[[i]]$combined_exposure_duration, subset_expo_dur2[[i]]$duration)
      subset_expo_dur2[[i]]$duration <- ifelse(is.na(subset_expo_dur2[[i]]$combined_exposure_duration) & !is.na(subset_expo_dur2[[i]]$toxval_Exposure_Duration), subset_expo_dur2[[i]]$toxval_Exposure_Duration, subset_expo_dur2[[i]]$duration)
      subset_expo_dur2[[i]]$duration_index <-as.numeric(apply((subset_expo_dur2[[i]])[names(subset_expo_dur2[[i]]) %in% cols], 1, which.max))
      subset_expo_dur2[[i]]$duration_index_name <- subset_expo_dur2[[i]]$duration_index
      subset_expo_dur2[[i]]$duration_index_name <- ifelse((subset_expo_dur2[[i]]$duration_index %in% "1"), subset_expo_dur2[[i]]$combined_exposure_duration_index_name, subset_expo_dur2[[i]]$duration_index_name )
      subset_expo_dur2[[i]]$duration_index_name <- ifelse((subset_expo_dur2[[i]]$duration_index %in% "2"),subset_expo_dur2[[i]]$toxval_exposure_duration_name, subset_expo_dur2[[i]]$duration_index_name )
      subset_expo_dur2[[i]]$duration_index_name <- as.character(subset_expo_dur2[[i]]$duration_index_name)
    }
  }
  subset_expo_dur2 <- lapply(subset_expo_dur2, function(x){x$duration_index <- gsub("^[a-zA-Z]+\\(.*\\)$", "", x$duration_index); x})
  for (i in 1:length(subset_expo_dur2)){
    subset_expo_dur2[[i]]$duration_index <-as.numeric(subset_expo_dur2[[i]]$duration_index)
  }
  expo_dur_units_cols2 <- lapply(res3, function(x) {
    grep("_Exposure_Duration_Units|_exposure_units", names(x), ignore.case = T)
  })
  subset_expo_dur_units2 <-""
  for (i in 1:length(res3)){
    subset_expo_dur_units2[i]<- lapply(res3[i], "[", expo_dur_units_cols2[[i]])
  }
  for (i in 1:length(subset_expo_dur_units2)){
    if (ncol(subset_expo_dur_units2[[i]]) == 2) {
      subset_expo_dur_units2[[i]]$duration_units <- subset_expo_dur_units2[[i]][cbind(seq_along(as.numeric(subset_expo_dur2[[i]]$duration_index)), as.numeric(subset_expo_dur2[[i]]$duration_index))]
    }
  }
  hpvis_expo_duration2 <- mapply(cbind, subset_expo_dur2, subset_expo_dur_units2, SIMPLIFY = F)
  for (i in 1:length(res3)) {
    res3[[i]] <- cbind(res3[[i]], hpvis_expo_duration2[[i]][,c(5,7,10)])
  }

  #####################################################################
  cat("subset res1 by excluding ecotox and then join the updated ecotox to the list of dataframes\n")
  #####################################################################
  res2 <- res1[9:20]
  new_res <- c(res3, res2)

  #####################################################################
  cat("columns with toxval categories\n")
  #####################################################################
  conc_cols <- c("Concentration_Units","Concentration_Value","Concentration_Result_Type","Concentration_Percentage","Concentration_Value_Description","Concentration_Upper_Value","Basis_for_Concentration")

  #####################################################################
  cat("dataframes having these toxval columns in mammalian\n")
  #####################################################################
  mammalian <- grep("hpvis_mammalian", names(new_res), value = T )
  res3 <- new_res[mammalian][lapply(new_res[mammalian], function(x)  any(names(x) %in% conc_cols)) == T]

  #####################################################################
  cat("combine concentration percentage and concentration  result type to form toxval type in mammalian dataframes having fields represented in conc_cols.

  for dataframes which lack these conc_cols fields assign them.\n")
  #####################################################################
  res2 <- new_res[mammalian]
  res2[names(res3)] <- lapply(res2[names(res3)], function(x) { x$toxval_type <- paste(x$Concentration_Result_Type, x$Concentration_Percentage, sep = ""); x })

  for (i in 1:length(res2)){
    colnames(res2[[i]])[which(colnames(res2[[i]]) == "Concentration_Value")] <- c("toxval_numeric")
    colnames(res2[[i]])[which(colnames(res2[[i]]) == "Concentration_Units")] <- c("toxval_units")
    colnames(res2[[i]])[which(colnames(res2[[i]]) == "Concentration_Value_Description")] <- c("toxval_numeric_qualifier")
    colnames(res2[[i]])[which(colnames(res2[[i]]) == "Concentration_Upper_Value")] <- c("toxval_Upper_Range")
    colnames(res2[[i]])[which(colnames(res2[[i]]) == "Basis_for_Concentration")] <- c("toxval_Basis_for_Concentration")
  }

  res2[[2]]["toxval_type"] <- c(rep("-", nrow(res2[[2]])))
  res2[[2]]["toxval_units"] <- c(rep("-", nrow(res2[[2]])))
  res2[[2]]["toxval_numeric"] <- c(rep("", nrow(res2[[2]])))
  res2[[2]]["toxval_numeric_qualifier"] <- c(rep("-", nrow(res2[[2]])))
  res2[[2]]["toxval_Upper_Range"] <- c(rep("", nrow(res2[[2]])))
  res2[[2]]["toxval_Basis_for_Concentration"] <- c(rep("-", nrow(res2[[2]])))
  res2[[4]]["toxval_type"] <- c(rep("-", nrow(res2[[4]])))
  res2[[4]]["toxval_units"] <- c(rep("-", nrow(res2[[4]])))
  res2[[4]]["toxval_numeric"] <- c(rep("", nrow(res2[[4]])))
  res2[[4]]["toxval_numeric_qualifier"] <- c(rep("-", nrow(res2[[4]])))
  res2[[4]]["toxval_Upper_Range"] <- c(rep("", nrow(res2[[4]])))
  res2[[4]]["toxval_Basis_for_Concentration"] <- c(rep("-", nrow(res2[[4]])))
  #####################################################################
  cat("incorporate updated mammalian data frames to new res and call the combined list of dataframes as res_new\n")
  #####################################################################
  res_new <- c(new_res[1:11], res2, new_res[19:20])
  #####################################################################
  cat("rename columns which represent the same field but are named differently in different dataframes\n")
  #####################################################################
  res_new <- lapply(res_new, function(x) setNames(x, gsub(x = names(x), pattern = "Species_or_in_Vitro_System", replacement = "Species")))
  res_new <- lapply(res_new, function(x) setNames(x, gsub(x = names(x), pattern = "Gender", replacement = "Sex")))
  res_new <- lapply(res_new, function(x) setNames(x, gsub(x = names(x), pattern = "Mammalian_Strain", replacement = "Strain")))
  res_new <- lapply(res_new, function(x) setNames(x, gsub(x = names(x), pattern = "Type_of_Study", replacement = "study_type")))

  #####################################################################
  cat("rename exposure period and its unit in dataframes having single entry of both\n")
  #####################################################################
  expo_dur_cols3 <- lapply(res_new, function(x) {
    grep("^Exposure_Period$|Duration|^Exposure_Period_Units$", names(x), ignore.case = T)
  })
  subset_expo_dur3 <-""
  for (i in 1:length(res_new)){
    subset_expo_dur3[i]<- lapply(res_new[i], "[", expo_dur_cols3[[i]])
  }
  names(subset_expo_dur3) <- names(res_new)
  subset_expo_dur3 <- subset_expo_dur3[sapply(subset_expo_dur3, ncol) == 2]
  res_new[names(res_new) %in% names(subset_expo_dur3)] <- lapply(res_new[names(res_new) %in% names(subset_expo_dur3)], function(x) setNames(x, gsub(x = names(x), pattern = "^Exposure_Period$", replacement = "duration")))
  res_new[names(res_new) %in% names(subset_expo_dur3)] <- lapply(res_new[names(res_new) %in% names(subset_expo_dur3)], function(x) setNames(x, gsub(x = names(x), pattern = "^Exposure_Period_Units$", replacement = "duration_units")))

  #####################################################################
  cat("replace dot characters to underscores in all data frame column names \n")
  #####################################################################
  res_new <- lapply(res_new, function(x) setNames(x, gsub(x = names(x), pattern = "\\.", replacement = "_")))

  #####################################################################
  cat("combine species and other species to form new_species with elements seperated by comma, do the same for strain and Route of Administration \n")
  #####################################################################
  cols <- c("Species","Other_Species","Strain","Other_Strain","Route_of_Administration","Other_Route_of_Administration")
  for ( i in 1:length(res_new)){
    res_new[[i]][setdiff(cols,names(res_new[[i]]))] <- ""
  }

  for ( i in 1:length(res_new)){
    res_new[[i]]$new_species <- paste(res_new[[i]]$Species,res_new[[i]]$Other_Species, sep = ",")
    res_new[[i]]$new_species <- gsub("\\,$|^\\,", "", res_new[[i]]$new_species)
    res_new[[i]]$new_Strain <- paste(res_new[[i]]$Strain,res_new[[i]]$Other_Strain, sep = ",")
    res_new[[i]]$new_Strain <- gsub("\\,$|^\\,", "", res_new[[i]]$new_Strain)
    res_new[[i]]$new_Route_of_Administration <- paste(res_new[[i]]$Route_of_Administration,res_new[[i]]$Other_Route_of_Administration, sep = ",")
    res_new[[i]]$new_Route_of_Administration <- gsub("\\,$|^\\,", "", res_new[[i]]$new_Route_of_Administration)
  }

  #####################################################################
  cat("change column names for Category, Sponsored and Test_Substance cas and name columns to include the corresponding keys along with the name. eg: test_substance_cas_number_t_s_cas_n\n")
  #####################################################################
  for ( i in 1:length(res_new)){
    old_cas_names <- chemical_categories_key_table$chemical_key_value
    new_cas_names <- paste(chemical_categories_key_table$chemical_key_value,  "_", chemical_categories_key_table$chemical_keys, sep = "")
    names(res_new[[i]])[match(old_cas_names, names(res_new[[i]]))] = new_cas_names
  }

  #####################################################################
  cat("fix multiple underscores and trailing underscores in column names\n")
  #####################################################################
  res_new <- lapply(res_new, function(x) setNames(x, gsub(x = names(x), pattern = "_+", replacement = "_")))
  res_new <- lapply(res_new, function(x) setNames(x, gsub(x = names(x), pattern = "_$", replacement = "")))
  #####################################################################
  cat("order column names alphabetically\n")
  #####################################################################
  res_new <- lapply(res_new, function(x) x[,order(names(x))])
  #####################################################################
  cat("assign appropriate data types\n")
  #####################################################################
  for (i in 1:length(res_new)) {
    res_new[[i]] <- lapply(res_new[[i]], function(x) type.convert(as.character(x), as.is = T))
    res_new[[i]] <- data.frame(res_new[[i]], stringsAsFactors = F)
    res_new[[i]][sapply(res_new[[i]], function(x) all(is.na(x) == T))] <- ""
  }

  # #####################################################################
  # cat("build new hpvis tables\n")
  # #####################################################################
  # table_names <- tolower(c("hpvis_ecotox_aquatic_invertebrate_acute","hpvis_ecotox_aquatic_invertebrate_chronic",
  #                          "hpvis_ecotox_aquatic_plant_acute","hpvis_ecotox_aquatic_plant_chronic","hpvis_ecotox_aquatic_vertebrate_acute",
  #                          "hpvis_ecotox_aquatic_vertebrate_chronic","hpvis_ecotox_soil_organism_chronic","hpvis_ecotox_terrestrial_nonmammalian","hpvis_eye_irritation",
  #                          "hpvis_genetox_in_vitro","hpvis_genetox_in_vivo", "hpvis_mammalian_acute","hpvis_mammalian_carcinogenicity","hpvis_mammalian_devtox",
  #                          "hpvis_mammalian_immunotox","hpvis_mammalian_neurotox","hpvis_mammalian_repeat_dose","hpvis_mammalian_reprotox",
  #                          "hpvis_skin_irritation","hpvis_skin_sensitization"))
  # table_names <- paste0("new_", table_names)

  # stop = FALSE
  # for( i in 1:length(res_new)){
  #   for (j in 1:length(table_names)){
  #     runInsertTable(res_new[[i]],table_names[j],db,do.halt=T,verbose=F)
  #     i <- i+1
  #     if (i == length(res_new)+1){
  #       stop = TRUE
  #       break
  #     }
  #   }
  #   if (stop){break}
  # }
  #####################################################################
  cat("required columns contain the major columns represented in the existing toxval source\n")
  #####################################################################
  reqd_cols <- c("casrn","cas_key","name","name_key","toxval_type","toxval_numeric","toxval_numeric_qualifier","toxval_units","toxval_Basis_for_Concentration","toxval_Upper_Range","new_species","sex",
                 "new_Strain","new_Route_of_Administration","Type_of_Exposure","duration","duration_units","duration_index_name","Year_Study_Performed","Program_Flag","Consortium_Name",
                 "study_type","GLP","Reliability","Study_Reference","hpvis_source_key","Population")

  #####################################################################
  cat("get the common columns in all res_new dataframes\n")
  #####################################################################
  common_cols <- Reduce(intersect, lapply(res_new, colnames))
  mutual_cols <- common_cols[which(common_cols %in% reqd_cols)]

  #####################################################################
  cat("get column names which are present in common cols but not in required cols , combine them to form the new required cols\n")
  #####################################################################
  non_mutual_cols <- setdiff(common_cols, mutual_cols)
  non_mutual_cols <- setdiff(non_mutual_cols, cols)
  reqd_cols <- c(reqd_cols, non_mutual_cols)

  #####################################################################
  cat("create new_res by subsetting res_new with the columns present in required cols\n")
  #####################################################################
  new_res <- lapply(res_new, function(x) subset(x, select = intersect(reqd_cols, colnames(x))))

  #####################################################################
  cat("in case any required col not being present in a data frame add that column and assign it as empty\n")
  #####################################################################
  for ( i in 1:length(new_res)){
    new_res[[i]][setdiff(reqd_cols,names(new_res[[i]]))] <- ""
  }

  #####################################################################
  cat("combine all dataframes to build new_combined_hpvis_table\n")
  #####################################################################
  # Add files.list name connection to each dataframe
  new_res = lapply(names(new_res), function(f){
    new_res[[f]] %>%
      mutate(raw_input_file = basename(files.list[[f]]))
  })
  hpvis_all_data <- do.call("rbind",new_res)
  row.names(hpvis_all_data) <- NULL
  hpvis_all_data <- unique(hpvis_all_data)
  hpvis_all_data["hpvis_id"] <- c(1:length(hpvis_all_data[,1]))
  #hpvis_all_data <- hpvis_all_data[c("hpvis_id",names(hpvis_all_data[-45]))]
  # reorder hpvis_id to first column
  hpvis_all_data <- hpvis_all_data[c("hpvis_id",names(hpvis_all_data)[!names(hpvis_all_data) %in% c("hpvis_id")])]
  hpvis_all_data <- lapply(hpvis_all_data, function(x) type.convert(as.character(x), as.is = T))
  hpvis_all_data <- data.frame(hpvis_all_data, stringsAsFactors = F)
  hpvis_all_data[sapply(hpvis_all_data, function(x) all(is.na(x) == T))] <- ""
  names(hpvis_all_data) <- tolower(names(hpvis_all_data))
  names(hpvis_all_data) <- gsub("\\.$","\\)",names(hpvis_all_data))
  names(hpvis_all_data) <- gsub("\\.","\\(",names(hpvis_all_data))
  hpvis_all_data$new_study_type <- "-"
  hpvis_all_data[grep(".*A$", hpvis_all_data$hpvis_source_key), "new_study_type"] <- "acute"
  hpvis_all_data[grep("E.*C$", hpvis_all_data$hpvis_source_key), "new_study_type"] <- "chronic"
  hpvis_all_data[grep(".*RD$", hpvis_all_data$hpvis_source_key), "new_study_type"] <- "repeat-dose"
  hpvis_all_data[grep("MR$", hpvis_all_data$hpvis_source_key), "new_study_type"] <- "reproductive"
  hpvis_all_data[grep("MD$", hpvis_all_data$hpvis_source_key), "new_study_type"] <- "developmental"
  hpvis_all_data[grep("MC$", hpvis_all_data$hpvis_source_key), "new_study_type"] <- "cancer"
  hpvis_all_data[grep("MI$", hpvis_all_data$hpvis_source_key), "new_study_type"] <- "immunotoxicity"
  hpvis_all_data[grep("MN$", hpvis_all_data$hpvis_source_key), "new_study_type"] <- "neurotoxicity"
  hpvis_all_data[grep("ETN", hpvis_all_data$hpvis_source_key), "new_study_type"] <- "terrestrial nonmammalian"
  hpvis_all_data[grep("EI", hpvis_all_data$hpvis_source_key), "new_study_type"] <- "eye irritation"
  hpvis_all_data[grep("^G.*V$", hpvis_all_data$hpvis_source_key), "new_study_type"] <- "genetic toxicity - in vivo"
  hpvis_all_data[grep("^G.*T$", hpvis_all_data$hpvis_source_key), "new_study_type"] <- "genetic toxicity - in vitro"
  hpvis_all_data[grep("SI", hpvis_all_data$hpvis_source_key), "new_study_type"] <- "skin irritation"
  hpvis_all_data[grep("SS", hpvis_all_data$hpvis_source_key), "new_study_type"] <- "skin sensitization"
  res = hpvis_all_data
  nlist = c(
    "hpvis_id","casrn","cas_key",
    "name","name_key","toxval_type",
    "toxval_numeric","toxval_numeric_qualifier","toxval_units",
    "toxval_basis_for_concentration","toxval_upper_range","new_species",
    "new_strain","new_route_of_administration","duration",
    "duration_units","duration_index_name","year_study_performed",
    "program_flag","consortium_name","glp",
    "reliability","study_reference","hpvis_source_key",
    "category_chemical_cas_number_c_c_cas_n","category_chemical_name_c_c_n","dose_remarks",
    "key_study_sponsor_indicator","method_guideline_followed","reliability_remarks",
    "results_remarks","sponsor_name","sponsored_chemical_cas_number_s_c_cas_n",
    "sponsored_chemical_name_s_c_n","sponsored_chemical_result_type","submission_name",
    "submitter_s_name","test_conditions_remarks","test_substance_cas_number_t_s_cas_n",
    "test_substance_chemical_name_t_s_c_n","test_substance_purity","sex",
    "type_of_exposure","study_type","population",
    "new_study_type", "raw_input_file"
  )


  nlist = c(
    "hpvis_id","casrn",
    "name","toxval_type",
    "toxval_numeric","toxval_numeric_qualifier","toxval_units",
    "toxval_basis_for_concentration","toxval_upper_range",
    "new_species","new_strain","sex",
    "population",
    "type_of_exposure",
    "new_route_of_administration",
    "duration", "duration_units","duration_index_name",
    "new_study_type",
    "year_study_performed",
    "program_flag","consortium_name",
    "reliability","study_reference","hpvis_source_key",
    "dose_remarks",
    "glp","key_study_sponsor_indicator","method_guideline_followed",
    "reliability_remarks","test_substance_purity",
    "results_remarks",
    "test_conditions_remarks",
    "submission_name","sponsor_name","submitter_s_name",
    "sponsored_chemical_result_type", "raw_input_file")
  res = res[,nlist]

  nlist = c(
    "hpvis_id","casrn",
    "name","toxval_type",
    "toxval_numeric","toxval_numeric_qualifier","toxval_units",
    "toxval_basis_for_concentration","toxval_upper_range",
    "species","strain","sex",
    "population",
    "exposure_method",
    "exposure_route",
    "study_duration_value", "study_duration_units","duration_index_name",
    "study_type",
    "year",
    "program_flag","consortium_name",
    "reliability","study_reference","hpvis_source_key",
    "dose_remarks",
    "glp","key_study_sponsor_indicator","method_guideline_followed",
    "reliability_remarks","test_substance_purity",
    "results_remarks",
    "test_conditions_remarks",
    "submission_name","sponsor_name","submitter_s_name",
    "sponsored_chemical_result_type", "raw_input_file")
  names(res) = nlist
  cat(nrow(res),"\n")
  res = res[!is.na(res$toxval_numeric),]
  cat(nrow(res),"\n")
  res = res[!is.na(res$casrn),]
  cat(nrow(res),"\n")
  res = res[!is.na(res$name),]
  cat(nrow(res),"\n")
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="HPVIS",table="source_hpvis",res=res,F,T,T)
}
#   #####################################################################
#   cat("Do the chemical checking\n")
#   #####################################################################
#   source = "HPVIS"
#   res = as.data.frame(hpvis_all_data)
#   res = res[!is.na(res$casrn),]
#   res = res[!is.na(res$name),]
#   res$clowder_id = "-"
#   res = fix.non_ascii.v2(res,source)
#   res = source_chemical.process(db,res,source,chem.check.halt,casrn.col="casrn",name.col="name",verbose=F)
#   #####################################################################
#   cat("Build the hash key and load the data \n")
#   #####################################################################
#   res = subset(res,select=-c(chemical_index))
#   toxval_source.hash.and.load(db,source,"new_combined_hpvis_table",F,F,res)
#   browser()
#   return(1)
#
#
#
#   runInsertTable(hpvis_all_data, "new_combined_hpvis_table",db,do.halt=T,verbose=F )
#
#
#
#
#    toxval_hpvis_all_data <- hpvis_all_data[!(is.na(hpvis_all_data$toxval_numeric)| hpvis_all_data$toxval_numeric ==""),]
#
#   #####################################################################
#   cat("combine just mammalian data\n")
#   #####################################################################
#   mammalian_all_data <- do.call("rbind", new_res[c(12:18)])
#   row.names(mammalian_all_data) <- NULL
#   mammalian_all_data <- unique(mammalian_all_data)
#   mammalian_all_data["hpvis_id"] <- c(1:length(mammalian_all_data[,1]))
#   mammalian_all_data <- mammalian_all_data[c("hpvis_id",names(mammalian_all_data[-45]))]
#   mammalian_all_data <- lapply(mammalian_all_data, function(x) type.convert(as.character(x), as.is = T))
#   mammalian_all_data <- data.frame(mammalian_all_data, stringsAsFactors = F)
#   mammalian_all_data[sapply(mammalian_all_data, function(x) all(is.na(x) == T))] <- ""
#   names(mammalian_all_data) <- tolower(names(mammalian_all_data))
#   names(mammalian_all_data) <- gsub("\\.$","\\)",names(mammalian_all_data))
#   names(mammalian_all_data) <- gsub("\\.","\\(",names(mammalian_all_data))
#   toxval_mammalian_all_data <- mammalian_all_data[!(is.na(mammalian_all_data$toxval_numeric)| mammalian_all_data$toxval_numeric ==""),]
#
#   # #####################################################################
#   # cat("Build hpvis_chemical_information table from hpvis_all_data\n")
#   # #####################################################################
#   # chemical_information <- hpvis_all_data[,c("casrn","name")]
#   # chemical_information <- unique(chemical_information[,1:2])
#   # chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
#   # chemical_information <- chemical_information[c('chemical_id','name','casrn')]
#   #
#   # runInsertTable(chemical_information, "hpvis_chemical_information",db,do.halt=T,verbose=F )
#
#
# }
#
#
#
#
#
