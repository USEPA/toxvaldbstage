#--------------------------------------------------------------------------------------
#' @description Load HPVIS data into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_hpvis_source
#' @return None; data is pushed to toxval_source
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[openxlsx]{read.xlsx}}
#'  \code{\link[stats]{setNames}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{setops}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{na_if}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{row_number}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{bind_rows}}
#'  \code{\link[utils]{type.convert}}
#'  \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_replace}}, \code{\link[stringr]{str_extract}}
#' @rdname import_hpvis_source
#' @export
#' @importFrom openxlsx read.xlsx
#' @importFrom stats setNames
#' @importFrom dplyr mutate setdiff intersect filter across where na_if case_when row_number select bind_rows
#' @importFrom utils type.convert
#' @importFrom stringr str_squish str_replace_all str_replace str_extract
#' @importFrom tidyr unite
#--------------------------------------------------------------------------------------
import_hpvis_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "HPVIS"
  source_table = "source_hpvis"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2019-12-20")
  dir = paste0(toxval.config()$datapath,"hpvis/hpvis_files/")

  # Build original_hpvis list of dataframes called res incorporating all the input source files
  files.list <- list.files(dir, pattern = "*.xlsx")
  files.list <- paste0(dir, '/',files.list)
  res <- lapply(files.list, openxlsx::read.xlsx)

  res_names <- c("Ecotox Aquatic Invertebrate Acute","Ecotox Aquatic Invertebrate Chronic","Ecotox Aquatic Plant Acute","Ecotox Aquatic Plant Chronic",
                 "Ecotox Aquatic Vertebrate Acute","Ecotox Aquatic Vertebrate Chronic","Ecotox Soil Organism Chronic","Ecotox Terrestrial Nonmammalian",
                 "Eye Irritation","Genetox In Vitro","Genetox In Vivo","Mammalian Acute","Mammalian Carcinogenicity","Mammalian Devtox","Mammalian Immunotox",
                 "Mammalian Neurotox","Mammalian Repeat Dose","Mammalian Reprotox","search categories","Skin Irritation","Skin Sensitization")

  names(res) <- paste0("hpvis_", res_names)
  names(res) <- gsub(" ","_",names(res))
  names(res) <- tolower(names(res))
  # Add names to files.list so can map the file name at the end
  names(files.list) <- names(res)

  res <- lapply(res, function(x) stats::setNames(x, gsub(x = names(x), pattern = "\\.", replacement = "_")))
  res <- lapply(res, function(x) stats::setNames(x, gsub(x = names(x), pattern = "\\_$", replacement = "")))
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  # Fix casrn and chemical name and then form new list of dataframes called res1(excluding search_categories)
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
    subset_cas[[i]] <- as.data.frame(subset_cas[[i]]) %>% dplyr::mutate(cas_key1 = ifelse(subset_cas[[i]]$Sponsored_Chemical_CAS_Number %in% subset_cas[[i]]$Test_Substance_CAS_Number,chemical_categories_key_table[2,1], chemical_categories_key_table[3,1]))
    subset_cas[[i]] <- as.data.frame(subset_cas[[i]]) %>% dplyr::mutate(cas_key2 = ifelse(subset_cas[[i]]$Category_Chemical_CAS_Number %in% subset_cas[[i]]$Test_Substance_CAS_Number,chemical_categories_key_table[1,1], chemical_categories_key_table[3,1]))
    subset_cas[[i]]$cas_key <- paste(subset_cas[[i]]$cas_key1, subset_cas[[i]]$cas_key2, sep = "," )
    subset_cas[[i]] <- subset_cas[[i]][,c(-4,-5)]
  }

  for (i in 1:length(subset_name)){
    subset_name[[i]] <- lapply(subset_name[[i]], gsub, pattern ="\\n", replacement = "")
    subset_name[[i]] <- as.data.frame(subset_name[[i]]) %>% dplyr::mutate(name_key1 = ifelse(subset_name[[i]]$Sponsored_Chemical_Name %in% subset_name[[i]]$Test_Substance_Chemical_Name,chemical_categories_key_table[5,1], chemical_categories_key_table[6,1]))
    subset_name[[i]] <- as.data.frame(subset_name[[i]]) %>% dplyr::mutate(name_key2 = ifelse(subset_name[[i]]$Category_Chemical_Name %in% subset_name[[i]]$Test_Substance_Chemical_Name,chemical_categories_key_table[4,1], chemical_categories_key_table[6,1]))
    subset_name[[i]]$name_key <- paste(subset_name[[i]]$name_key1, subset_name[[i]]$name_key2, sep = "," )
    subset_name[[i]] <- subset_name[[i]][,c(-4,-5)]
  }
  chemical_subset <- mapply(cbind, subset_cas, subset_name, SIMPLIFY = F)

  for (i in 1:length(chemical_subset)){
    chemical_subset[[i]] <- chemical_subset[[i]] %>% dplyr::mutate(casrn1 = ifelse(as.character(chemical_subset[[i]]$Sponsored_Chemical_CAS_Number) == as.character(chemical_subset[[i]]$Test_Substance_CAS_Number),as.character(chemical_subset[[i]]$Sponsored_Chemical_CAS_Number), as.character(chemical_subset[[i]]$Test_Substance_CAS_Number)))
    chemical_subset[[i]] <- chemical_subset[[i]] %>% dplyr::mutate(casrn2 = ifelse(as.character(chemical_subset[[i]]$Category_Chemical_CAS_Number) == as.character(chemical_subset[[i]]$Test_Substance_CAS_Number),as.character(chemical_subset[[i]]$Category_Chemical_CAS_Number), as.character(chemical_subset[[i]]$Test_Substance_CAS_Number)))
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
    chemical_subset[[i]] <- chemical_subset[[i]] %>% dplyr::mutate(name1 = ifelse(as.character(chemical_subset[[i]]$Sponsored_Chemical_Name) == as.character(chemical_subset[[i]]$Test_Substance_Chemical_Name),as.character(chemical_subset[[i]]$Sponsored_Chemical_Name), as.character(chemical_subset[[i]]$Test_Substance_Chemical_Name)))
    chemical_subset[[i]] <- chemical_subset[[i]] %>% dplyr::mutate(name2 = ifelse(as.character(chemical_subset[[i]]$Category_Chemical_Name) == as.character(chemical_subset[[i]]$Test_Substance_Chemical_Name),as.character(chemical_subset[[i]]$Category_Chemical_Name), as.character(chemical_subset[[i]]$Test_Substance_Chemical_Name)))
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

  # Remove the occurences of newline character from res1 data
  for (i in 1:length(res1)){
    res1[[i]] <- lapply(res1[[i]], gsub, pattern ="\\n", replacement = "")
    res1[[i]] <- data.frame(res1[[i]], stringsAsFactors = F)
  }

  # Incorporate new_column names and keys to search categories
  res$hpvis_search_categories$new_column_name <- gsub(" ","_", res$hpvis_search_categories$name)
  chem_colname_table <- data.frame(useme = rep("-", 6), name = rep("-", 6), description = rep("-", 6), new_column_name = as.vector(chemical_categories_key_table$chemical_key_value), stringsAsFactors = F)
  res$hpvis_search_categories <- rbind(res$hpvis_search_categories, chem_colname_table)
  res$hpvis_search_categories$new_column_name_key <- gsub("[a-z\\:\\(\\)\\-\\'\\.]+", "", res$hpvis_search_categories$new_column_name)
  res$hpvis_search_categories$new_column_name_key <- gsub("_$|--", "",res$hpvis_search_categories$new_column_name_key)
  res$hpvis_search_categories$new_column_name_key <- gsub("__", "_",res$hpvis_search_categories$new_column_name_key)

  # Build hpvis source names key table and include hpvis_source_key to res1
  hpvis_source_keys_table <- data.frame(source_name = res_names[-19], source_name_key = gsub("[a-z ]+","",res_names[-19]), stringsAsFactors = F)
  hpvis_source_keys_table$source_name_key[c(10,11)] <- c("GIVT","GIVV")
  hpvis_source_keys_table$new_source_names <- names(res1)

  for (i in 1:length(res1)){
    if (names(res1)[i] %in% hpvis_source_keys_table$new_source_names[i]){
      res1[[i]]$hpvis_source_key <- rep(hpvis_source_keys_table$source_name_key[i], nrow(res1[[i]]))
    }
  }

  # Fix row displacement and remove empty columns from res1 dataframes
  res1[[2]][176,c(96:133)] <- gsub("",NA,res1[[2]][176,c(96:133)])
  res1[[10]][,c(37:38)] <- gsub("",NA,res1[[10]][,c(37:38)])
  res1[[11]][,c(43:44)] <- gsub("",NA,res1[[11]][,c(43:44)])
  res1[[18]][,c(50:51)] <- gsub("",NA,res1[[18]][,c(50:51)])

  for (i in 1:length(res1)){
    res1[[i]][,grep("(^X$)|(^X\\d+$)", colnames(res1[[i]]))] <- res1[[i]][,grep("(^X$)|(^X\\d+$)", colnames(res1[[i]]))][!sapply(res1[[i]][,grep("(^X$)|(^X\\d+$)", colnames(res1[[i]]))], function(x) all(is.na(x)| x == ""))]
  }

  # Remove the ocuurences of line ending character from res1 data
  for (i in 1:length(res1)){
    res1[[i]] <- lapply(res1[[i]], gsub, pattern ="\\r", replacement = "")
    res1[[i]] <- data.frame(res1[[i]], stringsAsFactors = F)
  }

  # Fix multiple occurrences of Exposure duration and exposure duration units by taking the largest duration value
  # and its corresponding unit and also create an index name column to represent the source column for duration values
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

  # Assign appropriate data types
  for (i in 1:length(res1)) {
    res1[[i]] <- lapply(res1[[i]], function(x) utils::type.convert(as.character(x), as.is = T))
    res1[[i]] <- data.frame(res1[[i]], stringsAsFactors = F)
  }

  # Find data frames with seperate fields representing individual toxval type variables, then combine them to form
  # fields of toxval categories(numeric, units, qualifier etc)
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

  # Combine concentration result type and concentration percentage to form toxval type
  res3 <- lapply(res3, function(x) { x$new_toxval_type <- paste(x$Concentration_Result_Type, x$Concentration_Percentage, sep = ""); x })

  # Assign toxval variables for dataframes in ecotox, which are currently represented as concentration variables, and incorporate new toxval variables which
  # are absent from these data frames.
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

  # Build duration and duration units from combined exposure duration and toxval exposure duration.
  # hpvis_expo_duration2 has the indexes representing the source field name of the duration value and its corresponding unit
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

  # Subset res1 by excluding ecotox and then join the updated ecotox to the list of dataframes
  res2 <- res1[9:20]
  new_res <- c(res3, res2)

  # Columns with toxval categories
  conc_cols <- c("Concentration_Units","Concentration_Value","Concentration_Result_Type","Concentration_Percentage","Concentration_Value_Description","Concentration_Upper_Value","Basis_for_Concentration")

  # Dataframes having these toxval columns in mammalian
  mammalian <- grep("hpvis_mammalian", names(new_res), value = T )
  res3 <- new_res[mammalian][lapply(new_res[mammalian], function(x)  any(names(x) %in% conc_cols)) == T]

  # Combine concentration percentage and concentration  result type to form toxval type in mammalian dataframes having fields represented in conc_cols.
  # for dataframes which lack these conc_cols fields assign them
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

  # Incorporate updated mammalian data frames to new res and call the combined list of dataframes as res_new
  res_new <- c(new_res[1:11], res2, new_res[19:20])

  # Rename columns which represent the same field but are named differently in different dataframes
  res_new <- lapply(res_new, function(x) stats::setNames(x, gsub(x = names(x), pattern = "Species_or_in_Vitro_System", replacement = "Species")))
  res_new <- lapply(res_new, function(x) stats::setNames(x, gsub(x = names(x), pattern = "Gender", replacement = "Sex")))
  res_new <- lapply(res_new, function(x) stats::setNames(x, gsub(x = names(x), pattern = "Mammalian_Strain", replacement = "Strain")))
  res_new <- lapply(res_new, function(x) stats::setNames(x, gsub(x = names(x), pattern = "Type_of_Study", replacement = "study_type")))

  # Rename exposure period and its unit in dataframes having single entry of both
  expo_dur_cols3 <- lapply(res_new, function(x) {
    grep("^Exposure_Period$|Duration|^Exposure_Period_Units$", names(x), ignore.case = T)
  })
  subset_expo_dur3 <-""
  for (i in 1:length(res_new)){
    subset_expo_dur3[i]<- lapply(res_new[i], "[", expo_dur_cols3[[i]])
  }
  names(subset_expo_dur3) <- names(res_new)
  subset_expo_dur3 <- subset_expo_dur3[sapply(subset_expo_dur3, ncol) == 2]
  res_new[names(res_new) %in% names(subset_expo_dur3)] <- lapply(res_new[names(res_new) %in% names(subset_expo_dur3)], function(x) stats::setNames(x, gsub(x = names(x), pattern = "^Exposure_Period$", replacement = "duration")))
  res_new[names(res_new) %in% names(subset_expo_dur3)] <- lapply(res_new[names(res_new) %in% names(subset_expo_dur3)], function(x) stats::setNames(x, gsub(x = names(x), pattern = "^Exposure_Period_Units$", replacement = "duration_units")))

  # Replace dot characters to underscores in all data frame column names
  res_new <- lapply(res_new, function(x) stats::setNames(x, gsub(x = names(x), pattern = "\\.", replacement = "_")))

  # Combine species and other species to form new_species with elements separated by comma, do the same for strain and Route of Administration
  cols <- c("Species","Other_Species","Strain","Other_Strain","Route_of_Administration","Other_Route_of_Administration")
  for ( i in 1:length(res_new)){
    res_new[[i]][dplyr::setdiff(cols,names(res_new[[i]]))] <- ""
  }

  for ( i in 1:length(res_new)){
    res_new[[i]]$new_species <- paste(res_new[[i]]$Species,res_new[[i]]$Other_Species, sep = ",")
    res_new[[i]]$new_species <- gsub("\\,$|^\\,", "", res_new[[i]]$new_species)
    res_new[[i]]$new_Strain <- paste(res_new[[i]]$Strain,res_new[[i]]$Other_Strain, sep = ",")
    res_new[[i]]$new_Strain <- gsub("\\,$|^\\,", "", res_new[[i]]$new_Strain)
    res_new[[i]]$new_Route_of_Administration <- paste(res_new[[i]]$Route_of_Administration,res_new[[i]]$Other_Route_of_Administration, sep = ",")
    res_new[[i]]$new_Route_of_Administration <- gsub("\\,$|^\\,", "", res_new[[i]]$new_Route_of_Administration)
  }

  # Change column names for Category, Sponsored and Test_Substance cas and name columns to include the corresponding keys along with the name. eg: test_substance_cas_number_t_s_cas_n
  for ( i in 1:length(res_new)){
    old_cas_names <- chemical_categories_key_table$chemical_key_value
    new_cas_names <- paste(chemical_categories_key_table$chemical_key_value,  "_", chemical_categories_key_table$chemical_keys, sep = "")
    names(res_new[[i]])[match(old_cas_names, names(res_new[[i]]))] = new_cas_names
  }

  # Fix multiple underscores and trailing underscores in column names
  res_new <- lapply(res_new, function(x) stats::setNames(x, gsub(x = names(x), pattern = "_+", replacement = "_")))
  res_new <- lapply(res_new, function(x) stats::setNames(x, gsub(x = names(x), pattern = "_$", replacement = "")))

  # Order column names alphabetically
  res_new <- lapply(res_new, function(x) x[,order(names(x))])

  # Assign appropriate data types
  for (i in 1:length(res_new)) {
    res_new[[i]] <- lapply(res_new[[i]], function(x) utils::type.convert(as.character(x), as.is = T))
    res_new[[i]] <- data.frame(res_new[[i]], stringsAsFactors = F)
    res_new[[i]][sapply(res_new[[i]], function(x) all(is.na(x) == T))] <- ""
  }

  # Required columns contain the major columns represented in the existing toxval source
  reqd_cols <- c("casrn","cas_key","name","name_key","toxval_type","toxval_numeric","toxval_numeric_qualifier","toxval_units","toxval_Basis_for_Concentration","toxval_Upper_Range","new_species","sex",
                 "new_Strain","new_Route_of_Administration","Type_of_Exposure","duration","duration_units","duration_index_name","Year_Study_Performed","Program_Flag","Consortium_Name",
                 "study_type","GLP","Reliability","Study_Reference","hpvis_source_key","Population")

  # Get the common columns in all res_new dataframes
  common_cols <- Reduce(intersect, lapply(res_new, colnames))
  mutual_cols <- common_cols[which(common_cols %in% reqd_cols)]

  # Get column names which are present in common cols but not in required cols , combine them to form the new required cols
  non_mutual_cols <- dplyr::setdiff(common_cols, mutual_cols)
  non_mutual_cols <- dplyr::setdiff(non_mutual_cols, cols)
  reqd_cols <- c(reqd_cols, non_mutual_cols)

  # Create new_res by subsetting res_new with the columns present in required cols
  new_res <- lapply(res_new, function(x) subset(x, select = dplyr::intersect(reqd_cols, colnames(x))))

  # In case any required col not being present in a data frame add that column and assign it as empty
  for ( i in 1:length(new_res)){
    new_res[[i]][dplyr::setdiff(reqd_cols,names(new_res[[i]]))] <- ""
  }

  # Combine all dataframes to build new_combined_hpvis_table
  # Add files.list name connection to each dataframe
  new_res = lapply(names(new_res), function(f){
    new_res[[f]] %>%
      dplyr::mutate(raw_input_file = basename(files.list[[f]]))
  })
  hpvis_all_data <- do.call("rbind",new_res)
  row.names(hpvis_all_data) <- NULL
  hpvis_all_data <- unique(hpvis_all_data)
  hpvis_all_data["hpvis_id"] <- c(1:length(hpvis_all_data[,1]))

  # reorder hpvis_id to first column
  hpvis_all_data <- hpvis_all_data[c("hpvis_id",names(hpvis_all_data)[!names(hpvis_all_data) %in% c("hpvis_id")])]
  hpvis_all_data <- lapply(hpvis_all_data, function(x) utils::type.convert(as.character(x), as.is = T))
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

  nlist_prev = c(
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
  nlist_new = c(
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
  rename_list = setNames(nlist_prev, nlist_new)

  res = res %>%
    # Handle multiple studY_type fields
    dplyr::rename(study_type_source = study_type) %>%
    # Rename columns
    dplyr::rename(!!rename_list)

  res = res[!is.na(res$toxval_numeric),]
  res = res[!is.na(res$casrn),]
  res = res[!is.na(res$name),]

  # Fix repeat dose study type
  x = res[res$study_type=="repeat-dose",]
  y = res[res$study_type!="repeat-dose",]
  x[is.element(x$study_duration_units,"Years"),"study_type"] = "chronic"
  x[is.element(x$study_duration_units,"Hours"),"study_type"] = "acute"
  x[is.element(x$study_duration_units,"Minutes"),"study_type"] = "acute"
  x = x[is.element(x$study_type,"repeat-dose"),]
  x1 = x[is.element(x$study_duration_units,"Weeks"),]
  x2 = x[is.element(x$study_duration_units,"Days"),]
  x3 = x[is.element(x$study_duration_units,"Months"),]
  x4 = x[is.element(x$study_duration_units,""),]
  x5 = x[is.element(x$study_duration_units,"Other"),]

  x1a = x1[!is.na(x1$study_duration_value),]
  x1b = x1[is.na(x1$study_duration_value),]
  x1a$study_type = "chronic"
  x1a[x1a$study_duration_value<14,"study_type"] = "subchronic"
  x1a[x1a$study_duration_value<4,"study_type"] = "subacute"

  x2a = x2[!is.na(x2$study_duration_value),]
  x2b = x2[is.na(x2$study_duration_value),]
  x2a$study_type = "chronic"
  x2a[x2a$study_duration_value<100,"study_type"] = "subchronic"
  x2a[x2a$study_duration_value<28,"study_type"] = "subacute"

  x3a = x3[!is.na(x3$study_duration_value),]
  x3b = x3[is.na(x3$study_duration_value),]
  x3a$study_type = "chronic"
  x3a[x3a$study_duration_value<14,"study_type"] = "subchronic"
  x3a[x3a$study_duration_value<4,"study_type"] = "subacute"

  res0 = rbind(x1a,x1b,x2a,x2b,x3a,x3b,x4,x5,y)

  # Standardize the names
  names(res0) <- names(res0) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  res = res0 %>%
    # Filter out null species and non-"Measured" sponsored_chemical_result_type
    dplyr::filter(!(species %in% c(as.character(NA), NULL, "")),
                  sponsored_chemical_result_type %in% c("Measured")) %>%

    dplyr::mutate(
      # Fix unicode symbols across DF
      dplyr::across(dplyr::where(is.character), fix.replace.unicode),

      # Use test_substance for name/casrn
      name_source = name,
      casrn_source = casrn,
      name = test_substance_chemical_name_t_s_c_n,
      casrn = test_substance_cas_number_t_s_cas_n,

      # Set blank character values as NA
      dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., "")),

      # Add assignments from previous load script
      long_ref = study_reference,
      quality = reliability,
      source_url = "https://chemview.epa.gov/chemview/",
      subsource_url = source_url,

      # Perform cleaning operations
      species = species %>%
        tolower() %>%
        stringr::str_replace_all("other,|no data,", "") %>%
        stringr::str_replace_all(",multiple \\- see method remarks|,?\\s*see remarks", "") %>%
        stringr::str_replace(":$", "") %>%
        stringr::str_replace("fresh wate?\\b", "fresh_water)") %>%
        stringr::str_replace("zebra.fish", "zebrafish") %>%
        stringr::str_squish(),
      # Perform cleaning operations
      strain = strain %>%
        stringr::str_replace_all("Other,", "") %>%
        stringr::str_replace(":$", "") %>%
        dplyr::na_if("No Data") %>%
        stringr::str_squish(),

      toxval_numeric_qualifier = toxval_numeric_qualifier %>%
        stringr::str_replace("circa", "~") %>%
        dplyr::na_if("between"),

      toxval_units = toxval_units %>%
        dplyr::na_if("Other") %>%
        dplyr::na_if("No Data"),

      study_duration_value = dplyr::case_when(
        study_duration_units %in% c("Other", "No Data", as.character(NA), "") ~ NA,
        study_duration_value < 0 ~ NA,
        TRUE ~ study_duration_value
      ),
      study_duration_units = dplyr::case_when(
        is.na(study_duration_value) ~ as.character(NA),
        TRUE ~ study_duration_units
      ) %>% tolower() %>% stringr::str_squish(),

      # Extract sex, lifestage, and generation from population field
      sex = stringr::str_extract(population, "Female|Male") %>%
        c() %>%
        tolower(),
      lifestage = stringr::str_extract(population, "Maternal|Paternal|Parental|Fetal|Offspring") %>%
        c() %>%
        tolower(),
      generation = stringr::str_extract(population, "F[0-2]") %>%
        c() %>%
        toupper(),

      # Set invalid exposure_route entries as NA
      exposure_route = exposure_route %>%
        dplyr::na_if("Other") %>%
        dplyr::na_if("No Data") %>%
        dplyr::na_if("Unknown"),

      # Add description of external source ID
      external_source_id_desc = "HPVIS ID",

      # Extract subsource from raw_input_file
      subsource = raw_input_file %>%
        gsub("[0-9].+", "", .) %>%
        stringr::str_squish(),
    ) %>%
    # Rename ID and key finding fields
    dplyr::rename(external_source_id = hpvis_id,
                  key_finding = key_study_sponsor_indicator) %>%
    # Additional species filtering
    dplyr::filter(!(species %in% c("other", "unknown", "no data", "not specified",
                                   as.character(NA), NULL, ""))) %>%
    # Remove entries without exposure_route
    dplyr::filter(!is.na(exposure_route)) %>%

    # Separate entries with multiple species and select appropriate strain
    dplyr::mutate(
      initial_row = dplyr::row_number(),
      species = gsub("rats\\/mice", "rats and mice", species)
    ) %>%
    # Split species rows
    tidyr::separate_rows(species, sep=",|\\band\\b|\\bor\\b") %>%
    dplyr::group_by(initial_row) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      species = species %>%
        stringr::str_squish() %>%
        tolower(),

      species_plural = dplyr::case_when(
        species %in% c("mouse", "mice") ~ "(?:mice|mouse)",
        species %in% c("rat", "rats") ~ "(?:rat|rats)",
        TRUE ~ stringr::str_c("(?:", species, ")")
      ),

      # Alter strain to handle edge cases during extraction
      strain = dplyr::case_when(
        grepl("mouse|mice", species) & !grepl("rat", strain, ignore.case=TRUE) ~
          gsub("Sprague\\-Dawley; ", "", strain),
        TRUE ~ strain
      ) %>%
        gsub("Sprague\\-Dawley and Fisher B6C3F1", "Sprague-Dawley rat and Fisher B6C3F1 mice", .),

      # Extract strain
      strain = dplyr::case_when(
        # Case where entry contained only one species/strain
        n == 1 ~ strain,

        # Case of "Unknown"
        strain %in% c("Unknown", "unknown") ~ strain,

        # Case of "for both"
        stringr::str_detect(strain, "(.+) for both") ~ stringr::str_extract(strain, "(.+) for both", group=1),

        # Case of Species: Strain; Species: Strain
        stringr::str_detect(
          strain,
          stringr::regex(stringr::str_c(species_plural, ": (.+);?"), ignore_case=TRUE)
        ) ~
          stringr::str_extract(
            strain,
            stringr::regex(stringr::str_c(species_plural, ": (.+);?"), ignore_case=TRUE), group=1
          ),

        # Case where strain occurs at end of list of strains
        stringr::str_detect(
          strain,
          stringr::regex(stringr::str_c("(?:,? and |, )(?!.*,|.*and)(.+) ", species_plural), ignore_case=TRUE)
        ) ~
          stringr::str_extract(
            strain,
            stringr::regex(stringr::str_c("(?:,? and |, )(?!.*,|.*and)(.+) ", species_plural), ignore_case=TRUE), group=1
          ),

        # Case where strain occurs in middle of list of strains
        stringr::str_detect(
          strain,
          stringr::regex(stringr::str_c(", ([^,]+) ", species_plural, "(?:,| and)"), ignore_case=TRUE)
        ) ~
          stringr::str_extract(
            strain,
            stringr::regex(stringr::str_c(", ([^,]+) ", species_plural, "(?:,| and)"), ignore_case=TRUE), group=1
          ),

        # Case where strain occurs at beginning of list of strains
        stringr::str_detect(
          strain,
          stringr::regex(stringr::str_c("^([^,]+) ", species_plural), ignore_case=TRUE)
        ) ~
          stringr::str_extract(
            strain,
            stringr::regex(stringr::str_c("^([^,]+) ", species_plural), ignore_case=TRUE), group=1
          ),

        # Case of strain (species)
        stringr::str_detect(
          strain,
          stringr::regex(stringr::str_c("(\\S+) \\(", species_plural, "\\)"), ignore_case=TRUE)
        ) ~
          stringr::str_extract(
            strain,
            stringr::regex(stringr::str_c("(\\S+) \\(", species_plural, "\\)"), ignore_case=TRUE), group=1
          ),

        TRUE ~ as.character(NA)
      )
    ) %>%
    dplyr::select(-c("initial_row", "n", "species_plural")) %>%

    # Create properly formatted population field
    tidyr::unite("population",
                 sex, strain, species, generation,
                 sep=" ",
                 remove=FALSE,
                 na.rm=TRUE) %>%
    dplyr::mutate(
      population = population %>%
        gsub("\\b([FP][0-2])$", "\\(\\1\\)", .) %>%
        gsub("Other|Unknown|no data|unknown|other|not specified", "", .) %>%
        gsub(paste0("\\b(rat|dog|hamster|rabbit|pig|hen|cat|minnow|soybean|crab|cucumber|",
                    "shell|earthworm|human|mammal|onion|arthropod|mollusc|plant|moorfrog|duck)\\b"),
             "\\1s", .) %>%
        gsub("\\bmouse\\b", "mice", .) %>%
        gsub("\\btomato\\b", "tomatoes", .) %>%
        stringr::str_squish() %>%
        dplyr::na_if(""),

      key_finding = dplyr::case_when(
        key_finding == "Key" ~ "key",
        key_finding == "Not Key" ~ "no",
        # Set NA key_finding to "unspecified"
        is.na(key_finding) ~ "unspecified",
        TRUE ~ key_finding
      ) %>% tolower()
    ) %>%
    # Add range_relationship_id after separating species
    dplyr::mutate(range_relationship_id = dplyr::row_number())

  # Handle entries with upper ranges
  lower_range_res = res %>%
    dplyr::filter(!is.na(toxval_upper_range)) %>%
    dplyr::select(-toxval_upper_range) %>%
    dplyr::mutate(
      toxval_subtype = "Lower Range",
      toxval_numeric_qualifier = ">="
    )
  upper_range_res = res %>%
    dplyr::filter(!is.na(toxval_upper_range)) %>%
    dplyr::mutate(
      toxval_numeric = toxval_upper_range,
      toxval_subtype = "Upper Range",
      toxval_numeric_qualifier = "<="
    ) %>%
    dplyr::select(-toxval_upper_range)
  res = res %>%
    dplyr::filter(is.na(toxval_upper_range)) %>%
    dplyr::select(-toxval_upper_range) %>%
    dplyr::bind_rows(lower_range_res, upper_range_res) %>%
    # Filter out entries without required toxval fields
    dplyr::filter(!is.na(toxval_type), !is.na(toxval_units), !is.na(toxval_numeric), toxval_numeric >= 0)

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"

  # Perform deduping
  res = toxval.source.import.dedup(res)

  # Add version date. Can be converted to a mutate statement as needed
  res$source_version_date <- src_version_date

  # Add experimental_record value
  res$experimental_record = "experimental"
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db=db,
                       source=source,
                       table=source_table,
                       res=res,
                       do.reset=do.reset,
                       do.insert=do.insert,
                       chem.check.halt=chem.check.halt,
                       hashing_cols=toxval.config()$hashing_cols)
}
