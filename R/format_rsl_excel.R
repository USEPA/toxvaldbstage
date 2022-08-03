#library("xlsx")
library("openxlsx")
library("dplyr")
library("tidyr")
library("stringr")
#--------------------------------------------------------------------------------------
#' Convert complex rsl Source xlsx files into formatted rsl xlsx source files. 
#' @param x ./rsl/rsl_files/rsl_thq1_nov_2021.xlsx - The downloaded xls summary table for THQ 1 from RSL-Generic Tables (2020)
#' @param Y ./rsl/rsl_files/rsl_thq0.1_nov_2021.xlsx - The downloaded xls summary table for THQ 0.1 from RSL-Generic Tables (2020)
#' @param Z ./rsl/rsl_files/rsl_subchronic_nov_2021.xlsx- downloaded subchronic toxicity table 
#' @param thq_x corresponding thq for x, which is 1 here.
#' @param thq_y corresponding thq for y, which is 0.1 here.
#--------------------------------------------------------------------------------------
format_rsl_excel <- function(x,thq_x,y,thq_y,z){
  
  
  #####################################################################
  cat("Load excel file 1\n")
  #####################################################################
  rsl1 <- read.xlsx(x,1,startRow = 1, colNames = F)
  print(dim(rsl1))
  #####################################################################
  cat("Store and process key info\n")
  #####################################################################
  key_description_1 <- rsl1[1,1]
  key_description_1 <- as.character(key_description_1)
  key_description_1 <- unlist( strsplit( key_description_1 , ";" ) )
  
  #####################################################################
  cat("Remove term 'key:' from first key\n")
  #####################################################################
  key_description_1 <- gsub("...:", "",key_description_1)
  print(key_description_1)
  #####################################################################
  cat("save key description as data frame\n")
  #####################################################################
  key_description_1x <- as.matrix(key_description_1)
  key_description_1xx <- as.data.frame(key_description_1)
  rm(key_description_1xx)
  
  #####################################################################
  cat("Header processing from excel file\n")
  #####################################################################
  rsl1_import <- read.xlsx(x,1,startRow = 3, colNames = T)
  header_excel_1 <- names(rsl1_import)
  
  #####################################################################
  cat("Find duplicate column names\n")
  #####################################################################
  dups <- which(duplicated(header_excel_1) | duplicated(header_excel_1, fromLast = TRUE))
  names_of_dups <- header_excel_1[dups]
  header_to_modify <- header_excel_1[dups]
  header_to_add <- header_excel_1[dups-1]
  new_header <- paste(header_to_modify,'_', header_to_add)
  
  #####################################################################
  cat("Give new column names to column that have duplicate names\n")
  #####################################################################
  names(rsl1_import)[dups]<-c(new_header)
  
  #####################################################################
  cat("Add new column to mention THQ level for given data\n")
  #####################################################################
  rsl1_import["THQ"] <- rep(thq_x,nrow(rsl1_import))
  rsl_excel_1 <- rsl1_import
  rm (rsl1)
  rm (rsl1_import)
  
  #####################################################################
  cat("Load data from second excel file\n")
  #####################################################################
  rsl2 <- read.xlsx(y,1,startRow = 1, colNames = F)
  key_description_2 <- rsl2[1,1]
  key_description_2 <- as.character(key_description_2)
  key_description_2 <- unlist( strsplit( key_description_2 , ";" ) )
  print(key_description_2)
  #####################################################################
  cat("Remove term 'key:' from first key\n")
  #####################################################################
  key_description_2 <- gsub("...:", "",key_description_2)
  key_description_2x <- as.matrix(key_description_2)
  key_description_2xx <- as.data.frame(key_description_2)
  
  #####################################################################
  # Header processing from excel file
  #####################################################################
  
  
  rsl2_import <- read.xlsx(y,1,startRow = 3, colNames = T)
  header_excel_2 <- names(rsl2_import)
  
  #####################################################################
  # Check if all the keys from excel 1 matches exactly with excel 2
  #####################################################################
  
  if (length(header_excel_1)==length(header_excel_2)){
    for (o in 1:length(header_excel_1)){
      if (header_excel_1[o]==header_excel_2[o]){
        
        cat(sprintf("  The header '%s' for COLUMN-%s are the SAME in both the excel files \r\n",header_excel_1[o],o))
      }
      else{cat("  column headers do not match in excel 1 and 2 \r\n")
      }
    }
  }
  
  #####################################################################
  # Find duplicate column names in excel 2
  #####################################################################
  header_to_modify_2 <- header_excel_2[dups]
  
  #####################################################################
  # paste column names to table
  #####################################################################
  names(rsl2_import)[dups]<-c(new_header)
  
  #####################################################################
  # Add new column to mention THQ level for given data
  #####################################################################
  rsl2_import["THQ"] <- rep(thq_y,nrow(rsl2_import))
  rsl_excel_2 <- rsl2_import
  rm (rsl2)
  rm (rsl2_import)
  
  #####################################################################
  # Merge both excel files
  #####################################################################
  Data_rsl <- rbind(rsl_excel_1, rsl_excel_2)
  rm(rsl_excel_1, rsl_excel_2)
  
  
  #####################################################################
  # * and ** are disregarded  for time being  -> * (where: n SL < 100X c SL) and ** ( where n SL < 10X c SL), if * and ** considered comment out below line of code
  #####################################################################
  
  for (i in dups){
    Data_rsl[,i] <- gsub("[\\*-]", "", Data_rsl[,i])
  }
  
  #####################################################################
  # if considering to keep * and ** in the future, use the following code to represent them as f and j
  #####################################################################
  
  # for (i in dups[5:10]){
  #   Data_rsl[,i] <- gsub("(\\*\\*)", "j", Data_rsl[,i])
  #   Data_rsl[,i] <- gsub("(\\*)", "f", Data_rsl[,i])
  # 
  # }
  
  
  #####################################################################
  # split multiple key values using comma
  #####################################################################
  
  for (i in dups){
    for (j in 1:dim(Data_rsl)[1]){
      Data_rsl[j,i] <- paste (unlist (strsplit (Data_rsl[j,i], "")), collapse=",")
    }
  }
  
  
  #####################################################################
  # check all keys represented in the excel files
  #####################################################################
  rsl_keys <- Data_rsl[,dups]
  rsl_keys <- as.matrix(rsl_keys)
  colnames(rsl_keys) <- NULL
  rsl_keys <- unique(rsl_keys)
  rsl_keys <- strsplit(rsl_keys, split = ",")
  rsl_keys <- unlist(rsl_keys)
  rsl_keys <- unique(rsl_keys)
  rsl_keys <- gsub("NA", "", rsl_keys)
  rsl_keys <- rsl_keys[rsl_keys != ""]
  
  
  cat("\n All the keys that are present in given excel files :", rsl_keys)
  
  
  #####################################################################
  # fcode to fix repeated keys
  #####################################################################
  alphabets <- paste(toupper(letters))
  
  rsl_repeat_key_cap <- which(grepl("[[:upper:]]", rsl_keys))
  rsl_repeat_key_small <- which(grepl("[[:lower:]]", rsl_keys))
  
  index_of_capital_repeat <-which(rsl_keys[rsl_repeat_key_cap] %in% toupper(rsl_keys[rsl_repeat_key_small]))
  rsl_repeat_key <- rsl_keys[index_of_capital_repeat]
  cat("\n The following keys are repeated : ", rsl_repeat_key)
  number_of_keys_among26letter <- length(alphabets)-(length(rsl_keys) - length(rsl_repeat_key))
  cat(sprintf("\n There are total %s alphabets are available to replace repeated keys. \n\n", number_of_keys_among26letter))
  
  available_keys_to_replace <- setdiff(alphabets,toupper(rsl_keys))
  index_available_keys_to_replace <- which(alphabets %in% available_keys_to_replace )
  
  index_of_capitalRSLkeys_in_alphabet <- vector(mode="numeric", length=1)
  
  rsl_keys_repeated_upper <- rsl_keys[rsl_repeat_key_cap]
  for (g in 1:length(rsl_keys_repeated_upper)){
    index_of_capitalRSLkeys_in_alphabet[g] <- which(alphabets == rsl_keys_repeated_upper[g])
  }
  
  char_to_replace_in_key <- vector(mode="character", length=1)
  index_of_capital_repeat_in_26 <- which(alphabets %in% rsl_repeat_key)
  rsl_keys_capital_form <- rsl_keys[rsl_repeat_key_cap]
  
  
  y1 = as.numeric(index_available_keys_to_replace)
  
  zz  = factor(rsl_repeat_key, levels = LETTERS)
  
  z1 = as.numeric(zz)
  
  
  
  func <-  function(x) {
    xx = y1 - z1[x] 
    xx = which(xx>0)[1]
    r = available_keys_to_replace[xx]
    available_keys_to_replace <<- available_keys_to_replace[-xx]
    y1 <<- y1[-xx]
    r
  }
  char_to_replace_in_key <- sapply(seq_along(rsl_repeat_key),func)
  char_to_replace_in_key 
  if (any(is.na(char_to_replace_in_key)) | any(duplicated(char_to_replace_in_key))){
    cat("\n",paste0("keys not available to replace for ", paste0(rsl_repeat_key[which(is.na(char_to_replace_in_key))], collapse = ",")) , "\n")
    char_to_replace_in_key <- char_to_replace_in_key[!(is.na(char_to_replace_in_key)|duplicated(char_to_replace_in_key))]
  }
  
  
  #####################################################################
  # fix repeated keys in data file (made a copy)
  #####################################################################
  
  
  Data_rsl2_backup <- Data_rsl
  
  for (u in dups){
    for (v in 1:dim(Data_rsl2_backup)[1]){
      for(k in 1:length(char_to_replace_in_key)){
        Data_rsl2_backup[v,u] <- gsub(rsl_repeat_key[k], char_to_replace_in_key[k], Data_rsl2_backup[v,u])
      }
    }
  }
  
  #####################################################################
  # Capitalize all the keys in data file
  #####################################################################
  
  
  for (e in dups){
    for (f in 1:dim(Data_rsl2_backup)[1]){
      Data_rsl2_backup[f,e] <- toupper(Data_rsl2_backup[f,e])
    }
  }
  
  

  #####################################################################
  # Remove the new line character in front of c = cancer from key descriptions of both files
  #####################################################################
  
  key_description_1x <- gsub("\\n", "", key_description_1x)
  key_description_2x <- gsub("\\n", "", key_description_2x)
  
  
  #####################################################################
  # Check if all the key_descriptions from excel 1 matches exactly with excel 2
  #####################################################################
  
  if (length(key_description_1x)==length(key_description_2x)){
    for (p in 1:length(key_description_1x)){
      if (key_description_1x[p]==key_description_2x[p]){
        cat(sprintf(" The key description of key '%s' for ROW-%s are the SAME in both the excel files \r\n",key_description_1x[p],p))
      }
      else{cat("Key descriptions do not match in excel 1 and 2 ")
      }
    }
  }
  
  #####################################################################
  # Separate key description using '='
  #####################################################################
 
  key_description_1x1 <- unlist( strsplit( key_description_1x , "=" ) )
  
 
  #####################################################################
  # Create empty table and extract values of key from key description vector
  #####################################################################
  even_indexes<-seq(2,length(key_description_1x1),2)
  odd_indexes<-seq(1,length(key_description_1x1),2)
  key_descr_table <- matrix(key_description_1x1, nrow = length(even_indexes), ncol = 2)
  key_descr_table[1:(length(key_description_1x1)/2),1] <- key_description_1x1[odd_indexes]
  key_descr_table[1:(length(key_description_1x1)/2),2] <- key_description_1x1[even_indexes]
  
  #####################################################################
  # remove leading and trailing spaces from keys in key description table
  #####################################################################
  
  key_descr_table[,1] <- gsub("^\\s+|\\s+$", "", key_descr_table[,1])
  
  #####################################################################
  # if * and ** considered replace it with j and f in key desc table
  #####################################################################
  
  # key_descr_table[,1] <- gsub("(\\*\\*)", "j", key_descr_table[,1])
  # key_descr_table[,1] <- gsub("(\\*)", "f", key_descr_table[,1])
  
  
  #####################################################################
  # comment out the following line of code from key if * and ** are considered
  #####################################################################
  
  key_descr_table[,1] <- gsub("[\\*-]", "", key_descr_table[,1])

  
  #####################################################################
  # Remove key where key is lengthy(SSL values are based on DAF)
  #####################################################################
  
  key_descr_table <- subset(key_descr_table,nchar(key_descr_table[,1])<2)
  
  #####################################################################
  # Replace repeated keys in key description table 
  #####################################################################
  # Make empty vector
  index_key_descr_table <- vector(mode = 'character', length = 1)
  
  #match these sorted keys to the keys present in whole data data
  index_key_descr_table <- which(key_descr_table[,1] %in% rsl_keys)
  
  key_descr_table <- key_descr_table[index_key_descr_table,]
  
  # Replace repeated keys
  
  for (u1 in 1){
    for (v1 in 1:dim(key_descr_table)[1]){
      for(k1 in 1:length(char_to_replace_in_key)){
        key_descr_table[v1,u1] <- gsub(rsl_repeat_key[k1], char_to_replace_in_key[k1], key_descr_table[v1,u1])
      }
    }
  }
  
  #####################################################################
  # capitalize all keys in key desc table 
  #####################################################################
  
  for (e1 in 1){
    for (f1 in 1:dim(key_descr_table)[1]){
      key_descr_table[f1,e1] <- toupper(key_descr_table[f1,e1])
    }
  }
  
  #####################################################################
  # Final key description table
  #####################################################################
  key_descr_table <- as.data.frame(key_descr_table)
  write.xlsx(key_descr_table, "./rsl/rsl_files/key_description_nov_21.xlsx")
  #####################################################################
  # Final combined data
  #####################################################################
  
  final_rsl_file <- Data_rsl2_backup
  
  
  #####################################################################
  # read subchronic toxicity data
  #####################################################################
  
  subchr_rsl <- read.xlsx(z)
  subchr_data <- subchr_rsl[3:nrow(subchr_rsl),]
  subchr_header <- subchr_rsl[2,]
  names(subchr_data) <- c("name","casrn","RfDo (mg/kg-day)","k.e.y _ RfDo.(mg/kg-day)","SRfDo (mg/kg-day)", "SRfD Reference","RfCi (mg/m3)","RfC Reference","SRfCi (mg/m3)","SRfC Reference")
  
  write.xlsx(subchr_data, './rsl/rsl_files/final_rsl_subchronic_nov21.xlsx')
  
  
  #####################################################################
  # change empty values with just space character to NA in final_rsl_file (copy of final rsl file)
  #####################################################################
  
  final_rsl_file2 <- final_rsl_file
  names.list <- c("SFO.(mg/kg-day)-1","IUR.(ug/m3)-1","RfDo.(mg/kg-day)","RfCi.(mg/m3)","Csat.(mg/kg)","Resident.Soil.(mg/kg)","Industrial.Soil.(mg/kg)","Resident.Air.(ug/m3)","Industrial.Air.(ug/m3)","Tapwater.(ug/L)","MCL.(ug/L)","Risk-based.SSL.(mg/kg)","MCL-based.SSL.(mg/kg)")
  for (i in 1:nrow(final_rsl_file2[names.list])){
    final_rsl_file2[i,names.list] <- gsub("^\\s+$",NA,final_rsl_file2[i,names.list])
  }
  
  
  #####################################################################
  # replace key in final_rsl_table2 with key values
  #####################################################################
  
  for (i in 1:nrow(key_descr_table)){
    for( j in 1:nrow(final_rsl_file2)){
      final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))] <-replace(final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))], final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))] == as.character(key_descr_table[i,"V1"]), str_trim(as.character(key_descr_table[i,"V2"])))
    }
  }
  
  for (i in 1:nrow(key_descr_table)){
    for( j in 1:nrow(final_rsl_file2)){
      final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))] <-replace(final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))], final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))] == "N,M", "noncancer,ceiling limit exceeded")
      final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))] <-replace(final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))], final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))] == "N,S", "noncancer,Csat exceeded.")
      final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))] <-replace(final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))], final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))] == "C,R", "cancer,RBA applied")
      final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))] <-replace(final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))], final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))] == "N,M,S", "noncancer,ceiling limit exceeded,Csat exceeded.")
      
    }
  }
  
  #####################################################################
  # only if * and ** are considered use the below code
  #####################################################################
  # for (i in 1:nrow(key_descr_table)){
  #   for( j in 1:nrow(final_rsl_file2)){
  #     final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))] <-replace(final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))], final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))] == "C,J", "cancer ( where n SL < 10X c SL)")
  #     final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))] <-replace(final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))], final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))] == "C,F", "cancer (where: n SL < 100X c SL)")
  #     final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))] <-replace(final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))], final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))] == "C,J,R", "cancer ( where n SL < 10X c SL),RBA applied")
  #     final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))] <-replace(final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))], final_rsl_file2[j,grep("^k.*", names(final_rsl_file2))] == "C,F,R", "cancer ( where: n SL < 100X c SL),RBA applied")
  #   }
  # }
  
  write.xlsx(final_rsl_file2, './rsl/rsl_files/final_rsl_thq_combined_nov21.xlsx')
  
  
}
