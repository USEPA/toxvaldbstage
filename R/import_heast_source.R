library("openxlsx")
#--------------------------------------------------------------------------------------
#' Load heast Source into dev_toxval_source_v2. 
#' @param toxval.db The version of toxval into which the source is loaded.
#' @param infile The input file ./heast/heast_files/EPA_HEAST_Table1_ORNL for loading.xlsx


#--------------------------------------------------------------------------------------

import_heast_source <- function(toxval.db,infile) {
  printCurrentFunction(toxval.db)

  #####################################################################
  cat("Build original_heast_table \n")
  #####################################################################
  res <- read.xlsx(infile,1,colNames = T)
  #runInsertTable(res,"original_heast_table",toxval.db,do.halt=T,verbose=F)
  
  res$source_id <- c(1:dim(res)[1])
  res$source_hash <- "-"
  res$clowder_id <- "-"
  res$document_name <- "-"
  res <- res[c("source_id","source_hash","clowder_id",names(res)[which(!names(res) %in% c("source_id","source_hash","clowder_id","document_name"))],"document_name")]
  
  #print(names(res))
  #runInsertTable(res,"original_heast",toxval.db,do.halt=T,verbose=F)
  
  query <- paste0("select * from original_heast")
  res <- runQuery(query,toxval.db)
  
  # #####################################################################
  # cat("Build heast_chemical_information table from res\n")
  # #####################################################################
  # chemical_information <- res[,c("casrn","name")]
  # chemical_information <- unique(chemical_information[,1:2])
  # chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  # chemical_information <- chemical_information[c('chemical_id','name','casrn')]
  # runInsertTable(chemical_information,"heast_chemical_information",toxval.db,do.halt=T,verbose=F)
  
 
  #####################################################################
  cat("Make revised version of original_heast_table called res1 with a new_study_duration_qualifier field\n")
  #####################################################################
  res1 <- res
  
  names(res1)[which(names(res1) == "study_duration_value")] <- "study_duration_value_original"
  names(res1)[which(names(res1) == "study_duration_units")] <- "study_duration_units_original"
  
  #res1$study_duration_value <- res1$study_duration_value_original
  res1$study_duration_value <- gsub("^\\s+|\\s+$", "", res1$study_duration_value)
  
  non_numeric_dur_val <-  grep('[^0-9.]', res1$study_duration_value, value = T)
  non_numeric_dur_pos <- grep('[^0-9.]', res1$study_duration_value)
  #study_dur_qual <- gsub("[0-9.]","", non_numeric_dur_val)
  study_dur_qual <- non_numeric_dur_val
  study_dur_qual[grep("^[^[:alnum:]]",study_dur_qual)] <- gsub("(^[^[:alnum:]])(\\s*.*)","\\1", study_dur_qual[grep("^[^[:alnum:]]",study_dur_qual)])
  study_dur_qual[grep("^[^[:alnum:]]",study_dur_qual, invert = T)] <- "-"
  qualifier_table <- data.frame(non_numeric_dur_val, study_dur_qual, stringsAsFactors = F)
  res1$study_duration_qualifier <- "-"

  for (i in 1:length(qualifier_table$non_numeric_dur_val)){
    for (k in 1:nrow(res1)){
      res1$study_duration_qualifier[k][res1$study_duration_value[k] %in% qualifier_table$non_numeric_dur_val[[i]]] <- qualifier_table$study_dur_qual[i]
    }
  }

  #####################################################################
  cat("Create heast_study_duration_value_replacement_table to record replacements in study duration values\n")
  #####################################################################
  res1$study_duration_value <- gsub(">|Up to|generations", "", res1$study_duration_value)
  res1$study_duration_value <- gsub("^\\s+|\\s+$", "", res1$study_duration_value)
  replace_study_dur_all <- grep("^[a-zA-Z]+", res1$study_duration_value, value = T)
  replace_study_dur <- unique(grep("^[a-zA-Z]+", res1$study_duration_value, value = T))
  replace_study_dur_val <- c(1,2,2,2,1,28)
  replace_study_dur_unit <- c("days","years","years","generation","days","days")
  dur_val_replacement <- data.frame(replace_study_dur, replace_study_dur_val, replace_study_dur_unit, stringsAsFactors = F)
  # runInsertTable(dur_val_replacement,"heast_study_duration_value_replacement_table",toxval.db,do.halt=T,verbose=F)
  # 
  #####################################################################
  cat("Build new_heast_table and new_heast_rfd_rfc_table\n")
  #####################################################################

  for (i in 1:length(dur_val_replacement$replace_study_dur)){
    for (k in 1:nrow(res1)){
      res1$study_duration_value[k][res1$study_duration_value_original[k] %in% dur_val_replacement$replace_study_dur[[i]]] <- dur_val_replacement$replace_study_dur_val[i]
    }
  }

  
  for (i in 1:length(dur_val_replacement$replace_study_dur)){
    for (k in 1:nrow(res1)){
      res1$study_duration_units[k][res1$study_duration_value_original[k] %in% dur_val_replacement$replace_study_dur[[i]]] <- dur_val_replacement$replace_study_dur_unit[i]
    }
  }

  mult_val <- grep("[^0-9.]", res1$study_duration_value, value = T)
  mult_val_new <- "-"
  
  for(i in 1:length(mult_val)){
    mult_val_new[i] <- max(as.numeric(as.character(unlist(strsplit(gsub("[a-zA-Z\\+\\/\\-]+"," ",mult_val[i]),"\\s+"))))) 
  }
  
  multiple_values <- data.frame(mult_val,mult_val_new, stringsAsFactors = F )
  
  for (i in 1:length(multiple_values$mult_val)){
    for (k in 1:nrow(res1)){
      res1$study_duration_value[k][res1$study_duration_value[k] %in% multiple_values$mult_val[[i]]] <- multiple_values$mult_val_new[i]
    }
  }

  
  res1$study_duration_value[which(is.na(res1$study_duration_value))] <- ""
  res1$study_duration_units[which(is.na(res1$study_duration_units))] <- "-"
  res1$study_duration_value <- as.numeric(res1$study_duration_value)
  res1$strain <- "-"
  res1$sex <- "-"
  res1$exposure_route_original <- res1$exposure_route
  res1$exposure_method_original <- res1$exposure_route
  exposure_route_delimiter <-grep("\\:", res1$exposure_route)
  res1$exposure_method <- res1$exposure_route
  
  res1[exposure_route_delimiter,"exposure_method"] <- gsub("(.*\\:\\s*)(.*)","\\2",res1[exposure_route_delimiter,"exposure_method"])
  res1[exposure_route_delimiter,"exposure_route"] <- gsub("(.*)(\\s*\\:.*)","\\1",res1[exposure_route_delimiter,"exposure_route"])
  
  
  res1$year <- ""
  res1$year <- as.integer(res1$year)
  #res1 <- res1[!is.na(res1$toxval_numeric),]
  res1["heast_id"] <- c(1:dim(res1)[1])
  res1 <- res1[c("heast_id","source_hash",names(res1[5:10]),names(res1[35:36]),names(res1[c(11,39,37,38,12,13,14,32,33,34,15,16,29,30,40)]), names(res1[17:28]))]
  
  #print(names(res1))  
  ## without source_hash, id, clowder_id, document_name
  # res1 <- res1[c("heast_id","source_hash",names(res1[2:7]),names(res1[31:32]),names(res1[c(8,35,33,34,9,10,11,28,29,30,12,13,26,27,36)]), names(res1[14:25]))]
  #print(View(res1))
  
  #old
  # res1 <- res1[c("heast_id",names(res1[1:11]), names(res1[28:30]), names(res1[12:27]), names(res1[31:34]))]
  # runInsertTable(res1,"whole_heast_table",toxval.db,do.halt=T,verbose=F)
  #
  # query <- "select ht.*, ci.chemical_id from whole_heast_table ht inner join heast_chemical_information ci on ci.name = ht.name and ci.casrn =ht.casrn"
  # res2 <- runQuery(query,toxval.db)
  # res2_var <- names(res2) %in% c("name","casrn")
  # res2 <- res2[!res2_var]
  #
  
  # # without source_hash, id, clowder_id, document_name
  # res3 <- res1[,-c(25:36)]
  # res3 <- res3[!is.na(res3$toxval_numeric),]
  # print(View(res3))
  # # runInsertTable(res3,"new_heast",toxval.db,do.halt=T,verbose=F)
   
  res3 <- res1[,-c(26:37)]
  res3 <- res3[!is.na(res3$toxval_numeric),]
  #print(View(res3))
  runInsertTable(res3,"new_heast",toxval.db,do.halt=T,verbose=F)
  
  #old
  # # res3 <- res2[,-c(16:27)]
  # # res3 <- res3[!is.na(res3$toxval_numeric),]
  # # runInsertTable(res3,"new_heast_table",toxval.db,do.halt=T,verbose=F)
  # # 
  
  
  new_res <- res1[,c(1:4,26:37)]
  
  new_res$rfc_type1[!is.na(new_res$rfc_subchronic)] <- "RfC_subchronic"
  new_res$rfd_type2[!is.na(new_res$rfd_subchronic)] <- "RfD_subchronic"
  new_res$rfc_type3[!is.na(new_res$rfc_chronic)] <- "RfC_chronic"
  new_res$rfd_type4[!is.na(new_res$rfd_chronic)] <- "RfD_chronic"
  
  # rfc_type 1 to 4
  new_res <- cbind(new_res, toxval_type <- do.call(pmax, c(new_res[c(17:20)], list(na.rm = T))))
  # all 4 uf fields
  new_res <- cbind(new_res, toxval_uf <- do.call(pmax, c(new_res[c(5,8,11,14)], list(na.rm = T))))
  # all 4 numeric fields
  new_res <- cbind(new_res, toxval_numeric <- do.call(pmax, c(new_res[c(6,9,12,15)], list(na.rm = T))))
  # all 4 unit fields
  new_res <- cbind(new_res, toxval_units <- do.call(pmax, c(new_res[c(7,10,13,16)], list(na.rm = T))))

    
  # # without source_hash, id, clowder_id, document_name
  # new_res <- res1[,c(1:3,25:36)]
  # new_res$rfc_type1[!is.na(new_res$rfc_subchronic)] <- "RfC_subchronic"
  # new_res$rfd_type2[!is.na(new_res$rfd_subchronic)] <- "RfD_subchronic"
  # new_res$rfc_type3[!is.na(new_res$rfc_chronic)] <- "RfC_chronic"
  # new_res$rfd_type4[!is.na(new_res$rfd_chronic)] <- "RfD_chronic"
  # # rfc_type 1 to 4
  # new_res <- cbind(new_res, toxval_type <- do.call(pmax, c(new_res[c(16:19)], list(na.rm = T))))
  # # all 4 uf fields
  # new_res <- cbind(new_res, toxval_uf <- do.call(pmax, c(new_res[c(4,7,10,13)], list(na.rm = T))))
  # # all 4 numeric fields
  # new_res <- cbind(new_res, toxval_numeric <- do.call(pmax, c(new_res[c(5,8,11,14)], list(na.rm = T))))
  # # all 4 unit fields
  # new_res <- cbind(new_res, toxval_units <- do.call(pmax, c(new_res[c(6,9,12,15)], list(na.rm = T))))
  # 
  # # new_res <- res2[,c(1,16:27,34)]
  # # new_res <- cbind(new_res, toxval_type <- do.call(pmax, c(new_res[c(15:18)], list(na.rm = T))))
  # # new_res <- cbind(new_res, toxval_uf <- do.call(pmax, c(new_res[c(2,5,8,11)], list(na.rm = T))))
  # # new_res <- cbind(new_res, toxval_numeric <- do.call(pmax, c(new_res[c(3,6,9,12)], list(na.rm = T))))
  # # new_res <- cbind(new_res, toxval_units <- do.call(pmax, c(new_res[c(4,7,10,13)], list(na.rm = T))))
  # 
  
  
  new_res <- new_res[,c(1:4,21:24)]
  names(new_res) <- c("heast_id","source_hash","name","casrn","toxval_type","toxval_uf","toxval_numeric","toxval_units")
  new_res$toxval_type <- as.character(new_res$toxval_type)
  new_res$toxval_uf <- gsub("N/A","-999",new_res$toxval_uf)
  new_res$toxval_uf <- gsub("-","",new_res$toxval_uf)
  new_res$toxval_uf <- as.numeric(new_res$toxval_uf)
  new_res$toxval_numeric <- as.numeric(new_res$toxval_numeric)
  new_res$toxval_units <- as.character(new_res$toxval_units)
  new_res$toxval_subtype <- gsub(".*\\_","", new_res$toxval_type)
  new_res$toxval_type <- gsub("\\_.*","", new_res$toxval_type)
  new_res <- new_res[!is.na(new_res$toxval_numeric),]
  
  new_res$heast_rfd_rfc_id <- c(1:dim(new_res)[1])
  new_res <- new_res[c('heast_rfd_rfc_id', names(new_res[-10]))]
  
  runInsertTable(new_res,"new_heast_rfd_rfc",toxval.db,do.halt=T,verbose=F)
  
  # 
  #print(View(new_res))
  # 
  # 
  # # new_res <- new_res[,c(1,14,19:22)]
  # # names(new_res) <- c("heast_id","chemical_id","toxval_type","toxval_uf","toxval_numeric","toxval_units")
  # # new_res$toxval_type <- as.character(new_res$toxval_type)
  # # new_res$toxval_uf <- gsub("N/A","-999",new_res$toxval_uf)
  # # new_res$toxval_uf <- gsub("-","",new_res$toxval_uf)
  # # new_res$toxval_uf <- as.numeric(new_res$toxval_uf)
  # # new_res$toxval_numeric <- as.numeric(new_res$toxval_numeric)
  # # new_res$toxval_units <- as.character(new_res$toxval_units)
  # # new_res$toxval_subtype <- gsub(".*\\_","", new_res$toxval_type)
  # # new_res$toxval_type <- gsub("\\_.*","", new_res$toxval_type)
  # # new_res <- new_res[!is.na(new_res$toxval_numeric),]
  # # new_res$heast_rfd_rfc_id <- c(1:dim(new_res)[1])
  # # new_res <- new_res[c('heast_rfd_rfc_id', names(new_res[-8]))]
  # # runInsertTable(new_res,"new_heast_rfd_rfc_table",toxval.db,do.halt=T,verbose=F)
  
}

