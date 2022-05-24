#-------------------------------------------------------------------------------------
#' Map the icf master sheet hash to record source table hash
#' @param toxval.db The version of toxval in which the data is altered.
#' @return The database will be altered
#' @export
#--------------------------------------------------------------------------------------
map.hash.record_source <- function(toxval.db, source.db){
  printCurrentFunction(toxval.db)
  
  #####################################################################
  cat("Fix record_source_hash in the record source table based on values
  from CCTE_Deliverable_RefinfoSheet_Master.xlsx obtained from ICF\n")
  #####################################################################

  cat("get record_source data \n")
  query <- "select * from record_source"
  mat <- runQuery(query,toxval.db)

  print(dim(mat))
  print(names(mat))


  file <- paste0(toxval.config()$datapath,"dictionary/CCTE_Deliverable_RefinfoSheet_Master_Aug_2021.xlsx")
  temp <- read.xlsx(file)
  # get the unique hash keys and long_ref from icf dictionary
  dict <- unique(temp[,c("Record.Source.Hash","Long_Ref")])
  #print(View(dict))
  all_records <- mat
  #print(names(all_records))
  # get unique long_ref from record source table
  res <- data.frame(unique(all_records[,"long_ref"]),stringsAsFactors = F)
  names(res) <- "long_ref"
  # find matching long refs in both dataframes
  icf_refs_in_res <-res[which(res$long_ref %in% dict$Long_Ref),"long_ref"]
  # some record_source table long_ref value had some trailing special characters(eg:comma) compared to icf dict long ref, so matching with all words
  test_list <- str_extract_all(dict[,2],boundary("word"))
  test_list_clean <- ""
  for (i in (1:length(test_list))){
    test_list_clean[i] <-  paste((test_list)[[i]], collapse = " ")
  }

  test_list_clean <- as.list(test_list_clean)
  test_list_clean <- data.frame(matrix(unlist(test_list_clean), nrow=length(test_list_clean), byrow=T),stringsAsFactors = F)
  names(test_list_clean) <- "updated_icf_ref"

  icf_dict <- data.frame(dict[,1],dict[,2],test_list_clean$updated_icf_ref, stringsAsFactors = F)
  names(icf_dict) <- c("icf_record_source_hash","original_long_ref","updated_long_ref")
  # new dictionary with original values and updated long ref values
  file <- paste0(toxval.config()$datapath,"dictionary/icf_record_source_hash_dict_",Sys.Date(),".xlsx")
  write.xlsx(icf_dict, file)
  #write.xlsx(icf_dict, "./ToxValDB/Repo/dictionary/icf_record_source_hash_dict.xlsx")

  # get all words values from record source long ref
  #paste(str_extract_all(res$long_ref,boundary("word"))[[1]], collapse = " ")
  data_list <- str_extract_all(res$long_ref,boundary("word"))
  data_list_clean <- ""
  for (i in (1:length(data_list))){
    data_list_clean[i] <-  paste((data_list)[[i]], collapse = " ")
  }

  data_list_clean <- as.list(data_list_clean)
  data_list_clean <- data.frame(matrix(unlist(data_list_clean), nrow=length(data_list_clean), byrow=T),stringsAsFactors = F)
  names(data_list_clean) <- "updated_data_ref"
  #print(View(data_list_clean))
  data_dict <- data.frame(res$long_ref,data_list_clean$updated_data_ref, stringsAsFactors = F)
  data_dict[is.na(data_dict$res.long_ref)|(data_dict$res.long_ref == "-"),"data_list_clean.updated_data_ref"] <- "-"
  #print(View(data_dict))

  all_records[which(is.na(all_records$long_ref)),"long_ref"] <- "-"
  all_records$updated_long_ref <- all_records$long_ref

  x <- unique(all_records$updated_long_ref)
  x <- x[!is.na(x)]

  x <- x[!is.element(x,data_dict[,1])]
  cat("   missing values in dictionary:",length(x),"\n")

  if(length(x)>0) browser()

  for(i in 1:nrow(data_dict)) {
    valold <- data_dict[i,1]
    valnew <- data_dict[i,2]
    all_records[is.element(all_records$updated_long_ref,valold),"updated_long_ref"] <- valnew
  }

  x <- unique(all_records$updated_long_ref)
  x <- x[!is.na(x)]

  x <- x[!is.element(x,icf_dict[,3])]
  cat("   missing values in dictionary:",length(x),"\n")

  #if(length(x)>0) browser()

  for(i in 1:nrow(icf_dict)) {
    valold <- icf_dict[i,3]
    valnew <- icf_dict[i,2]
    all_records[is.element(all_records$updated_long_ref,valold),"updated_long_ref"] <- valnew
  }
  #print(names(all_records))
  #print(dim(all_records))
  #print(length(all_records$long_ref))
  #print(length(all_records$updated_long_ref))
  names(all_records)[names(all_records) == "long_ref"] <- "original_long_ref"
  names(all_records)[names(all_records) == "updated_long_ref"] <- "long_ref"

  #names(all_records)[names(all_records) == c("long_ref","updated_long_ref")] <- c("original_long_ref","long_ref")
  long_ref_dict <- data.frame(all_records$original_long_ref,all_records$long_ref, stringsAsFactors = F)
  names(long_ref_dict) <- c("original_long_ref","long_ref")
  long_ref_dict <- unique(long_ref_dict[,c("original_long_ref","long_ref")])

  all_records <- all_records[,-which(names(all_records) %in% c("original_long_ref"))]

  all_records$record_source_hash_new <- all_records$record_source_hash

  x <- unique(all_records$long_ref)
  x <- x[!is.na(x)]

  x <- x[!is.element(x,icf_dict[,2])]
  cat("   missing values in dictionary:",length(x),"\n")

  #if(length(x)>0) browser()

  for(i in 1:nrow(icf_dict)) {
    valold <- icf_dict[i,2]
    valnew <- icf_dict[i,1]
    all_records[is.element(all_records$long_ref,valold),"record_source_hash_new"] <- valnew
  }

  all_records$record_source_hash_update_notes <- all_records$record_source_hash %in% all_records$record_source_hash_new

  all_records[which(all_records$record_source_hash_update_notes == FALSE),"record_source_hash_update_notes"] <- "Mapped from ICF"
  all_records[which(all_records$record_source_hash_update_notes == TRUE),"record_source_hash_update_notes"] <- "new hash key"

  record_source_hash_dict <- data.frame(all_records$record_source_hash,all_records$record_source_hash_new,all_records$record_source_hash_update_notes, stringsAsFactors = F )

  file <- paste0(toxval.config()$datapath,"dictionary/record_source_hash_map_notes_",Sys.Date(),".xlsx")
  write.xlsx(record_source_hash_dict, file)


  #####################################################################
  cat("record_source_table names with record_source_hash_new, original and notes\n")
  #####################################################################

  print(names(all_records))
  print(head(all_records,4))

  all_records <- all_records[,-which(names(all_records) %in% c("record_source_hash","record_source_hash_update_notes"))]

  #####################################################################
  cat("record_source_table names with record_source_hash_new and without original and notes\n")
  #####################################################################

  print(names(all_records))

  names(all_records)[names(all_records) == c("record_source_hash_new")] <- c("record_source_hash")

  #####################################################################
  cat("record_source_table names with record_source_hash_new renamed as record_source_hash\n")
  #####################################################################

  print(names(all_records))

  all_records <- all_records[c(names(all_records)[1:2],names(all_records)[24],names(all_records)[3:9],names(all_records)[23],names(all_records)[10:22])]

  #####################################################################
  cat("record_source_table names in correct order\n")
  #####################################################################

  print(names(all_records))

  cat("delete from record_source\n")
  query <- "delete from record_source"
  runQuery(query,toxval.db)


  cat("incorporate new record_source table with updated record_source_hash and long_ref\n")
  runInsertTable(all_records, "record_source", toxval.db, verbose)
  
  
  
}
