#-------------------------------------------------------------------------------------
#' Map the icf master sheet hash to record source table hash
#' @param toxval.db The version of toxval in which the data is altered.
#' @return The database will be altered
#--------------------------------------------------------------------------------------
map.hash.record_source.by.source <- function(toxval.db,  source){
  printCurrentFunction(paste(toxval.db, ":",source ))

  #####################################################################
  cat("To Fix record_source_hash in the record source table based on values
  from CCTE_Deliverable_RefinfoSheet_Master.xlsx obtained from ICF\n")
  #####################################################################

  #####################################################################
  cat("First import record_source data \n")
  #####################################################################

  query <- paste0("select * from record_source where source like '",source,"'")
  mat <- runQuery(query,toxval.db)

  print(dim(mat))
  print(names(mat))

  #####################################################################
  cat("Read in icf record_source file \n")
  #####################################################################

  file <- paste0(toxval.config()$datapath,"dictionary/CCTE_Deliverable_RefinfoSheet_Master_Aug_2021.xlsx")
  temp <- read.xlsx(file)

  #####################################################################
  cat("Get the unique hash keys and long_ref from icf dictionary \n")
  #####################################################################

  dict <- unique(temp[,c("Record.Source.Hash","Long_Ref")])

  #####################################################################
  cat("checks, finds and replaces non ascii characters in dataframe with XXX\n")
  #####################################################################
  dict <- fix.non_ascii(dict)
  #print(View(dict))

  #####################################################################
  cat("Create a copy of record_source data \n")
  #####################################################################

  all_records <- mat
  #print(names(all_records))

  #####################################################################
  cat("Get unique long_ref from record source table \n")
  #####################################################################
  res <- data.frame(unique(all_records[,"long_ref"]),stringsAsFactors = F)
  names(res) <- "long_ref"

  #####################################################################
  cat("Find matching long refs in both dataframes \n")
  #####################################################################
  icf_refs_in_res <-res[which(res$long_ref %in% dict$Long_Ref),"long_ref"]


  #####################################################################
  cat("some record_source table long_ref value had some trailing special characters(eg:comma) compared to icf dict long ref, so matching with only words \n")
  #####################################################################
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

  #####################################################################
  cat("new dictionary with original values and updated long ref values \n")
  #####################################################################
  # file <- paste0(toxval.config()$datapath,"dictionary/icf_record_source_hash_dict_",source,"_",Sys.Date(),".xlsx")
  # write.xlsx(icf_dict, file)

  #####################################################################
  cat("get only word values from record source long ref \n")
  #####################################################################
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
  # create a field in all_records to house updated_long_ref
  all_records$updated_long_ref <- all_records$long_ref

  #####################################################################
  cat("find missing values in record source dataframe (all_records) from updated record_source dataframe (data_dict)\n")
  #####################################################################


  x <- unique(all_records$updated_long_ref)
  x <- x[!is.na(x)]

  x <- x[!is.element(x,data_dict[,1])]
  cat("   missing values in dictionary:",length(x),"\n")

  if(length(x)>0) browser()

  #####################################################################
  cat("replace updated_long_ref values in record source dataframe (all_records) from updated record_source dataframe (data_dict)\n")
  #####################################################################


  for(i in 1:nrow(data_dict)) {
    valold <- data_dict[i,1]
    valnew <- data_dict[i,2]
    all_records[is.element(all_records$updated_long_ref,valold),"updated_long_ref"] <- valnew
  }


  names(all_records)[names(all_records) == "long_ref"] <- "original_long_ref"
  names(all_records)[names(all_records) == "updated_long_ref"] <- "long_ref"

  #####################################################################
  cat("create a long_ref dictionary with original and updated long ref from all records \n")
  #####################################################################

  long_ref_dict <- data.frame(all_records$original_long_ref,all_records$long_ref, stringsAsFactors = F)
  names(long_ref_dict) <- c("original_long_ref","long_ref")
  long_ref_dict <- unique(long_ref_dict[,c("original_long_ref","long_ref")])
  # file <- paste0(toxval.config()$datapath,"dictionary/updated_long_refs_in_record_source_",source,"_",Sys.Date(),".xlsx")
  # write.xlsx(long_ref_dict, file)

  #####################################################################
  cat("create a new column to house the updated record source hashes \n")
  #####################################################################

  all_records$record_source_hash_new <- all_records$record_source_hash

  #####################################################################
  cat("find missing updated long refs from all records in updated icf dictionary \n")
  #####################################################################

  x <- unique(all_records$long_ref)
  x <- x[!is.na(x)]

  x <- x[!is.element(x,icf_dict[,3])]
  cat("   missing values in dictionary:",length(x),"\n")

  #####################################################################
  cat("replace record_source_hash in all records based on long ref matching with icf dict \n")
  #####################################################################

  for(i in 1:nrow(icf_dict)) {
    valold <- icf_dict[i,3]
    valnew <- icf_dict[i,1]
    all_records[is.element(all_records$long_ref,valold),"record_source_hash_new"] <- valnew
  }


  #####################################################################
  cat("table showing the updated hash keys in each of the sources \n")
  #####################################################################

  all_records$record_source_hash_update_notes <- all_records$record_source_hash %in% all_records$record_source_hash_new

  all_records[which(all_records$record_source_hash_update_notes == FALSE),"record_source_hash_update_notes"] <- "Mapped from ICF"
  all_records[which(all_records$record_source_hash_update_notes == TRUE),"record_source_hash_update_notes"] <- "new hash key"

  record_source_hash_dict <- data.frame(all_records$record_source_hash,all_records$record_source_hash_new,all_records$record_source_hash_update_notes, stringsAsFactors = F )

  # file <- paste0(toxval.config()$datapath,"dictionary/record_source_hash_map_notes_",source,"_",Sys.Date(),".xlsx")
  # write.xlsx(record_source_hash_dict, file)


  #####################################################################
  cat("record_source_table names excluding original and notes\n")
  #####################################################################


  all_records <- all_records[,-which(names(all_records) %in% c("record_source_hash","record_source_hash_update_notes"))]

  #####################################################################
  cat("record_source_table names with record_source_hash_new renamed as record_source_hash\n")
  #####################################################################

  names(all_records)[names(all_records) == c("record_source_hash_new")] <- c("record_source_hash")

  #####################################################################
  cat("replace the updated long refs with original\n")
  #####################################################################

  all_records$long_ref <- all_records$original_long_ref

  #####################################################################
  cat("remove additional column with original long ref\n")
  #####################################################################

  all_records <- all_records[,-which(names(all_records) %in% c("original_long_ref"))]

  #####################################################################
  cat("record_source_table names in correct order\n")
  #####################################################################

  all_records <- all_records[c(names(all_records)[1:2],names(all_records)[24],names(all_records)[3:9],names(all_records)[23],names(all_records)[10:22])]

  # testing with sbox version of v8 which has no field for qa status
  #all_records <- all_records[c(names(all_records)[1:2],names(all_records)[23],names(all_records)[3:9],names(all_records)[22],names(all_records)[10:21])]

  print(names(all_records))

  #####################################################################
  cat("load updated record_source table for the particular source\n")
  #####################################################################


  cat("delete from record_source for the given source \n")
  query <- paste0("delete from record_source where source like '",source,"'")
  runQuery(query,toxval.db)


  cat("incorporate new record_source table with updated record_source_hash and long_ref for the given source\n")
  runInsertTable(all_records, "record_source", toxval.db, verbose)



}
