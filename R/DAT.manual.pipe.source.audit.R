#--------------------------------------------------------------------------------------
#' Processes manually QC'd (sans-DAT) QC audit information into database
#'
#' @param source name of ToxVal source table audit information is associated with
#' @param db the name of the database
#' @param live_df a filepath to the DAT live data to push to the 'source' table
#' @param qc_user The name of the user who completed the QC
#'
#' @import dplyr DBI magrittr
#'
#' @export
#--------------------------------------------------------------------------------------
DAT.manual.pipe.source.audit <- function(source, db, live_df, qc_user = "Evelyn Rowan") {
  # live_df = list("Repo/DAT reports/qc_updated_hawc_pfas_150_20221011_hashed.xlsx",
  #             "Repo/DAT reports/qc_updated_hawc_pfas_150_clowder_20221020_hashed.xlsx")
  # Used to prevent accidental usage - only use manually when certain it should be used.
  return("Should only be used to process special case of manually QC'd data without DAT")
  # Check input to ensure is list or string
  live_df = switch(typeof(live_df),
                   "character" = list(live_df),
                   "list" = live_df)
  if(is.null(live_df)){
    return("Unsupported input live_df - must be a file path or list of filepaths")
  }
  DAT_data = list()
  # Load and combine input data
  DAT_data$live_dat = lapply(live_df, function(f){
    tmp = readxl::read_xlsx(f) %>%
      # Remove QC fields that will be repopulated in this workflow
      .[ , !(names(.) %in% c("parent_hash", "qc_notes", "qc_flags", "created_by"))] %>%
      filter(!qc_status %in% c("done in 20221011", "fail: multiple source documents"),
             !is.na(qc_status))

    names(tmp) <- names(tmp) %>%
      # Replace whitespace and periods with underscore
      gsub("[[:space:]]|[.]", "_", .) %>%
      stringr::str_squish() %>%
      tolower()
    return(tmp)
  }) %>%
    dplyr::bind_rows()
  # Check for duplicates between QC files
  dups = DAT_data$live_dat %>%
    group_by(source_hash) %>%
    dplyr::summarize(n = n()) %>%
    filter(n > 1)
  # Throw error and browse if duplicates found
  if(nrow(dups)){
    message("Duplicate source_hash values found...need to reconcile")
    DAT_data$live_dat %>%
      filter(source_hash %in% unique(dups$source_hash)) %>%
      arrange(source_hash) %>%
      View()
    browser()
  }
  # Rename for less code refactoring
  DAT_data$live_dat = DAT_data$live_dat %>%
    dplyr::rename(src_record_id = source_hash)

  # Add back columns removed from QC data
  source_data = runQuery(paste0("SELECT * FROM ", source, " WHERE source_hash in ('",
                                paste0(DAT_data$live_dat$src_record_id, collapse="', '"),"')"), db=db) %>%
    # Only select columns (and source_hash) not already present in DAT QC data
    .[, names(.)[!names(.) %in% names(DAT_data$live_dat)]] %>%
    # Remove QC fields that will be repopulated in this workflow
    .[ , !(names(.) %in% c("parent_hash", "qc_notes", "qc_flags", "created_by"))]

  # Combine to add back missing columns (columns not QC'd)
  DAT_data$live_dat = DAT_data$live_dat %>%
    left_join(source_data, by = c("src_record_id" = "source_hash"))

  # List of ID columns for audit table (JSON conversion ignore)
  id_list = c("source_hash", "parent_hash", "version", "data_record_annotation",
              "failure_reason", "src_tbl_name", "qc_status",
              "status_name", "create_by", "create_time", "end_time",
              # DAT cols
              "uuid", "description", "data_record_annotation", "total_fields_changed",
              "failure_reason", "create_time", "version", "src_record_id", "dataset_name",
              "dataset_description", "DAT_domain_name", "domain_description",
              "DAT_source_name", "source_description", "status_name", "status_description",
              "create_by", "created_by", "source_name", "qc_changes", "qc_comments")
  # Identifiers excluded from source_hash generation
  hash_id_list = append(id_list,
                        c("chemical_id","source_id","clowder_id","document_name",
                          "source_hash","qc_status", "parent_hash","create_time",
                          "modify_time","created_by", "qc_notes", "qc_flags",
                          "qc_changes", "qc_comments")) %>%
    unique()
  # Removing version which is part of source_hash generation
  # hash_id_list = hash_id_list[!hash_id_list %in% c("version")]

  live = DAT_data$live_dat
  # Fill in additional changes required for hashing that was missed in older versions
  table = source
  desc <- runQuery(paste0("desc ",table),db)
  desc <- desc[is.element(desc[,"Field"],names(live)),]
  for(i in 1:dim(desc)[1]) {
    col <- desc[i,"Field"]
    type <- desc[i,"Type"]
    if(contains(type,"varchar") || contains(type,"text")) {
      live = live %>%
        mutate(!!col := as.character(!!col) %>%
                 enc2native(.) %>%
                 iconv(.,from="latin1",to="UTF-8") %>%
                 iconv(.,from="LATIN1",to="UTF-8") %>%
                 iconv(.,from="LATIN2",to="UTF-8") %>%
                 iconv(.,from="latin-9",to="UTF-8") %>%
                 enc2utf8(.)
                 )

      live[is.na(live[[col]]), col] <- "-"
    }
  }

  # Prepare live values
  live = live %>%
    prep.DAT.conversion(., hash_id_list=hash_id_list, source=source) %>%
    # Special case where all changes are submitted as version 2
    mutate(version = 2,
           created_by = qc_user,
           create_time = Sys.time())

  # Subset audit fields based on non-matching source_hash and parent_hash fields
  audit = live %>%
    filter(source_hash != parent_hash)
  # live %>% select(source_hash, parent_hash) %>% View()
  # Look at changes made/noted
  # audit %>% select(qc_status, qc_changes, qc_comments) %>% distinct %>% View()
  # No change
  # live %>% filter(source_hash == parent_hash) %>% select(qc_status, qc_changes, qc_comments) %>% distinct() %>% View()
  # Prepare audit values
  audit = audit %>%
    # Transform record columns into JSON
    mutate(record = convert.audit.to.json(select(., -any_of(id_list)))) %>%
    # Select only audit/ID columns and JSON record
    select(any_of(id_list), record)


  live = live %>%
    # Process "fail" messages to use qc_flags
    tidyr::separate(col="qc_status", into=c("qc_status", "qc_flags"), sep="; ", fill = "right") %>%
    # Combine qc_change and qc_comments
    tidyr::unite("qc_notes", qc_comments, qc_changes, sep="; ") %>%
    mutate(qc_notes = qc_notes %>%
             gsub("NA; |; NA", "", .))

  audit = audit %>%
    # Process "fail" messages to use qc_flags
    tidyr::separate(col="qc_status", into=c("qc_status", "qc_flags"), sep="; ", fill = "right") %>%
    # Combine qc_change and qc_comments
    tidyr::unite("qc_notes", qc_comments, qc_changes, sep="; ") %>%
    mutate(qc_notes = qc_notes %>%
             gsub("NA; |; NA", "", .))

  # qc_status spot check
  # live %>% select(data_record_annotation, failure_reason, qc_status)

  # Prep audit table push
  audit = audit %>%
    dplyr::rename(fk_source_hash = source_hash) %>%
    .[, !names(.) %in% c("status_name", "source_name")]

  # Rename columns as needed
  live = live %>%
    #filter(!source_hash %in% v_list$source_hash) %>%
    .[, !names(.) %in% c("dataset_name", "status_name", "source_name")]

  # Hash spotcheck - compare should be TRUE for unchanged records
  # live %>% select(parent_hash, source_hash, version) %>% mutate(compare = parent_hash == source_hash)
  # audit %>% select(parent_hash, fk_source_hash, version) %>% mutate(compare = parent_hash == fk_source_hash)

  # Push live and audit table changes
  # runInsertTable(mat=audit, table="source_audit", db=db, get.id = FALSE)
  # Query to join and make updates
  updateQuery = paste0("UPDATE ", source," a INNER JOIN z_updated_df b ",
                       "ON (a.source_hash = b.parent_hash) SET ",
                       paste0("a.", names(live),  " = b.", names(live), collapse = ", ")
  )
  # runUpdate(table=source, updateQuery=updateQuery, updated_df=live, db)
}

# Combine non-ID columns from audit table into JSON format for audit storage
convert.audit.to.json <- function(in_dat){
  lapply(seq_len(nrow(in_dat)), function(row){
    in_dat[row, ] %>%
      summarise(record = jsonlite::toJSON(.)) %>%
      select(record)
  }) %>%
    dplyr::bind_rows() %>%
    unlist() %>%
    unname() %>%
    return()
}

# Select and rename DAT audit columns for toxval_source, calculate new source_hash
prep.DAT.conversion <- function(in_dat, hash_id_list, source){
  in_dat = in_dat %>%
    dplyr::rename(parent_hash = src_record_id) %>%
    # Remove extraneous DAT fields
    .[ , !(names(.) %in% c("uuid", "description", "total_fields_changed", "dataset_description", "DAT_domain_name",
                           "domain_description", "DAT_source_name", "source_description", "status_description"))] %>%
    # Alphabetize the columns to ensure consistent hashing column order
    .[, sort(colnames(.))] %T>% {
      # message(paste0(names(.)[!names(.) %in% hash_id_list], collapse = ", "))
    } %>%
    tidyr::unite("pre_source_hash", any_of(names(.)[!names(.) %in% hash_id_list]),
                 sep="", remove = FALSE) %>%
    # Set source_hash
    mutate(source_hash = purrr::map_chr(pre_source_hash, digest, serialize=FALSE)) %>%
    select(-pre_source_hash)#  %>%
  # in_dat %>% select(parent_hash, pre_source_hash) %>% head(1) %>% View()

  # in_dat$source_hash_2 = "-"
  # for (i in 1:nrow(in_dat)){
  #   row <- in_dat[i,]
  #   row = row[,sort(names(row)[!names(row) %in% hash_id_list])]
  #   in_dat[i,"source_hash_2"] <- digest(paste0(row,collapse=""), serialize = FALSE)
  #   if(i%%1000==0) cat(i," out of ",nrow(in_dat),"\n")
  # }
  return(in_dat)
}
