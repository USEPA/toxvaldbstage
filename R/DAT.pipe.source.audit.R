#--------------------------------------------------------------------------------------
#' Processes DAT QC audit information into database
#'
#' @param source name of ToxVal source table audit information is associated with
#' @param db the name of the database
#' @param live_df a filepath to the DAT live data to push to the 'source' table
#' @param audit_df a filepath to the DAT audit data to push to source_audit
#'
#' @import dplyr DBI magrittr
#'
#' @export
#--------------------------------------------------------------------------------------
DAT.pipe.source.audit <- function(source, db, live_df, audit_df) {

  # Pull associated DAT files for an input source table
  DAT_data = list(
    live_dat = live_df %>%
      readxl::read_xlsx() %>%
      # Remove QC fields that will be repopulated in this workflow
      .[ , !(names(.) %in% c("parent_hash", "qc_notes", "qc_flags", "created_by"))],
    audit_dat = audit_df %>%
      readxl::read_xlsx() %>%
      dplyr::rename(src_tbl_name=dataset_name) %>%
      mutate(src_tbl_name = gsub("toxval_", "", src_tbl_name)) %>%
      # Remove QC fields that will be repopulated in this workflow
      .[ , !(names(.) %in% c("parent_hash", "qc_notes", "qc_flags", "created_by"))]
  )

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
  DAT_data$audit_dat = DAT_data$audit_dat %>%
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
              "create_by", "source_name")
  # Identifiers excluded from source_hash generation
  hash_id_list = append(id_list, toxval.config()$non_hash_cols) %>%
    unique()

  # Removing version which is part of source_hash generation
  # hash_id_list = hash_id_list[!hash_id_list %in% c("version")]

  # Prepare live values
  live = DAT_data$live_dat %>%
    prep.DAT.conversion(., hash_id_list=hash_id_list, source=source)

  # Prepare audit values
  audit = DAT_data$audit_dat %>%
    # Select and rename DAT audit columns for toxval_source, calculate new source_hash
    prep.DAT.conversion(., hash_id_list=hash_id_list, source=source) %>%
    # Transform record columns into JSON
    mutate(record = convert.audit.to.json(select(., -any_of(id_list)))) %>%
    # Select only audit/ID columns and JSON record
    select(any_of(id_list), record)

  # Correct version numbers based on parent hash version in toxval_source
  v_list = runQuery(paste0("SELECT source_hash, version as parent_version FROM ",
                           audit$src_tbl_name %>% unique()), db)
  # Based on toxval_source parent_hash, increment up
  audit = audit %>%
    left_join(v_list, by=c("parent_hash" = "source_hash")) %>%
    mutate(audit_version = version + parent_version) %>%
    select(-parent_version, -version)
  # Based on toxval_source parent_hash, increment up
  live = live %>%
    left_join(v_list, by=c("parent_hash" = "source_hash")) %>%
    mutate(audit_version = version + parent_version) %>%
    select(-parent_version, -version)

  if(any(is.na(live$audit_version)) | any(is.na(audit$audit_version))){
    message("Error matching parent_hash back to database to correct version for ",length(which(is.na(live$audit_version)))," records...")

    browser()
    live = live %>%
      filter(!is.na(audit_version))
    audit = audit %>%
      filter(!is.na(audit_version))
  }

  # Determine qc_status - default these to fail
  live$qc_status = "pass"
  # Audit always set to fail since it's been changed
  audit$qc_status = "fail"
  # Unchanged records = PASS outright
  # live$qc_status[live$source_hash %in% v_list$source_hash] = "pass"
  # If record changed and has a failure reason
  live$qc_status[!is.na(live$failure_reason)] = "fail"

  # qc_status spot check
  # live %>% select(data_record_annotation, failure_reason, qc_status)

  # Prep audit table push
  audit = audit %>%
    dplyr::rename(fk_source_hash = source_hash,
                  version = audit_version,
                  qc_notes = data_record_annotation,
                  qc_flags = failure_reason,
                  created_by = create_by) %>%
    .[, !names(.) %in% c("status_name", "source_name")]

  # Rename columns as needed
  live = live %>%
    #filter(!source_hash %in% v_list$source_hash) %>%
    dplyr::rename(version = audit_version,
                  qc_notes = data_record_annotation,
                  qc_flags = failure_reason,
                  created_by = create_by,
                  # Rename preemptively
                  old_parent_chemical_id = parent_chemical_id,
                  parent_chemical_id = chemical_id) %>%
    .[, !names(.) %in% c("dataset_name", "status_name", "source_name")]

  # Hash spotcheck - compare should be TRUE for unchanged records
  # live %>% select(parent_hash, source_hash, version) %>% mutate(compare = parent_hash == source_hash)
  # audit %>% select(parent_hash, fk_source_hash, version) %>% mutate(compare = parent_hash == fk_source_hash)

  # General Check
  # live %>% select(source_hash, parent_hash, qc_status, qc_flags, qc_notes, version) %>% mutate(compare = parent_hash == source_hash) %>% View()

  stop("Rehashing of chemicals to parent untested...")
  # Re-hash chemical information
  live = source_chemical.process(db,live,
                                 source=live$source,
                                 chem.check.halt=FALSE,
                                 casrn.col="casrn",name.col="name")

  # TODO Check combinations between chemical_id, parent_chemical_id, and old_parent_chemical_id
  live$parent_chemical_id[live$parent_chemical_id == live$chemical_id] = "-"
  # Replace "-" parent_chemical_id with old_parent_chemical_id since no change was made in chemical_id
  live$parent_chemical_id[live$parent_chemical_id == "-"] = live$old_parent_chemical_id[live$parent_chemical_id == "-"]

  # Remove column
  live$old_parent_chemical_id = NULL

  # Export intermediate before push
  writexl::write_xlsx(list(live=live, audit=audit),
                      paste0(toxval.config()$datapath,"QC Pushed/", source,"_QC_push_",Sys.Date(),".xlsx"))

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
    select(-uuid, -description, -total_fields_changed, -dataset_description, -DAT_domain_name,
           -domain_description, -DAT_source_name, -source_description, -status_description) %>%
    # Alphabetize the columns to ensure consistent hashing column order
    .[, sort(colnames(.))] %>%
    tidyr::unite("pre_source_hash", any_of(names(.)[!names(.) %in% hash_id_list]),
                 sep="", remove = FALSE) %>%
    # Set source_hash
    mutate(source_hash = purrr::map_chr(pre_source_hash, digest, serialize=FALSE)) %>%
    select(-pre_source_hash)

    return(in_dat)
}
