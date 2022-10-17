#--------------------------------------------------------------------------------------
#' Processes DAT QC audit information into database
#'
#' @param source name of ToxVal source table audit information is associated with
#' @param db the name of the database
#'
#' @import dplyr DBI magrittr
#'
#' @export
#--------------------------------------------------------------------------------------
DAT.pipe.source.audit <- function(source, db) {

  stop("Draft script, not ready to use")
  # Insert logic to pull associated audit files for an input source table

  DAT_data = list(
    live_dat = "Repo/DAT reports/dataset_detail_QC_2022-10-17_test.xlsx" %>%
      readxl::read_xlsx(),
  audit_dat = "Repo/DAT reports/dataset_detail_audit_QC_2022-10-17_test.xlsx" %>%
    readxl::read_xlsx() %>%
    dplyr::rename(src_tbl_name=dataset_name) %>%
    mutate(src_tbl_name = gsub("toxval_", "", src_tbl_name))
  )

  id_list = c("source_hash", "parent_hash", "version", "data_record_annotation",
              "failure_reason", "src_tbl_name", "qc_status",
              "status_name", "create_by", "create_time", "end_time")
  # Identifiers excluded from source_hash generation
  hash_id_list = append(id_list,
                        c("chemical_id","source_id","clowder_id","document_name",
                          "source_hash","qc_status", "parent_hash","create_time",
                          "modify_time","created_by")) %>%
    unique()
  # Removing version which is part of source_hash generation
  hash_id_list = hash_id_list[!hash_id_list %in% c("version")]

  # Prepare live values
  live = DAT_data$live_dat %>%
    prep.DAT.conversion()

  # Prepare audit values
  audit = DAT_data$audit_dat %>%
    prep.DAT.conversion() %>%
    # dplyr::rename(parent_hash = src_record_id) %>%
    # # Remove extraneous DAT fields
    # select(-uuid, -description, -total_fields_changed, -dataset_description, -DAT_domain_name,
    #        -domain_description, -DAT_source_name, -source_description, -status_description) %>%
    # tidyr::unite("pre_source_hash", any_of(names(.)[!names(.) %in% hash_id_list]),
    #              sep="", remove = FALSE) %>%
    # # Set source_hash
    # mutate(source_hash = purrr::map_chr(pre_source_hash, digest, serialize=FALSE)) %>%
    # select(-pre_source_hash) %>%
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

  # Determine qc_status
  live = live %>%
    mutate(qc_status = "TBD in Code")

  # Prep audit table push
  audit = audit %>%
    dplyr::rename(fk_source_hash = source_hash,
                  version = audit_version,
                  qc_notes = data_record_annotation,
                  qc_flags = failure_reason,
                  created_by = create_by) %>%
    select(-status_name)

  # Filter out unchanged records from live table (unchanged = matching source_hash)
  # Rename columns as needed
  live = live %>%
    filter(!source_hash %in% v_list$source_hash) %>%
    dplyr::rename(version = audit_version,
                  qc_notes = data_record_annotation,
                  qc_flags = failure_reason,
                  created_by = create_by) %>%
    select(-dataset_name, -status_name, -source_name, -create_time)

  # Push live and audit table changes
  # runInsertTable(mat=audit, table="source_audit", db=db, get.id = FALSE)
  # Query to join and make updates
  updateQuery = paste0("UPDATE ", source," a INNER JOIN z_updated_df b ",
                       "ON (a.source_hash = b.parent_hash) SET ",
                       paste0("a.", names(live),  " = b.", names(live), collapse = ", ")
                       )
  # runUpdate(updateQuery=updateQuery, updated_df=live, db)
}

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

prep.DAT.conversion <- function(in_dat){
  in_dat %>%
    dplyr::rename(parent_hash = src_record_id) %>%
    # Remove extraneous DAT fields
    select(-uuid, -description, -total_fields_changed, -dataset_description, -DAT_domain_name,
           -domain_description, -DAT_source_name, -source_description, -status_description) %>%
    tidyr::unite("pre_source_hash", any_of(names(.)[!names(.) %in% hash_id_list]),
                 sep="", remove = FALSE) %>%
    # Set source_hash
    mutate(source_hash = purrr::map_chr(pre_source_hash, digest, serialize=FALSE)) %>%
    select(-pre_source_hash) %>%
    return()
}
