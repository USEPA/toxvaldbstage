#--------------------------------------------------------------------------------------
#' @description Processes DAT QC audit information into database
#'
#' @param source_table name of ToxVal source table audit information is associated with
#' @param db the name of the database
#' @param live_df a filepath to the DAT live data to push to the 'source' table
#' @param audit_df a filepath to the DAT audit data to push to source_audit #'
#' @import dplyr DBI magrittr
#'
#' @export
#' @title FUNCTION_TITLE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[dplyr]{rename}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{filter}}
#'  \code{\link[tidyr]{reexports}}
#'  \code{\link[writexl]{write_xlsx}}
#' @rdname DAT.pipe.source.audit
#' @importFrom readxl read_xlsx
#' @importFrom dplyr rename mutate left_join select filter
#' @importFrom tidyr any_of
#' @importFrom writexl write_xlsx
#--------------------------------------------------------------------------------------
DAT.pipe.source.audit <- function(source_table, db, live_df, audit_df) {

  # Pull associated DAT files for an input source table
  DAT_data = list(
    live_dat = live_df %>%
      readxl::read_xlsx() %>%
      # Remove QC fields that will be repopulated in this workflow
      .[ , !(names(.) %in% c("parent_hash", "qc_notes", "qc_flags", "created_by"))] %>%
      # Fill NA character fields with "-"
      dplyr::mutate(dplyr::across(where(is.character), ~tidyr::replace_na(., "-"))),
    audit_dat = audit_df %>%
      readxl::read_xlsx() %>%
      dplyr::rename(src_tbl_name=dataset_name) %>%
      dplyr::mutate(src_tbl_name = gsub("toxval_", "", src_tbl_name)) %>%
      # Remove QC fields that will be repopulated in this workflow
      .[ , !(names(.) %in% c("parent_hash", "qc_notes", "qc_flags", "created_by"))] %>%
      # Fill NA character fields with "-"
      dplyr::mutate(dplyr::across(where(is.character), ~tidyr::replace_na(., "-")))
  )

  # Add back columns removed from QC data
  source_data = runQuery(paste0("SELECT * FROM ", source_table, " WHERE source_hash in ('",
                                paste0(DAT_data$live_dat$src_record_id, collapse="', '"),"')"), db=db) %>%
    # Only select columns (and source_hash) not already present in DAT QC data
    .[, names(.)[!names(.) %in% names(DAT_data$live_dat)]] %>%
    # Remove QC fields that will be repopulated in this workflow
    .[ , !(names(.) %in% c("parent_hash", "qc_notes", "qc_flags", "created_by"))]

  # Combine to add back missing columns (columns not QC'd)
  DAT_data$live_dat = DAT_data$live_dat %>%
    dplyr::left_join(source_data, by = c("src_record_id" = "source_hash"))
  DAT_data$audit_dat = DAT_data$audit_dat %>%
    dplyr::left_join(source_data, by = c("src_record_id" = "source_hash"))

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
  stop("Ensure hashing ID list matches this source's specific hashing column needs from import!")

  # Removing version which is part of source_hash generation
  # hash_id_list = hash_id_list[!hash_id_list %in% c("version")]

  # Prepare live values
  live = DAT_data$live_dat %>%
    prep.DAT.conversion(., hash_id_list=hash_id_list)

  # Prepare audit values
  audit = DAT_data$audit_dat %>%
    # Select and rename DAT audit columns for toxval_source, calculate new source_hash
    prep.DAT.conversion(., hash_id_list=hash_id_list) %>%
    # Transform record columns into JSON
    dplyr::mutate(record = convert.fields.to.json(dplyr::select(., -tidyr::any_of(hash_id_list)))) %>%
    # Select only audit/ID columns and JSON record
    dplyr::select(tidyr::any_of(hash_id_list), record)

  # Correct version numbers based on parent hash version in toxval_source
  v_list = runQuery(paste0("SELECT source_hash, version as parent_version FROM ",
                           source_table), db)
  # Based on toxval_source parent_hash, increment up
  audit = audit %>%
    dplyr::left_join(v_list, by=c("parent_hash" = "source_hash")) %>%
    dplyr::mutate(audit_version = version + parent_version) %>%
    dplyr::select(-parent_version, -version)
  # Based on toxval_source parent_hash, increment up
  live = live %>%
    dplyr::left_join(v_list, by=c("parent_hash" = "source_hash")) %>%
    dplyr::mutate(audit_version = version + parent_version) %>%
    dplyr::select(-parent_version, -version)

  if(any(is.na(live$audit_version)) | any(is.na(audit$audit_version))){
    message("Error matching parent_hash back to database to correct version for ",length(which(is.na(live$audit_version)))," records...")

    browser()
    live = live %>%
      dplyr::filter(!is.na(audit_version))
    audit = audit %>%
      dplyr::filter(!is.na(audit_version))
  }

  # Determine qc_status
  stop("Check with already present DAT status and failture_reason to ensure correct status set for live...")
  live$qc_status = "pass"
  # Audit always set to fail since it's been changed
  audit$qc_status = "fail"
  # Unchanged records = PASS outright
  # live$qc_status[live$source_hash %in% v_list$source_hash] = "pass"
  # If record changed and has a failure reason
  live$qc_status[!live$failure_reason %in% c(NA, "-")] = "fail"

  # qc_status spot check
  # live %>% select(data_record_annotation, failure_reason, qc_status)

  # Prep audit table push
  # Get audit table names
  audit_fields <- runQuery("SELECT * FROM source_audit LIMIT 1", db=db) %>% names()
  audit = audit %>%
    dplyr::rename(fk_source_hash = source_hash,
                  version = audit_version,
                  qc_notes = data_record_annotation,
                  qc_flags = failure_reason,
                  created_by = create_by,
                  src_version_date = source_version_date) %>%
    select(any_of(c(audit_fields, "src_version_date")))

  # Rename columns as needed
  # Select columns from source_table
  src_tbl_fields <- runQuery(paste0("SELECT * FROM ", source_table, " LIMIT 1"), db=db) %>% names()
  live = live %>%
    #filter(!source_hash %in% v_list$source_hash) %>%
    dplyr::rename(version = audit_version,
                  qc_notes = data_record_annotation,
                  qc_flags = failure_reason,
                  created_by = create_by,
                  # Rename preemptively
                  old_parent_chemical_id = parent_chemical_id,
                  parent_chemical_id = chemical_id) %>%
    select(any_of(src_tbl_fields), old_parent_chemical_id)

  # Hash spotcheck - compare should be TRUE for unchanged records
  # live %>% select(parent_hash, source_hash, version) %>% mutate(compare = parent_hash == source_hash)
  # audit %>% select(parent_hash, fk_source_hash, version) %>% mutate(compare = parent_hash == fk_source_hash)

  # General Check
  # live %>% select(source_hash, parent_hash, qc_status, qc_flags, qc_notes, version) %>% mutate(compare = parent_hash == source_hash) %>% View()

  stop("Rehashing of chemicals to parent untested...")
  # Get source name from chemical index
  source_name <- runQuery(paste0("SELECT source FROM chemical_source_index WHERE source_table = '", source_table,"'"),
                          db=db)[[1]]
  # Re-hash chemical information
  live_chems = source_chemical.process(db = db,
                                       res = live %>%
                                         dplyr::select(casrn, name) %>%
                                         dplyr::distinct(),
                                       source = source_name,
                                       table = source_table,
                                       chem.check.halt = FALSE,
                                       casrn.col = "casrn",
                                       name.col = "name")

  # Map back chemical information to all records
  live <- live %>%
    left_join(live_chems %>%
                dplyr::select(-chemical_index),
              by = c("name", "casrn"))

  # Check combinations between chemical_id, parent_chemical_id, and old_parent_chemical_id
  # live %>% select(chemical_id, parent_chemical_id, old_parent_chemical_id) %>% distinct()
  live$parent_chemical_id[live$parent_chemical_id == live$chemical_id] = "-"
  # Replace "-" parent_chemical_id with old_parent_chemical_id since no change was made in chemical_id
  live$parent_chemical_id[live$parent_chemical_id == "-"] = live$old_parent_chemical_id[live$parent_chemical_id == "-"]
  # Ensure fields match source table fields
  live <- select(live, any_of(src_tbl_fields))

  stop("Compare hash and redo hash as needed for live and rejoin to audit")
  View(live %>%
         select(source_hash, parent_hash) %>%
         mutate(compare = source_hash == parent_hash) %>%
         distinct())
  View(audit %>%
         select(fk_source_hash, parent_hash) %>%
         mutate(compare = fk_source_hash == parent_hash) %>%
         distinct())
  audit$fk_source_hash[!audit$fk_source_hash %in% live$source_hash]
  live$source_hash[!live$source_hash %in% audit$fk_source_hash]

  audit = audit %>%
    # dplyr::rename(fk_source_hash_old = fk_source_hash) %>%
    dplyr::left_join(live %>%
                       dplyr::select(parent_hash, fk_source_hash=source_hash),
                     by="parent_hash")

  # Check for needed schema changes
  src_fields_mismatch = set_field_SQL_type(src_f = live) %>%
    str_split("\n") %>%
    unlist() %>%
    data.frame(src_field = .) %>%
    dplyr::mutate(src_field = src_field %>%
                    gsub("`", "",.) %>%
                    sub('COLLATE.*', '', .) %>%
                    sub('DEFAULT.*', '', .) %>%
                    sub(", ", ",", .) %>%
                    stringr::str_squish() %>%
                    tolower()) %>%
    tidyr::separate(src_field, into = c("field_name", "field_type"),
                    sep=" ",
                    fill="left",
                    extra="merge") %>%
    dplyr::filter(!field_name %in% toxval.config()$non_hash_cols) %>%
    dplyr::left_join(
      runQuery(paste0("SELECT COLUMN_NAME as field_name, COLUMN_TYPE as db_field_type ",
                      "FROM INFORMATION_SCHEMA.COLUMNS WHERE table_name = '", source_table, "' ",
                      "AND TABLE_SCHEMA = '", db,"'"), db),
      by = "field_name"
    ) %>%
    dplyr::mutate(mismatch = field_type != db_field_type) %>%
    dplyr::filter(mismatch == TRUE)

  if(nrow(src_fields_mismatch)){
    stop("Need to address field type mismatch to avoid truncation of fields...")
  }

  # Check if triggers are active
  trigger_check = runQuery(paste0("SHOW TRIGGERS"), db) %>%
    dplyr::filter(grepl("audit_bu|update_bu", Trigger),
                  Table %in% c(source_table))

  # Expecting 2 triggers for audit to be active
  if(nrow(trigger_check) != 2){
    stop("Ensure audit triggers are active!")
  }

  # Check missing audit fields
  if(length(audit_fields[!audit_fields %in% names(audit)])){
    cat(paste0("- ", audit_fields[!audit_fields %in% names(audit)], "\n", collapse="\n"))
    stop("Audit potentially missing needed fields")
  }

  # Export intermediate before push
  writexl::write_xlsx(list(live=live, audit=audit),
                      paste0(toxval.config()$datapath,"QC Pushed/", source_table,"_QC_push_",Sys.Date(),".xlsx"))

  # Push live and audit table changes
  # runInsertTable(mat=audit, table="source_audit", db=db, get.id = FALSE)
  # Query to join and make updates
  updateQuery = paste0("UPDATE ", source_table," a INNER JOIN z_updated_df b ",
                       "ON (a.source_hash = b.parent_hash) SET ",
                       paste0("a.", names(live),  " = b.", names(live), collapse = ", ")
  )
  # Update first (source_hash = parent_hash) to audit what's already in the table
  # runUpdate(table=source_table, updateQuery=updateQuery, updated_df=live, db)

################################################################################
################################################################################
  updateQuery = paste0("UPDATE ", source_table," a INNER JOIN z_updated_df b ",
                       "ON (a.source_hash = b.source_hash) SET ",
                       paste0("a.", names(live)[!names(live) %in% c("source_id")],  " = b.",
                              names(live)[!names(live) %in% c("source_id")], collapse = ", ")
  )
  # Update again (source_hash = source_hash) WITHOUT TRIGGERS to get true version in source_table
  # runUpdate(table=source_table, updateQuery=updateQuery, updated_df=live, db, trigger_check=FALSE)

  # Check expected version counts
  # live %>% dplyr::group_by(version) %>% dplyr::summarise(n=n())
}
