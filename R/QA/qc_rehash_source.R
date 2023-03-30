#--------------------------------------------------------------------------------------
#' Updates the source_hash for a selected source table
#'
#' @param source.table Name of the source table in toxval_source
#' @param source.name Name of the source in human readable format
#' @param db the name of the database
#' @export
#--------------------------------------------------------------------------------------
qc_rehash_source <- function(source.table, source.name, db){

  # Pull source table data
  db_dat = runQuery(paste0("SELECT * FROM ", source.table), db) %>%
    select(-parent_hash)

  # Re-hash
  non_hash_cols = c("chemical_id", "parent_chemical_id", "source_id","clowder_id","document_name","source_hash","qc_status",
                    "parent_hash","create_time","modify_time","created_by", "qc_flags", "qc_notes", "version",
                    "raw_input_file")

  db_dat = qc_rehash(in_dat=db_dat,
                     non_hash_cols=non_hash_cols,
                     source.table=source.table,
                     source.name=source.name,
                     db=db,
                     # Do not make changes to the data, just rehash
                     generic.fixes = FALSE)

  # Compare re-hash/filter to changes
  db_dat = db_dat %>%
    filter(source_hash != parent_hash) %>%
    rowwise() %>%
    # Append QC note of the re-hash
    mutate(qc_notes = ifelse(is.na(qc_notes),
                             "rehashed",
                             ifelse(qc_notes == "-",
                                    "rehashed",
                                    paste(qc_notes, "rehashed", sep="; ")))) %>%
    ungroup() %>%
    select(source_hash, parent_hash, qc_notes)

  # Push update to database (update source_hash, parent_hash, and qc_notes)QQQ
  updateQuery = paste0("UPDATE ", source.table," a INNER JOIN z_updated_df b ",
                       "ON (a.source_hash = b.parent_hash) SET ",
                       paste0("a.", names(db_dat),  " = b.", names(db_dat), collapse = ", ")
  )
  # runUpdate(table=source.table, updateQuery=updateQuery, updated_df=db_dat, db=db)

}


