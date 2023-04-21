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
  non_hash_cols <- toxval.config()$non_hash_cols

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


