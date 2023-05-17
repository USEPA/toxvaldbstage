#' @title prep_source_hawc_pfas_150_qc_load
#' @description Function to match QC'd file data to source_hash values before QC loading.
#' Special case where all reviewed passed without edits, so just join to get source_hash
#' and push QC'd records.
#' @param in_file Filepath to QC'd data (from Jira ticket TOXVAL-309)
#' @param source_table name of ToxVal source table audit information is associated with
#' @param db The version of toxval_source into which the source is loaded.
#' @return DataFrame of QC'd data with matched source_hash field
prep_source_hawc_pfas_150_qc_load <- function(in_file, source_table, db){

  # Get source name from chemical index
  source_name <- runQuery(paste0("SELECT source FROM chemical_source_index WHERE source_table = '", source_table,"'"),
                          db=db)[[1]]
  # Pull QC'd data (from Jira ticket TOXVAL-297)
  qc_data <- readxl::read_xlsx(in_file)
  # Check source_hash matches in database
  db_source_parent_hashes <- runQuery(paste0("SELECT source_hash FROM ", source_table, " WHERE source_hash in ('",
                          paste0(qc_data$parent_hash, collapse="', '"),"')"), db=db)

  # Prep re-hash chemical_id
  live <- qc_data %>%
    dplyr::filter(parent_hash %in% db_source_parent_hashes$source_hash) %>%
    dplyr::mutate(old_parent_chemical_id = "-") %>%
    dplyr::rename(parent_chemical_id = chemical_id)

  # Get source name from chemical index
  source_name <- runQuery(paste0("SELECT source FROM chemical_source_index WHERE source_table = '", source_table,"'"),
                          db=db)[[1]]
  # Select columns from source_table
  src_tbl_fields <- runQuery(paste0("SELECT * FROM ", source_table, " LIMIT 1"), db=db) %>% names()
  # Re-hash chemical information
  live = source_chemical.process(db = db,
                                 res = live,
                                 source = source_name,
                                 table = source_table,
                                 chem.check.halt = FALSE,
                                 casrn.col = "casrn",
                                 name.col = "name")

  # Check combinations between chemical_id, parent_chemical_id, and old_parent_chemical_id
  # live %>% select(chemical_id, parent_chemical_id, old_parent_chemical_id) %>% distinct()
  live$parent_chemical_id[live$parent_chemical_id == live$chemical_id] = "-"
  # Replace "-" parent_chemical_id with old_parent_chemical_id since no change was made in chemical_id
  live$parent_chemical_id[live$parent_chemical_id == "-"] = live$old_parent_chemical_id[live$parent_chemical_id == "-"]
  # Ensure fields match source table fields
  live <- select(live, any_of(src_tbl_fields))

  # Query to join and make updates
  updateQuery = paste0("UPDATE ",source_table," a INNER JOIN z_updated_df b ",
                       "ON (a.source_hash = b.parent_hash) SET ",
                       paste0("a.", names(live),
                              " = b.", names(live),
                              collapse = ", ")
  )
  # runUpdate(table=source_table, updateQuery=updateQuery, updated_df=live, db=db)
}

# writexl::write_xlsx(live, "Repo/QC Pushed/source_hawc_pfas_150_QC_push_20230427.xlsx")
