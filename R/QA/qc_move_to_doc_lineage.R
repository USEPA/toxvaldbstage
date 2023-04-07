#' qc_move_to_doc_lineage
#' Utility script to move document information from source tables to document lineage tables
#' @param db the name of the database
qc_move_to_doc_lineage <- function(db){
  message("Starting document lineage move...")
  # Check all required document lineage tables are present
  doc_lineage_check <- runQuery(query = paste0("SHOW TABLES FROM ", db),
                                db=db) %>%
    .[[1]] %>%
    .[grepl("documents", .)]

  doc_lineage_tbls <- c("documents", "documents_lineage", "documents_records")

  doc_lineage_check <- doc_lineage_tbls[!doc_lineage_tbls %in% doc_lineage_check]

  if(length(doc_lineage_check)){
    stop("Missing document lineage table: ", paste0(doc_lineage_check, collapse = ", "))
  }

  # PUll list of source tables
  tblList = runQuery(query = paste0("SHOW TABLES FROM ", db),
                     db=db) %>% unlist() %>% unname() %>%
    # Filter to those named "source_*"
    .[grepl("source_", .)] %>%
    # Ignore those like source_audit or source_chemical
    .[!grepl("chemical|_old|_new|source_audit", .)]

  # Pull all source_hash, clowder_id, and document_name information from source tables
  doc_data <- lapply(tblList, function(s_tbl){
    message("...Pulling from ", s_tbl, "...")
    n_check <- runQuery(paste0("SELECT * FROM ", s_tbl, " LIMIT 1"), db=db)

    # Check Clowder ID and document name in source table
    if(!all(c("clowder_id", "document_name") %in% names(n_check))) return(NULL)
    # Pull desired fields (not NA/NULL clowder_id)
    n_check <- runQuery(paste0("SELECT source_hash, clowder_id, document_name FROM ", s_tbl,
                          " WHERE clowder_id is not null and clowder_id != '-'",
                          " and clowder_id not in (select clowder_id FROM documents)"
                          ),
                   db=db)
    # Check if anything returned
    if(!nrow(n_check)) return(NULL)

    # Return with source_table
    n_check %>%
      mutate(source_table = s_tbl) %>%
      return()
  }) %>%
    # Remove NULL entries
    purrr::compact() %>%
    # Combine into dataframe
    dplyr::bind_rows() %>%
    # Handle cases where records stored in ; separated fashion
    tidyr::separate_rows(clowder_id, document_name, sep="; ") %>%
    distinct()

  doc_list <- runQuery("SELECT distinct clowder_id FROM documents", db=db) %>% .[[1]]
  # Prep documents table to push
  mat <- doc_data %>%
    # Only need clowder_id and document_name
    dplyr::select(clowder_id, document_name) %>%
    dplyr::distinct() %>%
    # Filter out already pushed document records
    dplyr::filter(!clowder_id %in% doc_list)

  # Submit to documents table
  if(nrow(mat)){
    runInsertTable(mat = mat,
                   table = "documents",
                   db = db, get.id = FALSE)
  } else {
    message("...no new records to push to documents table")
  }

  # Pull created document ID and match by clowder_id
  doc_list <- runQuery("SELECT id as fk_doc_id, clowder_id FROM documents", db=db)
  # Pull already pushed document_records
  doc_records <- runQuery("SELECT source_hash, fk_doc_id FROM documents_records", db=db) %>%
    left_join(doc_list,
              by="fk_doc_id") %>%
    tidyr::unite(col="documents_records_old", source_hash, clowder_id, sep="_", remove=FALSE)

  mat = doc_data %>%
    left_join(doc_list, by="clowder_id") %>%
    tidyr::unite(col="documents_records_new", source_hash, clowder_id, sep="_", remove=FALSE) %>%
    # Filter out any already matched/pushed to database
    filter(!documents_records_new %in% doc_records$documents_records_old) %>%
    select(-clowder_id, -document_name, -documents_records_new) %>%
    distinct()

  if(nrow(mat)){
    # Submit to documents_records table
    runInsertTable(mat = mat,
                   table = "documents_records",
                   db = db, get.id = FALSE)
  } else {
    message("...no new records to push to documents_records table")
  }
  message("Done")
}
