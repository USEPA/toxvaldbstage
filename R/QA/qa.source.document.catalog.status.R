#--------------------------------------------------------------------------------------
#' Create audit table and add BEFORE UPDATE audit triggers to source_* tables
#'
#' @param db the name of the database
#' @export
#--------------------------------------------------------------------------------------
qa.source.document.catalog.status <- function(db){
  # Get list of source tables to add triggers
  tblList = runQuery(query = paste0("SHOW TABLES FROM ", db),
                     db=db) %>% unlist() %>% unname() %>%
    # Filter to those named "source_*"
    .[grepl("source_", .)] %>%
    # Ignore those like source_audit or source_chemical
    .[!grepl("chemical|source_audit", .)]

  out = lapply(tblList, function(src_tbl){
    message("Pulling source ", match(src_tbl, tblList), " of ", length(tblList))
    tmp = runQuery(paste0("SELECT source_hash, clowder_id FROM ", src_tbl), db)

    data.frame(source = src_tbl,
                       n_records = nrow(tmp),
                       doc_mapped = tmp %>%
                         filter(clowder_id != "-") %>%
                         nrow(),
                       doc_missing = tmp %>%
                         filter(clowder_id == "-") %>%
                         nrow()) %>%
      mutate(perc_missing = ifelse(doc_missing > 0, round((doc_missing / n_records) * 100, 3),
                                   0)) %>%
      return()

  }) %>% dplyr::bind_rows() %>%
    mutate(perc_toxval_source = round((n_records / sum(n_records)) * 100, 3) )

  return(out)
}
