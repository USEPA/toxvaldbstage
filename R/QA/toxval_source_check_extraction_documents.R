#-------------------------------------------------------------------------------------
#' @title toxval_source_check_extraction_documents
#' @description Check which sources do not have "extraction document" associations
#' wit their records (by source hash)
#' @param db The version of toxval source database to use.
#' @param write_output Whether to write output to Excel (FALSE by default)
#' @return List of sources without extraction document associations. Export XLSX file
#' is also produced.
#' @import RMySQL dplyr readxl magrittr
#--------------------------------------------------------------------------------------
toxval_source_check_extraction_documents <- function(db, write_output=FALSE){

  # Query to check document type counts for each source
  doc_query = paste0("SELECT DISTINCT a.source_table, b.document_type, count(*) AS n ",
                     "FROM documents_records a LEFT JOIN documents b on a.fk_doc_id = b.id ",
                     "WHERE a.source_table IN ",
                     "(SELECT source_table FROM chemical_source_index WHERE source_status='active') ",
                     "GROUP BY a.source_table, b.document_type")

  # Get list of all active tables with extraction document associations
  active_extraction = runQuery(doc_query, db) %>%
    dplyr::filter(document_type == "extraction") %>%
    dplyr::pull(var = "source_table") %>%
    paste0("'", ., "'")

  # Query to check for active sources without extraction document associations
  no_extraction_query = paste0("SELECT source_table FROM chemical_source_index ",
                               "WHERE source_table NOT IN (",
                               toString(active_extraction),
                               ") AND source_status='active'")

  # Get list of all active tables without extraction document associations
  active_no_extraction = runQuery(no_extraction_query, db)

  # Write sources without extraction document associations to Excel if needed, return values
  if (write_output) {writexl::write_xlsx(active_no_extraction, "no_extraction_associations.xlsx")}
  return(active_no_extraction)
}
