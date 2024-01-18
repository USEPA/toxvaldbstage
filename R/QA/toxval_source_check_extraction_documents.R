#-------------------------------------------------------------------------------------
#' @title toxval_source_check_extraction_documents
#' @description Check which sources do not have "extraction document" associations
#' in their records (by source hash) and get list of docs with null document_type
#' @param db The version of toxval source database to use.
#' @param write_output Whether to write output to Excel (FALSE by default)
#' @return List of sources without extraction document associations and document information
#' for docs without document_type. Export XLSX files produced if specified.
#' @import RMySQL dplyr readxl magrittr
#--------------------------------------------------------------------------------------
toxval_source_check_extraction_documents <- function(db, write_output=FALSE){

  # Query to check document type counts for each source
  doc_type_query = paste0("SELECT DISTINCT a.source_table, b.document_type, count(*) AS n ",
                          "FROM documents_records a LEFT JOIN documents b on a.fk_doc_id = b.id ",
                          "WHERE a.source_table IN ",
                          "(SELECT source_table FROM chemical_source_index ",
                          "WHERE source_status='active') ",
                          "GROUP BY a.source_table, b.document_type")

  # Get list of all active tables with extraction document associations
  active_extraction = runQuery(doc_type_query, db) %>%
    dplyr::filter(document_type == "extraction") %>%
    dplyr::pull(var = "source_table") %>%
    paste0("'", ., "'")

  # Query to check for active sources without extraction document associations
  no_extraction_query = paste0("SELECT source_table FROM chemical_source_index ",
                               "WHERE source_table NOT IN (",
                               toString(active_extraction),
                               ") AND source_status='active'")
  active_no_extraction = runQuery(no_extraction_query, db)

  # Query for documents w/o document type
  doc_query = paste0("SELECT * FROM documents_records a LEFT JOIN documents b on a.fk_doc_id = b.id ",
                     "WHERE b.document_type='<NA>' AND a.source_table IN ",
                     "(SELECT source_table FROM chemical_source_index WHERE source_status='active')")
  null_docs = runQuery(doc_query, db)

  # Write output to Excel if needed
  if (write_output) {
    writexl::write_xlsx(active_no_extraction, "no_extraction_associations.xlsx")
    writexl::write_xlsx(null_docs, "null_documents.xlsx")
  }

  # Return sources without extraction association, documents without document_type
  return(list(active_no_extraction, null_docs))
}
