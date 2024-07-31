#-------------------------------------------------------------------------------------
#' @title toxval_source_check_extraction_documents
#' @description Check which sources do not have "extraction document" associations
#' in their records (by source hash) and get list of docs with null document_type
#' @param source.db The version of toxval source database to use.
#' @param toxval.db The version of toxval database to use.
#' @param write_output Whether to write output to Excel (FALSE by default)
#' @return List of sources without extraction document associations and document information
#' for docs without document_type. Export XLSX files produced if specified.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{pull}}, \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{distinct}}
#'  \code{\link[writexl]{write_xlsx}}
#' @rdname toxval_source_check_extraction_documents
#' @export
#' @importFrom dplyr filter pull bind_rows distinct
#' @importFrom writexl write_xlsx
#--------------------------------------------------------------------------------------
toxval_source_check_extraction_documents <- function(source.db, toxval.db, write_output=FALSE){
  cat("Getting list of sources with no extraction document associations... ")
  # Query to check document type counts for each source
  doc_query = paste0("SELECT DISTINCT a.source_table, b.document_type, count(*) AS n ",
                     "FROM documents_records a LEFT JOIN documents b on a.fk_doc_id = b.id ",
                     "WHERE (a.source_table IN ",
                     "(SELECT source_table FROM chemical_source_index WHERE source_status='active') ",
                     "OR a.source_table IN ",
                     "('ECOTOX', 'ToxRefDB', 'Uterotrophic Hershberger DB', 'ChemIDplus')) ",
                     "GROUP BY a.source_table, b.document_type")

  # Get list of all active tables with extraction document associations
  active_extraction_df = runQuery(doc_query, source.db) %>%
    dplyr::filter(document_type == "extraction")
  active_extraction = active_extraction_df %>%
    dplyr::pull(var = "source_table") %>%
    paste0("'", ., "'")

  # Query to check for active sources without extraction document associations
  no_extraction_query = paste0("SELECT source_table FROM chemical_source_index ",
                               "WHERE source_table NOT IN (",
                               toString(active_extraction),
                               ")")
  active_no_extraction = runQuery(no_extraction_query, source.db) %>%
    dplyr::filter(source_table != "direct load")

  # Check direct load sources as well
  missing_direct_load_sources = tibble::as_tibble(
    c('ECOTOX', 'ToxRefDB', 'Uterotrophic Hershberger DB', 'ChemIDplus')
  ) %>%
    dplyr::rename(source_table = value) %>%
    dplyr::left_join(active_extraction_df, by=c("source_table")) %>%
    dplyr::filter(is.na(document_type)) %>%
    dplyr::select(source_table)

  # Add direct load sources missing extraction document to active_no_extraction
  active_no_extraction = active_no_extraction %>%
    dplyr::bind_rows(missing_direct_load_sources) %>%
    dplyr::distinct()

  # Record list of active sources without extraction document associations, if needed
  if (write_output) {
    writexl::write_xlsx(active_no_extraction, "Repo/toxval_source_status_reports/no_extraction_document_associations.xlsx")
  }
  cat("Done\n")

  cat("Getting information for documents that are missing document_type field... ")
  # Query source_hash, source_table, and document_type for active sources
  doc_cat_info <- runQuery(paste0("SELECT distinct a.source_hash, a.source_table, b.document_type ",
                                  "FROM documents_records a ",
                                  "LEFT JOIN documents b on a.fk_doc_id = b.id ",
                                  "WHERE (a.source_table IN ",
                                  "(SELECT source_table FROM chemical_source_index WHERE source_status='active') ",
                                  "OR a.source_table IN ",
                                  "('ECOTOX', 'ToxRefDB', 'Uterotrophic Hershberger DB', 'ChemIDplus'))"),
                           source.db)

  # Get documents with null document_type
  docs_missing_doc_type = lapply(unique(doc_cat_info$source_table), function(s_tbl){
    # Check if table exists
    s_check = runQuery(paste0("SHOW TABLES LIKE '", s_tbl, "'"), source.db)
    # Get hash values if source table
    if(nrow(s_check)) {
      hashes = runQuery(paste0("SELECT DISTINCT source_hash FROM ", s_tbl), source.db)
    } else {
      # If not a source table, check if direct load source
      s_check = runQuery(paste0("SELECT DISTINCT source FROM toxval WHERE source='", s_tbl, "'"), toxval.db)
      if(nrow(s_check)) {
        hashes = runQuery(paste0("SELECT DISTINCT source_hash FROM toxval WHERE source='", s_tbl, "'"), toxval.db)
      }
    }
    # Return NULL if not present in toxval
    if(!nrow(s_check)) return(NULL)

    # Pull data for existing table
    doc_cat_info %>%

      # Filter for existing documents with null document type
      dplyr::filter(source_table == s_tbl,
                    source_hash %in% hashes$source_hash,
                    is.na(document_type)) %>%
      return()
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct()

  # Record list of documents with null document_type, if needed
  if (write_output) {
    writexl::write_xlsx(docs_missing_doc_type, "Repo/toxval_source_status_reports/docs_missing_doc_type.xlsx")
  }
  cat("Done")

  return (list(active_no_extraction, docs_missing_doc_type))
}
