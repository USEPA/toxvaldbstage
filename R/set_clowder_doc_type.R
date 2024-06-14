#--------------------------------------------------------------------------------------
#' @description Update documents table entries "document_type" field based on Clowder organization
#' @param source_table The source table name (e.g. source_test). Default is NULL for "all"
#' @param source_version_date The version date for the source table. Default is NULL for "all"
#' @param clowder_url URL to Clowder
#' @param clowder_api_key API key to access Clowder resources
#' @param source.db Name of the toxval_source database to apply updates to
#' @param ds_id Clowder Dataset ID for ToxVal Clowder Documents.
#' @param clowder_id_list Optional DataFrame with field "clowder_id" values for document records to update.
#' @return None. SQL statements are performed.
#' @title set_clowder_doc_type
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[dplyr]{rename}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{mutate}}
#'  \code{\link[tidyr]{separate_rows}}, \code{\link[tidyr]{unite}}
#' @rdname set_clowder_doc_type
#' @export
#' @importFrom httr GET add_headers content
#' @importFrom jsonlite fromJSON
#' @importFrom tidyr unnest
#' @importFrom dplyr filter select mutate distinct across
#' @importFrom tidyselect where
#--------------------------------------------------------------------------------------
set_clowder_doc_type <- function(source_table=NULL,
                                 source_version_date=NULL,
                                 clowder_url=NULL,
                                 clowder_api_key=NULL,
                                 source.db=NULL,
                                 ds_id=NULL,
                                 clowder_id_list=NULL) {
  printCurrentFunction(source_table)
  if(!is.null(clowder_id_list)){
    cat("\nUsing input clowder_id_list...")
    doc_list = clowder_id_list
  } else {
    cat("\nQuerying source.db documents table...")
    # Base query to pull all document entries
    query = paste0("SELECT DISTINCT a.source_table, a.source_version_date, b.clowder_id ",
                   "FROM documents_records a ",
                   "LEFT JOIN documents b on a.fk_doc_id = b.id")

    # Filter to source table if desired
    if(!is.null(source_table)){
      query = paste0(query, " WHERE source_table = '", source_table, "'")
      cat("\nFiltering to specific source_table...")
    }

    # Filter to source version date if desired
    if(!is.null(source_version_date)){
      query = paste0(query, " AND source_version_date = '", source_version_date, "'")
      cat("\nFiltering to specific source_version_date...")
    }

    # Get list of documents from documents tables
    doc_list = runQuery(query, source.db)
  }

  # Wait between requests
  Sys.sleep(0.25)
  # Pull Clowder file information
  cat("\nPulling Clowder information...")
  file_info <- httr::GET(paste0(clowder_url,
                               "/api/datasets/",
                               ds_id,
                               "/listAllFiles"),
                        httr::add_headers(`X-API-Key`= clowder_api_key)) %>%
    httr::content(type="text", encoding = "UTF-8") %>%
    jsonlite::fromJSON() %>%
    tidyr::unnest(cols = c(folders), names_sep = "") %>%
    dplyr::filter(id %in% doc_list$clowder_id)

  if(!nrow(file_info)){
    cat("\nNo Clowder information pulled...returning...")
  } else {
    # Update document type in source.db documents table
    cat("\nPushing documents table document_type updates...")
    for(doc_type in c("extraction", "origin")){
      docs_push = file_info %>%
        dplyr::filter(grepl(paste0("^",doc_type), foldersname, ignore.case = TRUE)) %>%
        dplyr::select(clowder_id = id, document_name = filename) %>%
        dplyr::mutate(document_type = !!doc_type) %>%
        dplyr::distinct() %>%
        dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~fix.replace.unicode(.)))

      updateQuery = paste0("UPDATE documents a INNER JOIN z_updated_df b ",
                           "ON (a.clowder_id = b.clowder_id) SET ",
                           paste0("a.", names(docs_push)[!names(docs_push) %in% c("clowder_id")],
                                  " = b.", names(docs_push)[!names(docs_push) %in% c("clowder_id")], collapse = ", ")
      )
      runUpdate(table="documents", updateQuery=updateQuery, updated_df=docs_push, db=source.db)

      # # Push Update by clowder_id
      # runQuery(paste0("UPDATE documents SET document_type = '", doc_type, "' ",
      #                 "WHERE clowder_id in ('",
      #                 paste0(docs_push$id, collapse = "', '"),"')"),
      #          source.db)

      # TODO INSERT UPDATE QUERY and runUpdate
    }
  }

  cat("\nDone...")
}
