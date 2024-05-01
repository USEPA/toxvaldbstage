#' @title doc_lineage_sync_clowder_metadata
#' @description Utility script to sync the Clowder metadata to the database based on Clowder ID
#' @param source_table The source table name (e.g. source_test)
#' @param db the name of the database
#' @param clowder_url URL to Clowder
#' @param clowder_api_key API key to access Clowder resources
#' @param dsID Clowder Dataset ID
#' @import httr jsonlite
#' @param batch_size PARAM_DESCRIPTION, Default: 250
#' @return Clowder metadata
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[httr]{GET}}, \code{\link[httr]{add_headers}}, \code{\link[httr]{content}}
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[tidyr]{nest}}, \code{\link[tidyr]{reexports}}
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{bind}}
#'  \code{\link[purrr]{keep}}
#'  \code{\link[stringr]{str_replace}}, \code{\link[stringr]{str_trim}}
#' @rdname doc_lineage_sync_clowder_metadata
#' @export
#' @importFrom httr GET add_headers content
#' @importFrom jsonlite fromJSON
#' @importFrom tidyr unnest starts_with any_of
#' @importFrom dplyr filter mutate select bind_rows
#' @importFrom purrr compact
#' @importFrom stringr str_replace_all str_squish
#' @param baseurl PARAM_DESCRIPTION
#' @param apiKey PARAM_DESCRIPTION
doc_lineage_sync_clowder_metadata <- function(source_table,
                                              db,
                                              clowder_url,
                                              clowder_api_key,
                                              batch_size = 100,
                                              dsID = "5e31dc1e99323f93a9f5cec0"){

  # PUll Clowder ID values
  if(!is.null(source_table) && !is.na(source_table)){
    # Filter to Clowder ID values associated with a source_table
    clowder_id_list <- runQuery(paste0("SELECT distinct clowder_id FROM documents where id in (",
                                       "SELECT fk_doc_id FROM documents_records WHERE source_table = '", source_table, "' ",
                                       "and clowder_id != '-')"), db) %>% .[[1]]
  } else {
    # Pull all to sync
    clowder_id_list <- runQuery("SELECT distinct clowder_id FROM documents where clowder_id != '-'", db=db) %>% .[[1]]
  }

  doc_tbl_names <- runQuery("SELECT * FROM documents LIMIT 1", db=db) %>%
    names() %>%
    .[!. %in% c("id")]

  ds_file_list <- clowder_get_dataset_files(dsID, clowder_url, clowder_api_key) %>%
    dplyr::rename(document_name=filename)

  # Prepare for batched updates
  # Batch update
  # https://www.mssqltips.com/sqlservertip/5829/update-statement-performance-in-sql-server/
  # batch_size <- 250
  startPosition <- 1
  endPosition <- length(clowder_id_list)# runQuery(paste0("SELECT max(id) from documents"), db=db) %>% .[[1]]
  incrementPosition <- batch_size

  while(startPosition <= endPosition){

    # Filter/get file names with dataset API
    file_info = ds_file_list %>%
      dplyr::filter(clowder_id %in% clowder_id_list[startPosition:incrementPosition])

    message("Pulling metadata...", startPosition, " to ", incrementPosition)
    # Loop through each Clowder ID value
    metadata_out <- clowder_get_file_metadata(fileID = clowder_id_list[startPosition:incrementPosition], clowder_url, clowder_api_key)

    metadata = metadata_out %>%
      dplyr::left_join(file_info, by="clowder_id")
    # Normalize names
    names(metadata) = tolower(names(metadata))
    # Get fields not to convert to JSON in documents clowder_metadata
    non_json_fields <- names(metadata) %>%
      tolower() %>%
      .[. %in% doc_tbl_names]

    # TODO Continue editing to new metadata pull approach
    # Combine with file_info to get document_name field
    # Transform records into JSON
    metadata = metadata %>%
      dplyr::mutate(clowder_metadata = convert.fields.to.json(dplyr::select(., -tidyr::any_of(non_json_fields)))) %>%
      # Select only non_json columns and JSON record
      dplyr::select(tidyr::any_of(non_json_fields), clowder_metadata) %T>% {
        names(.) <- tolower(names(.))
      }

    # Add missing fields to help combine dataframe
    metadata[, doc_tbl_names[!doc_tbl_names %in% names(metadata)]] <- NA

    # Replace "-" with NA
    metadata_out[metadata_out == '-'] <- NA

    # Generic fixes to encoding
    metadata_out = fix.non_ascii.v2(metadata_out,"documents")

    #
    # make sure all characters are in UTF8 - moved from runInsertTable.R
    # so it is applied BEFORE hashing and loading
    #
    desc <- runQuery(paste0("desc ","documents"),db)
    desc <- desc[generics::is.element(desc[,"Field"],names(metadata_out)),]
    for(i in 1:dim(desc)[1]) {
      col <- desc[i,"Field"]
      type <- desc[i,"Type"]
      if(grepl("varchar|text", type)) {
        # if(verbose) cat("   enc2utf8:",col,"\n")
        x <- as.character(metadata_out[[col]])
        x[is.na(x)] <- "-"
        x <- enc2native(x)
        x <- iconv(x,from="latin1",to="UTF-8")
        x <- iconv(x,from="LATIN1",to="UTF-8")
        x <- iconv(x,from="LATIN2",to="UTF-8")
        x <- iconv(x,from="latin-9",to="UTF-8")
        metadata_out[[col]] <- enc2utf8(x)
      }
    }

    metadata_out <- metadata_out %>%
      #https://stackoverflow.com/questions/58312873/how-to-remove-registered-trademark-and-copyright-symbols-from-a-string
      dplyr::mutate(clowder_metadata = stringr::str_replace_all(clowder_metadata, "\\u00AE|\\u00a9|\\u2122", "") %>%
                      # Replace unicode hyphens
                      stringr::str_replace_all(., "\\u002D|\\u05BE|\\u1806|\\u2010|\\u2011|\\u2012|\\u2013|\\u2014|\\u2015|\\u207B|\\u208B|\\u2212|\\uFE58|\\uFE63|\\uFF0D", "-") %>%
                      # Replace single-quote or apostrophe
                      stringr::str_replace_all(., "\\u2019", "'") %>%
                      stringr::str_squish())

    # Replace "-" with NA
    metadata_out[metadata_out == '-'] <- NA

    message("...Inserting new data in batch: ", batch_size, " startPosition: ", startPosition," : incrementPosition: ", incrementPosition, " at: ", Sys.time())
    # Pull document IDs to update in the batch
    batch_doc_ids <- runQuery(paste0("SELECT id FROM documents WHERE clowder_id in ('",
                                     paste0(clowder_id_list[startPosition:incrementPosition], collapse="', '"),"')"), db=db)
    updateQuery = paste0("UPDATE documents a INNER JOIN z_updated_df b ",
                         "ON (a.clowder_id = b.clowder_id) SET ",
                         paste0("a.", names(metadata_out)[!names(metadata_out) %in% c("clowder_id")],
                                " = b.", names(metadata_out)[!names(metadata_out) %in% c("clowder_id")],
                                collapse = ", "),
                         " WHERE a.id in (", toString(batch_doc_ids$id), ")")

    runUpdate(table="documents",
              updateQuery = updateQuery,
              updated_df = metadata_out,
              db=db,
              trigger_check = FALSE)

    startPosition <- startPosition + batch_size
    incrementPosition <- startPosition + batch_size - 1
  }
}

clowder_get_file_metadata <- function(fileID, baseurl, apiKey){
  # Rest between requests
  Sys.sleep(0.25)

  # Format URL for request
  url = paste0(baseurl, "/api/files/metadata.jsonld?id=",
               # Combine multiple file ID values if provided
               paste0(fileID, collapse="&id="),
               "&?limit=0")
  # Pull metadata for input files
  metadata = httr::GET(
    url=url,
    httr::accept_json(),
    httr::content_type_json(),
    # Use API Key for authorization
    httr::add_headers(`X-API-Key` = apiKey),
    encode = "json"
  ) %>%
    httr::content()

  # Format data to return (combine across multiple metadata submissions)
  lapply(metadata, function(f){
    lapply(f, function(ff){
      ff %>%
        purrr::pluck("content") %>%
        purrr::compact() %>%
        data.frame() %>%
        tidyr::unnest(cols=c()) %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), ~as.character(.)))
    }) %>%
      dplyr::bind_cols()
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(clowder_id = fileID) %>%
    return()
}

clowder_get_dataset_files <- function(dsID, baseurl, apiKey){
  # Rest between requests
  Sys.sleep(0.25)
  # Pull all Clowder Files from input dataset
  c_files_list = httr::GET(
    paste0(baseurl, "/api/datasets/", dsID,"/listAllFiles?limit=0"),
    httr::accept_json(),
    httr::content_type_json(),
    # Use API Key for authorization
    httr::add_headers(`X-API-Key` = apiKey),
    encode = "json"
  ) %>%
    httr::content()
  # Format data
  c_files_list = lapply(c_files_list, function(f){
    f %>%
      data.frame() %>%
      tidyr::unnest(cols=c())
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::select(clowder_id = id, `folders.name`, filename) %>%
    return()
}
