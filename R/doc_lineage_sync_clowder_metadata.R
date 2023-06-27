#' @title doc_lineage_sync_clowder_metadata
#' @description Utility script to sync the Clowder metadata to the database based on Clowder ID
#' @param source_table The source table name (e.g. source_test)
#' @param db the name of the database
#' @param clowder_url URL to Clowder
#' @param clowder_api_key API key to access Clowder resources
#' @import httr jsonlite
#' @param batch_size PARAM_DESCRIPTION, Default: 250
#' @return OUTPUT_DESCRIPTION
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
doc_lineage_sync_clowder_metadata <- function(source_table,
                                              db,
                                              clowder_url,
                                              clowder_api_key,
                                              batch_size = 250){

  # PUll Clowder ID values
  if(!is.null(source_table) || !is.na(source_table)){
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

  # Prepare for batched updates
  # Batch update
  # https://www.mssqltips.com/sqlservertip/5829/update-statement-performance-in-sql-server/
  # batch_size <- 250
  startPosition <- 1
  endPosition <- length(clowder_id_list)# runQuery(paste0("SELECT max(id) from documents"), db=db) %>% .[[1]]
  incrementPosition <- batch_size

  while(startPosition <= endPosition){

    message("Pulling metadata...")
    # Loop through each Clowder ID value
    metadata_out <- lapply(startPosition:incrementPosition, function(i){

      # message("Working on file: ", clowder_id_list[i])
      if(i %% floor(batch_size * 0.25) ==0) cat("...", i," out of ",length(startPosition:incrementPosition),"\n")
      # Wait between requests
      Sys.sleep(0.25)
      # Pull filename
      filename <- httr::GET(paste0(clowder_url,
                                   "/api/files/",
                                   clowder_id_list[i],
                                   "/metadata"),
                            httr::add_headers(`X-API-Key`= clowder_api_key)) %>%
        httr::content(type="text", encoding = "UTF-8") %>%
        jsonlite::fromJSON()
      # Wait between requests
      Sys.sleep(0.25)
      # Pull metadata from API
      metadata <- httr::GET(paste0(clowder_url,
                                   "/api/files/",
                                   clowder_id_list[i],
                                   "/metadata.jsonld"),
                            httr::add_headers(`X-API-Key`= clowder_api_key)) %>%
        httr::content(type="text", encoding = "UTF-8") %>%
        jsonlite::fromJSON() %>%
        tidyr::unnest(agent, names_sep = "agent_") %>%
        # Filter to only user added metadata
        dplyr::filter(`agentagent_@type` == "cat:user")

      # Check if no metadata available, fill in NA fields
      if(!length(metadata) || !nrow(metadata)) {
        metadata <- data.frame(clowder_id = clowder_id_list[i],
                   document_name = filename$filename)
        metadata[, doc_tbl_names[!doc_tbl_names %in% names(metadata)]] <- NA
        return(metadata)
      }

      metadata <- metadata %>%
        # Convert string to datetime
        # as.POSIXct("Wed Nov 23 18:22:04 2023",
        #            format="%a %B %d %H:%M:%S %Y", tz="GMT")
        dplyr::mutate(created_at = gsub(" GMT ", "", created_at) %>%
                        as.POSIXct(format="%a %B %d %H:%M:%S %Y", tz="GMT")) %>%
        # Filter to most recent metadata submission
        dplyr::filter(created_at == max(created_at)) %>%
        dplyr::select(tidyr::starts_with("content")) %>%
        dplyr::mutate(clowder_id = clowder_id_list[i],
                      document_name = filename$filename) %>%
        tidyr::unnest(cols="content")

      # https://stackoverflow.com/questions/28548245/how-to-remove-columns-from-a-data-frame-by-data-type
      # Remove dataframe/list columns
      df_check <- sapply(metadata, class) == "data.frame"
      if(any(df_check)){
        metadata <- metadata[,-which(df_check)]
      }

      # Remove NA columns
      metadata <- metadata[ , colSums(is.na(metadata))==0]

      # Skip over documents without metadata of interest
      if(length(names(metadata)) == 1) return(NULL)

      # Replace empty strings with NA
      metadata[metadata == ''] <- NA
      metadata[metadata == '-'] <- NA

      # Get fields not to convert to JSON in documents clowder_metadata
      non_json_fields <- names(metadata) %>%
        tolower() %>%
        .[. %in% doc_tbl_names]
      # Pick them out of the metadata field names
      non_json_fields <- names(metadata)[grepl(paste0(non_json_fields, collapse="|"),
                                               names(metadata), ignore.case = TRUE)]

      # Transform records into JSON
      metadata = metadata %>%
        dplyr::mutate(clowder_metadata = convert.fields.to.json(dplyr::select(., -tidyr::any_of(non_json_fields)))) %>%
        # Select only non_json columns and JSON record
        dplyr::select(tidyr::any_of(non_json_fields), clowder_metadata) %T>% {
          names(.) <- tolower(names(.))
        }

      # Add missing fields to help combine dataframe
      metadata[, doc_tbl_names[!doc_tbl_names %in% names(metadata)]] <- NA
      return(metadata)
    }) %>%
      # Remove NULL pulls (no metadata)
      purrr::compact() %>%
      dplyr::bind_rows()

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
