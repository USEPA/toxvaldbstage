#--------------------------------------------------------------------------------------
#' Create audit table and add BEFORE UPDATE audit triggers to source_* tables
#'
#' @param src_tbl the name of the ToxVal source table to update
#' @param source the name of the ToxVal source (different from src_tbl)
#' @param map_file the file path to a Clowder document map
#' @param db the name of the database
#' @param do.halt if TRUE, halt on errors or warnings
#' @param verbose if TRUE, print diagnostic information
#' @param reset if TRUE, fully replace/update Clowder ID values, even if ID values exist
#' @export
#--------------------------------------------------------------------------------------
update.source.clowder.id <- function(src_tbl, source, map_file = NULL, db,
                                     do.halt=FALSE, verbose=FALSE, reset=FALSE){
  # Get toxval_source data to use for document mapping and updating
  res = runQuery(query=paste0("SELECT * FROM ", src_tbl), db=db)
  # Check if clowder_id field exists
  if(!"clowder_id" %in% names(res)){
    stop("Cannot update source without a clowder_id fields in toxval_source")
  }
  # Get map_file if provided
  if(!is.null(map_file)){
    f_ext = strsplit(basename(map_file), split="\\.")[[1]][2]
    map_file = switch(f_ext,
                      csv = readr::read_csv(map_file, col_types=readr::cols()),
                      xlsx = readxl::read_xlsx(map_file),
                      # Default to "" if
                      stop("map_file extension not supported"))
  }
  # If not a full reset, filter to only those without a Clowder ID
  if(!reset){
    res = res %>%
      filter(is.na(clowder_id) | clowder_id == "")
  }
  if(nrow(res)){
    # Map Clowder ID values like normal
    mapped_res = set_clowder_id(res=res,
                                source=source,
                                map_file=map_file)

    # Query to join and make updates (update Clowder info, keep same time, disabled triggers for audit)
    update_query = paste0("UPDATE ", src_tbl," a INNER JOIN z_updated_df b ",
                          "ON (a.source_hash = b.source_hash) SET a.clowder_id = b.clowder_id, ",
                          "a.document_name = b.document_name, a.create_time = b.create_time"
    )
    # Push temp table of updates
    runUpdate(table=src_tbl, updateQuery=update_query, updated_df=mapped_res, db=db)
  } else {
    cat("\nNo new records to update. Set 'reset' to TRUE if a full reset is desired.")
  }
}
