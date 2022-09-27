#--------------------------------------------------------------------------------------
#' Create audit table and add BEFORE UPDATE audit triggers to source_* tables
#'
#' @param src_tbl the name of the ToxVal source table to update
#' @param map_file the file path to a Clowder document map
#' @param db the name of the database
#' @param do.halt if TRUE, halt on errors or warnings
#' @param verbose if TRUE, print diagnostic information
#' @param reset if TRUE, fully replace/update Clowder ID values, even if ID values exist
#' @export
#--------------------------------------------------------------------------------------
update.source.clowder.id <- function(src_tbl, map_file = NULL, db,
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
                                source=src_tbl,
                                map_file=map_file)

    cat("\nNeed to update code to use a better write table call approach")
    # Push temp table of updates
    con <- dbConnect(drv=RMySQL::MySQL(),user=DB.USER,password=DB.PASSWORD,host=DB.SERVER,dbname=db)
    res = dbWriteTable(con,
                       name="temp_table2",
                       value=mapped_res,
                       row.names=FALSE,
                       overwrite=TRUE)
    dbDisconnect(con)
    # Query to join and make updates
    update_query = paste0("UPDATE ", src_tbl," a INNER JOIN temp_table2 b ",
    "ON (a.source_hash = b.source_hash) SET a.clowder_id = b.clowder_id, ",
    "a.document_name = b.document_name"
    )
    # Push updates
    runStatement(query=update_query, db=db)
    # Drop temp table
    runStatement(query="DROP TABLE IF EXISTS temp_table2", db=db)
  } else {
    cat("\nNo new records to update. Set 'reset' to TRUE if a full reset is desired.")
  }
}
