#--------------------------------------------------------------------------------------
#' Create audit table and add BEFORE UPDATE audit triggers to source_* tables
#'
#' @param db the name of the database
#' @param do.halt if TRUE, halt on errors or warnings
#' @param verbose if TRUE, print diagnostic information
#' @param reset if TRUE, DROP source_* table trigger (to reset/edit it)
#' @export
#--------------------------------------------------------------------------------------
init.audit.table <- function(db, do.halt=FALSE, verbose=FALSE, reset=TRUE){
  # List of ID fields not to be added to JSON of audit
  id_list = c("source_id", "source_hash", "parent_hash", "version", "qc_status",
              "create_time", "modify_time", "created_by")
  # Load SQL file with audit table and trigger creation queries
  audit_sql = parse_sql_file() %T>%
    { names(.) <- c("create_audit", "audit_trigger", "drop_trigger") }

  # Create audit table
  runQuery(query=audit_sql$create_audit, db=db)

  # Get list of source tables to add triggers
  tblList = runQuery(query = paste0("SHOW TABLES FROM ", db),
                     db=db) %>% unlist() %>% unname() %>%
    # Filter to those named "source_*"
    .[grepl("source_", .)] %>%
    # Ignore those like source_audit or source_chemical
    .[!grepl("chemical|audit", .)]

  # Loop through each table, get fields for JSON, reparse SQL, run Statement
  s_tbl = tblList[1]
  field_list = runQuery(query=paste0("SELECT * FROM ", s_tbl, " LIMIT 1"),
                        db=db) %>%
    names() %>%
    .[!. %in% id_list]
  src_trigger = audit_sql$audit_trigger %>%
    # Insert source table name
    gsub("source_table", s_tbl, .) %>%
    # Format JSON
    gsub("JSON_OBJECT\\(\\)", paste0("JSON_OBJECT(",
                                 paste0("'", field_list, "', OLD.", field_list,
                                        collapse=", "),
                                 ")"),
         .) %>%
    paste0("DELIMITER // ", ., "END;// DELIMITER;")
  # Reset trigger if desired
  if(reset){
    runQuery(query=audit_sql$drop_trigger %>%
               gsub("source_table", s_tbl, .),
             db=db)
  }
  # Apply trigger to table
  runQuery(query=src_trigger, db=db)
}

#'@description Function to parse SQL file into SQL query strings
#'@param filepath Input SQL filepath
#'@import stringr dplyr
parse_sql_file <- function(filepath = "Repo/audit_sql/toxval_source_audit_init.sql"){
  # Read in SQL file lines
  raw_query = readr::read_lines(filepath)
  # Replace -- comments with /**/ contained comments
  raw_query = lapply(raw_query, function(line){
    if(grepl("--",line) == TRUE){
      line <- paste(sub("--","/*",line),"*/")
    }
    return(line)
  }) %>% unlist()

  # Empty list to append collapsed query lines
  clean_query = list()
  # Empty string to append query lines to for ";" checks
  tmp_query = ""
  for(i in seq_len(length(raw_query))){
    tmp_query = paste(tmp_query, raw_query[i], sep=" ")
    if(grepl(";", raw_query[i])){
      clean_query = append(clean_query, tmp_query %>%
                             stringr::str_squish())
      tmp_query = ""
    }
  }
  # Return cleaned list of queries to run
  return(clean_query)
}
