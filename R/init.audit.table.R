#--------------------------------------------------------------------------------------
#' Create audit table and add BEFORE UPDATE audit triggers to source_* tables
#'
#' @param db the name of the database
#' @param do.halt if TRUE, halt on errors or warnings
#' @param verbose if TRUE, print diagnostic information
#' @export
#--------------------------------------------------------------------------------------
init.audit.table <- function(db, do.halt=FALSE, verbose=FALSE){
  # List of ID fields not to be added to JSON of audit
  id_list = c("source_id", "source_hash", "parent_hash", "version", "qc_status",
              "create_time", "created_by", "modify_time")
  # Load SQL file with audit table and trigger creation queries
  audit_sql = parse_sql_file("Repo/audit_sql/toxval_source_audit_init.sql") %T>%
    { names(.) <- c("create_audit", "bu_audit_trigger", "drop_bu_audit_trigger",
                    "bu_source_trigger", "drop_bu_source_trigger") }

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
  for(s_tbl in tblList){
    cat("Applying audit trigger to ", s_tbl, "\n")
    field_list = runQuery(query=paste0("SELECT * FROM ", s_tbl, " LIMIT 1"),
                          db=db) %>%
      names()
    # Update audit fields as needed
    audit.update.fields(s_tbl=s_tbl, field_list=field_list, db=db)
    # Remove ID fields (don't add to JSON record field of audit table)
    field_list = field_list[!field_list %in% id_list]
    # Parse custom trigger for source table and fields
    # BEFORE UPDATE TRIGGER
    src_bu_audit_trigger = audit_sql$bu_audit_trigger %>%
      # Insert source table name
      gsub("source_table", s_tbl, .) %>%
      # Format JSON
      gsub("JSON_OBJECT\\(\\)", paste0("JSON_OBJECT(",
                                       paste0("'", field_list, "', OLD.`", field_list,
                                              collapse="`, "),
                                       "`)"),
           .) %>%
      paste0(#"DELIMITER // \n",
             ., "\nEND;")#// DELIMITER;")

    # AFTER UPDATE TRIGGER
    src_bu_source_trigger = audit_sql$bu_source_trigger %>%
      # Insert source table name
      gsub("source_table|source_update", s_tbl, .) %>%
      # Format JSON
      paste0(#"DELIMITER // \n",
        ., "\nEND;")#// DELIMITER;")

    # Drop trigger if exists already
    runQuery(query=audit_sql$drop_bu_audit_trigger %>%
               gsub("source_table", s_tbl, .),
             db=db)

    # Drop trigger if exists already
    runQuery(query=audit_sql$drop_bu_source_trigger %>%
               gsub("source_table", s_tbl, .),
             db=db)

    # Apply trigger to table
    runQuery(query=src_bu_audit_trigger, db=db)
    runQuery(query=src_bu_source_trigger, db=db)
  }
}

#--------------------------------------------------------------------------------------
#' @description Function to add/modify/delete select audit columns to source table
#' @param s_tbl Source table name to apply changes to
#' @param field_lsit List of current field names in source table
#' @param db the name of the database
#--------------------------------------------------------------------------------------
audit.update.fields <- function(s_tbl, field_list, db){
  # Update create_time to always update timestamp
  if("create_time" %in% field_list){
    runQuery(paste0("ALTER TABLE ",s_tbl,
                    " MODIFY create_time datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP;"),
             db)
  } else {
    # Add create_time if not present
    runQuery(paste0("ALTER TABLE ", s_tbl,
                    " ADD COLUMN create_time datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP;"),
             db=db)
  }

  # Drop modify_time
  if("modify_time" %in% field_list){
    runQuery(query = paste0("ALTER TABLE ",s_tbl," DROP COLUMN modify_time;"),
             db=db)
  }

  update_list = c("version", "qc_flags", "qc_notes") %>%
    .[!. %in% field_list]
  for(u in update_list){
    if(!u %in% field_list){
      query = switch(u,
                     # Add version
                     version = paste0("ALTER TABLE ",s_tbl,
                                      " ADD COLUMN version int(11) NOT NULL DEFAULT 1 AFTER parent_hash;"),
                     # Add qc_flags
                     qc_flags = paste0("ALTER TABLE ",s_tbl,
                            " ADD COLUMN qc_flags text DEFAULT NULL AFTER version;"),
                     # Add qc_notes
                     qc_notes = paste0("ALTER TABLE ",s_tbl,
                            " ADD COLUMN qc_notes text DEFAULT NULL AFTER qc_flags;"),
                     { NULL }
      )
      runQuery(query=query, db=db)
    }
  }
}
