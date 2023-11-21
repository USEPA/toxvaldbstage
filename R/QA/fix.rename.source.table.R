#' @title fix.rename.source.table
#' @description Function to help rename a source within the toxval_source database
#' @param old_name Old source name (e.g., DOE ECORISK)
#' @param old_table Old source table name (e.g., source_lanl)
#' @param new_name New source name (e.g., DOE LANL ECORISK)
#' @param new_table New source table name (e.g., source_doe_lanl_ecorisk)
#' @return None. SQL statements are carried out.
fix.rename.source.table <- function(old_name, old_table, new_name, new_table){

  message("Renaming ", old_name , " (", old_table,") to ", new_name, " (", new_table,")")
  # Drop UPDATE triggers (not auditing a name change)
  # Drop trigger if exists already
  runQuery(query= "DROP TRIGGER IF EXISTS source_table_audit_bu" %>%
             gsub("source_table", old_table, .),
           db=db)

  # Drop trigger if exists already
  runQuery(query= "DROP TRIGGER IF EXISTS source_table_update_bu" %>%
             gsub("source_table", old_table, .),
           db=db)

  # chemical_source_index
  tmp <- runQuery(paste0("SELECT * FROM chemical_source_index WHERE source_table = '", old_table,"'"), db)
  if(nrow(tmp)){
    runQuery(paste0("UPDATE chemical_source_index SET source = '", new_name, "', ",
                    "source_table = '", new_table,"' WHERE source_table = '", old_table, "'"),
             db)
  }

  # data_profiling_dups_log
  tmp <- runQuery(paste0("SELECT * FROM data_profiling_dups_log WHERE source_table = '", old_table,"' OR ",
                         "dup_source_table = '", old_table,"'"), db)
  if(nrow(tmp)){
    runQuery(paste0("UPDATE data_profiling_dups_log SET source_table = '", new_table, "' ",
                    "WHERE source_table = '", old_table, "'"),
             db)
    runQuery(paste0("UPDATE data_profiling_dups_log SET dup_source_table = '", new_table, "' ",
                    "WHERE dup_source_table = '", old_table, "'"),
             db)
  }

  # documents_records
  tmp <- runQuery(paste0("SELECT * FROM documents_records WHERE source_table = '", old_table,"'"), db)
  if(nrow(tmp)){
    runQuery(paste0("UPDATE documents_records SET source_table = '", new_table, "' ",
                    "WHERE source_table = '", old_table, "'"),
             db)
  }

  # source_audit
  tmp <- runQuery(paste0("SELECT * FROM source_audit WHERE src_tbl_name = '", old_table,"'"), db)
  if(nrow(tmp)){
    runQuery(paste0("UPDATE source_audit SET src_tbl_name = '", new_table, "' ",
                    "WHERE src_tbl_name = '", old_table, "'"),
             db)
  }
  # source_chemical
  tmp <- runQuery(paste0("SELECT * FROM source_chemical WHERE source = '", old_name,"'"), db)
  tmp2 <- runQuery(paste0("SELECT * FROM source_chemical_orig WHERE source = '", old_name,"'"), db)
  if(nrow(tmp) | nrow(tmp2)){
    runQuery(paste0("UPDATE source_chemical SET source = '", new_name, "' ",
                    "WHERE source = '", old_name, "'"),
             db)
    runQuery(paste0("UPDATE source_chemical_orig SET source = '", new_name, "' ",
                    "WHERE source = '", old_name, "'"),
             db)
  }

  # source table itself
  tmp <- runQuery(paste0("SELECT * FROM ", old_table, " WHERE source = '", old_name,"'"), db)
  if(nrow(tmp)){
    runQuery(paste0("UPDATE ", old_table," SET source = '", new_name, "' ",
                    "WHERE source = '", old_name, "'"),
             db)
  }

  # Rename source table
  runQuery(paste0("rename table ", old_table," to ", new_table),
           db)

  message("Done.")

}
