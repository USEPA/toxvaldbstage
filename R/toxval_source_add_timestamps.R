# Script to add timestamp columns to toxval_source tables
# By: Jonathan Taylor Wall
# Created: 2022-05-23
# R version 3.6.1 (2019-07-05)
# dplyr_1.0.2; RMySQL_0.10.20; DBI_1.1.0

#-------------------------------------------------------------------------------------
#' @title toxval.source.add.timestamps
#' @description get chemical info from source db tables
#' @param source.db The version of toxval source database to use.
#' @return None. SQL statements are executed to add timestamp columns.
#' @import RMySQL dplyr
#' @export
#--------------------------------------------------------------------------------------
toxval.source.add.timestamps <- function(source.db){
  tbl_list = runQuery(query=paste0("SELECT TABLE_NAME FROM information_schema.tables WHERE TABLE_TYPE = 'base table' AND TABLE_SCHEMA='",source.db,"'"),
                      db=source.db) %>%
    unlist() %>% unname()
  # List of fields to add (name = field definitions)
  new_fields = list(create_time = "create_time datetime DEFAULT CURRENT_TIMESTAMP",
                    update_time = "modify_time datetime DEFAULT NULL ON UPDATE CURRENT_TIMESTAMP")

  # Loop through each table in toxval_source, check for timestamp columns, add if not present
  for(t in tbl_list){
    # Get list of fields already present in table
    f_list = runQuery(paste0("SELECT * FROM ", t, " LIMIT 1"), db=source.db) %>%
      names()
    # Loop through each new_fields entry to add to table
    for(f in names(new_fields)){
      # Only add if not already present
      if(!f %in% f_list){
        # Run Statement
        runStatement(query=paste0("ALTER TABLE ", t, " ADD COLUMN ", new_fields[[f]]),
                     db=source.db)
      }
    }
  }
}
