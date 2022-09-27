#--------------------------------------------------------------------------------------
#' Input source data is used to generate the SQL for the source's toxval_source table.
#' SQL is based off a generic SQL file
#'
#' @param source name of the source being processed
#' @param res input dataframe of source data
#' @param do.halt if TRUE, halt on errors or warnings
#' @param verbose if TRUE, print diagnostic information
#' @export
#--------------------------------------------------------------------------------------
create_source_table_SQL <- function(source, res, db, do.halt=TRUE, verbose=FALSE) {
  # Normalize names
  names(res) <- names(res) %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    stringr::str_squish() %>%
    tolower()
  # PUll generic table SQL
  src_sql = parse_sql_file(filepath = "Repo/generic_toxval_source_table.sql") %T>% {
    names(.) <- "snew_source"
  }

  # Parse input data fields
  src_fields = set_field_SQL_type(src_f = res) %>%
    paste0(., ",\n")

  # Customize the source table SQL
  src_sql$snew_source = src_sql$snew_source %>%
    # Insert source table name
    gsub("snew_source", source, .) %>%
    # Insert custom fields
    gsub("source_custom_fields", src_fields, .) %T>%
    # Export a copy
    writeLines(.,
               paste0(toxval.config()$datapath,source,"/",
                      source,"_MySQL/",source,".sql"))

  # Push the new table to database
  # runQuery(query = src_sql$snew_source, db=db)
  # Return SQL if desired
  return(src_sql$snew_source)
}

#--------------------------------------------------------------------------------------
#'@description Helper function to generate SQL field types based on dataframe field types
#'@param src_f Dataframe to generate field types from
#'@return SQL string for the input dataframe's fields
#--------------------------------------------------------------------------------------
set_field_SQL_type <- function(src_f = NULL){
  lapply(names(src_f), function(f){
    # Get type
    type = typeof(src_f[[f]])
    # Get max character length
    t_len = max(nchar(src_f[[f]]), na.rm = TRUE)
    switch(type,
           "character"=ifelse(t_len >= 100,
                              "TEXT",
                              paste0("VARCHAR(",t_len,")")),
           "integer"=paste0("INT(",t_len,")"),
           "double"=paste0("INT(",t_len,")"),
           "logical"=ifelse(t_len >= 100,
                            "TEXT",
                            paste0("VARCHAR(",t_len,")")),
           { stop("Unhandled SQL type in set_field_SQL_type(): ", type) }) %>%
      paste0("`", f, "` ", .,
             ifelse(grepl("VARCHAR", .),
                    " COLLATE utf8_unicode_ci", ""),
             " DEFAULT NULL") %>%
      return()
  }) %>%
    unlist() %>%
    paste(., collapse=",\n") %>%
    return()
}
