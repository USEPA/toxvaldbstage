#--------------------------------------------------------------------------------------
#' @description Input source data is used to generate the SQL for the source's toxval_source table.
#' SQL is based off a generic SQL file
#'
#' @param source name of the source being processed
#' @param res input dataframe of source data
#' @param src_version Version date of the source
#' @param db PARAM_DESCRIPTION
#' @param do.halt if TRUE, halt on errors or warnings
#' @param verbose if TRUE, print diagnostic information
#' @export
#' @title FUNCTION_TITLE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_trim}}
#' @rdname create_source_table_SQL
#' @importFrom stringr str_squish
#--------------------------------------------------------------------------------------
create_source_table_SQL <- function(source, res, src_version, db, do.halt=TRUE, verbose=FALSE) {
  message("source: ", source)
  # Normalize names
  names(res) <- names(res) %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    stringr::str_squish() %>%
    tolower()
  # PUll generic table SQL
  src_sql = parse_sql_file(filepath = paste0(toxval.config()$datapath, "custom_sql/generic_toxval_source_table.sql")) %T>% {
    names(.) <- "snew_source"
  }
  # Split by "`" and select even indexes which are the default fields
  default_fields = src_sql$snew_source %>%
    strsplit(split="`") %>%
    unlist() %>%
    .[seq_along(.) %% 2 == 0] %>%
    .[!. %in% c("snew_source")]

  # Parse input data fields
  src_fields = set_field_SQL_type(src_f = res, default_fields=default_fields) %>%
    paste0(., ",\n")

  # Check for large table uploads
  if(length(res) > 50){
    # Replace all varchar with TEXT fields due to row size memory limit
    src_fields = src_fields %>%
      # https://stackoverflow.com/questions/24173194/remove-parentheses-and-text-within-from-strings-in-r
      gsub("VARCHAR\\([^\\)]+\\)","TEXT", .)
  }

  # Customize the source table SQL
  src_sql$snew_source = src_sql$snew_source %>%
    # Insert source table name
    gsub("snew_source", source, .) %>%
    # Insert custom fields
    gsub("source_custom_fields", src_fields, .)

  # Parse filepath to save copy
  sql_file = paste0(toxval.config()$datapath,
                    gsub("source_", "", source),
                    "/",
                    gsub("source_", "", source),
                    "_MySQL/",
                    source, "_",
                    src_version,
                    ".sql")
  # IUCLID is special because it's a nested subfolder structure
  if(grepl("iuclid", source)){
    sql_file = gsub("^Repo", "Repo/iuclid", sql_file)
  }
  # Export a copy
  writeLines(src_sql$snew_source, sql_file)

  # Push the new table to database
  runQuery(query = src_sql$snew_source, db=db)
  # Return SQL if desired
  return(res)
}
