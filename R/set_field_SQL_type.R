#--------------------------------------------------------------------------------------
#' @description Helper function to generate SQL field types based on dataframe field types
#' @param src_f Dataframe to generate field types from
#' @param default_fields Default fields already handled by input generic SQL
#' @return SQL string for the input dataframe's fields
#' @title FUNCTION_TITLE
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{bind}}
#' @rdname set_field_SQL_type
#' @export
#' @importFrom dplyr bind_rows
#--------------------------------------------------------------------------------------
set_field_SQL_type <- function(src_f = NULL, default_fields = NULL){
  lapply(names(src_f)[!names(src_f) %in% default_fields], function(f){
    # # Get type
    # type = typeof(src_f[[f]])
    # Get class and type, which matters for things like Dates
    type = paste0(c(class(src_f[[f]]), typeof(src_f[[f]])), collapse=";")

    # Get numeric/double length and decimal places
    if(grepl("numeric|double", type)){
      t_len = lapply(src_f[[f]] %>% unique(), function(num){
        return(get.num.decimal.count(num))
      }) %>% dplyr::bind_rows()
      # Default if nothing returned (case of all NA field)
      if(!nrow(t_len)){
        t_len = data.frame(num=8, dec=3)
      }
    } else {
      # Get max character length
      t_len = max(nchar(src_f[[f]]), na.rm = TRUE) %>%
        suppressWarnings() %>%
        # Handle case of empty column, or empty strings, set size to 25 or 10 default guess
        ifelse(is.infinite(.) | . == 0,
               ifelse(grepl("character|logical", type), 25, 10),
               .)
    }

    switch(type,
           "character;character"=ifelse(t_len >= 25,
                                        "TEXT",
                                        paste0("VARCHAR(",t_len,")")),
           "integer;integer"=paste0("INT(",t_len,")"),
           "numeric;double"=paste0("DECIMAL(", max(t_len$num)+max(t_len$dec),", ", max(t_len$dec),")"), # paste0("DOUBLE(",t_len,",",t_len,")"),
           "logical;logical"=ifelse(t_len >= 25,
                                    "TEXT",
                                    paste0("VARCHAR(",t_len,")")),
           "POSIXct;POSIXt;double"= "date",
           { message("Unhandled SQL type in set_field_SQL_type() for field '", f,"': ", type); browser(); stop() }) %>%
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
