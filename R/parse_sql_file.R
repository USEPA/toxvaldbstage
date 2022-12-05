#--------------------------------------------------------------------------------------
#'@description Function to parse SQL file into SQL query strings
#'@param filepath Input SQL filepath
#'@import stringr dplyr
#'@export
#--------------------------------------------------------------------------------------
parse_sql_file <- function(filepath = NULL){
  # Read in SQL file lines
  raw_query = readr::read_lines(filepath)
  # Replace -- comments with /**/ contained comments
  raw_query = lapply(raw_query, function(line){
    if(grepl("--",line) == TRUE){
      line <- paste(sub("--","/*",line),"*/")
    }
    return(line)
  }) %>% unlist() %>%
    # Remove empty rows
    .[. != ""]

  # Empty list to append collapsed query lines
  clean_query = list()
  # Empty string to append query lines to for ";" checks
  tmp_query = ""
  for(i in seq_len(length(raw_query))){
    tmp_query = paste(tmp_query, raw_query[i], sep=" ")
    # Check if has termination ; AND next line is not an IF statement
    if(grepl(";", raw_query[i]) & !grepl("IF |IF;|SET |INSERT", raw_query[i+1])){
      clean_query = append(clean_query, tmp_query %>%
                             stringr::str_squish())
      tmp_query = ""
    }
  }
  # Return cleaned list of queries to run
  return(clean_query)
}
