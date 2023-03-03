#--------------------------------------------------------------------------------------
#' @#'
#'
#' @return Returns a dataframe of the length of the numeric and decimal places
#' @export
#' @title get.num.decimal.count
#' @description A function count the length of an input numeric and its decimal places

#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get.num.decimal.count
#--------------------------------------------------------------------------------------
get.num.decimal.count <- function(in_num){

  # Check if input is a numeric value
  if(!class(in_num) %in% c("numeric", "double")) return(data.frame())
  in_num = as.character(in_num)

  # Handle scientific notation conversion (either 10's or e notation)
  if (grepl("[Xx]", in_num) && grepl("^[0-9]+\\.?[0-9]* ?[Xx] ?10\\^.*$", in_num)){
    mantissa <- gsub(" ?[Xx].*", "", in_num)
    exponent <- abs(as.numeric(gsub(".*\\^", "", in_num)))
    return(data.frame(num=nchar(gsub(".", "", mantissa, fixed=TRUE))+exponent,
                      dec=nchar(sub('.*\\.', '', mantissa)) + exponent))
  } else if(grepl("[eE]", in_num) && grepl("^(-?[0-9]*)\\.?[0-9]+[eE]?[-\\+]?[0-9]+$", in_num)){
    mantissa <- gsub(" ?[eE].*", "", in_num)
    exponent <- abs(as.numeric(gsub(".*?[eE]", "", in_num)))
    return(data.frame(num=nchar(gsub(".", "", mantissa, fixed=TRUE))+exponent,
                      dec=nchar(sub('.*\\.', '', mantissa)) + exponent))
  }

  switch(as.character(stringr::str_count(in_num, "\\.")),
         "0" = data.frame(num=nchar(in_num), dec=0),
         "1" = data.frame(num = nchar(gsub(".", "", in_num, fixed=TRUE)),
                        dec = nchar(sub('.*\\.', '', in_num))),
         data.frame())
}
