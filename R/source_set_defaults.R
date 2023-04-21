#--------------------------------------------------------------------------------------
#' @description Set default value for NAs - jsut set NA to "-" for columns of type character
#' @param res The input dataframe
#' @param source The data source name
#' @return Returns the input dataframe with defaults set
#' @title FUNCTION_TITLE
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname source_set_defaults
#' @export 
#--------------------------------------------------------------------------------------
source_set_defaults <- function(res,source) {
  printCurrentFunction(source)
  for(i in 1:ncol(res)) {
    x = res[,i]
    cc = class(x)
    if(any(cc == "character")) {
      x[is.na(x)] = "-"
      res[,i] = x
    }
  }
  return(res)
}
