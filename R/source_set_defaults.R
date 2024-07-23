#--------------------------------------------------------------------------------------
#' @description Set default value for NAs - just set NA to "-" for columns of type character
#' @param res The input dataframe
#' @param source The data source name
#' @return Returns the input dataframe with defaults set
#' @title source_set_defaults
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname source_set_defaults
#' @export
#' @seealso
#'  [pull][dplyr::pull]
#' @importFrom dplyr pull
#--------------------------------------------------------------------------------------
source_set_defaults <- function(res,source) {
  printCurrentFunction(source)
  for(i in 1:ncol(res)) {
    x = res %>% dplyr::pull(i)
    cc = class(x)
    # Replace NA characters with "-"
    if(any(cc == "character")) {
      x[x %in% c(NA, "")] = "-"
      res[,i] = x
    }
  }
  return(res)
}
