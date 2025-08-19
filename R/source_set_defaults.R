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
  res %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.character),
        ~dplyr::case_match(
          .,
          c(NA, as.character(NA), "") ~ "-",
          .default = .)
      )
    ) %>%
    return()
}
