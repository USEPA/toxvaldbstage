#' @title convert.fields.to.json
#' @description Combine non-ID columns from audit table into JSON format for audit storage
#' @param in_dat data to translate to JSON format
#' @return Values in JSON format
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{bind}}
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#' @rdname convert.fields.to.json
#' @export
#' @importFrom dplyr summarise select bind_rows
#' @importFrom jsonlite toJSON
convert.fields.to.json <- function(in_dat){
  # Iterate through rows of data
  lapply(seq_len(nrow(in_dat)), function(row){
    # Translate rows to JSON format
    in_dat[row, ] %>%
      dplyr::summarise(record = jsonlite::toJSON(.)) %>%
      dplyr::select(record)
  }) %>%
    dplyr::bind_rows() %>%
    unlist() %>%
    unname() %>%
    return()
}
