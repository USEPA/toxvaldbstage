#--------------------------------------------------------------------------------------
#' @title toxval.source.import.dedup
#' @description Perform deduping on data before it is sent to toxval_source
#' @param res dataframe containing the source data to dedup
#' @param dedup_fields vector containing field names to dedup, Default: NULL (all fields but hashing cols)
#' @param hashing_cols vector containing field names of hashing columns, Default: toxval.config()$hashing_cols
#' @param delim string used to separate collapsed values, Default: ' |::| '
#' @return dataframe containing deduped source data
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{select}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{context}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{na_if}}, \code{\link[dplyr]{distinct}}
#' @rdname toxval.source.import.dedup
#' @export
#' @importFrom dplyr select group_by summarise n filter mutate across any_of na_if ungroup distinct
#--------------------------------------------------------------------------------------
toxval.source.import.dedup <- function(res,
                                       dedup_fields=NULL,
                                       hashing_cols=toxval.config()$hashing_cols,
                                       delim=" |::| ") {
  cat("Deduping data\n")

  # If no dedup fields provided, set dedup_fields to be all cols but source_hash and hashing_cols
  if(is.null(dedup_fields)) {
    dedup_fields = names(res %>% dplyr::select(-dplyr::any_of(c("source_hash", hashing_cols))))
  }

  # Add source_hash column
  res.temp = source_hash_vectorized(res, hashing_cols)
  res$source_hash = res.temp$source_hash

  # Check for immediate duplicate hashes
  dup_hashes = res %>%
    dplyr::group_by(source_hash) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::filter(n > 1)

  # Perform deduping only if there are duplicate entries
  if(nrow(dup_hashes)) {
    cat("Duplicate records identified. Performing deduping...\n")
    # Dedup by collapsing non hashing columns to dedup
    res = res %>%
      dplyr::group_by(source_hash) %>%
      dplyr::mutate(dplyr::across(dplyr::any_of(!!dedup_fields),
                                  ~paste0(.[!is.na(.)], collapse=!!delim) %>%
                                    dplyr::na_if("NA") %>%
                                    dplyr::na_if("")
      )) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()

    # Check if success
    dup_hashes = res %>%
      dplyr::group_by(source_hash) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::filter(n > 1)

    if(nrow(dup_hashes)) {
      cat("Deduping failed. Duplicate records still present.\n")
      browser()
    } else {
      cat("Deduping was successful. Returning...\n")
    }
  } else {
    cat("No duplicate records found.\n")
  }

  res = res %>% dplyr::select(-source_hash)
  return(res)
}




