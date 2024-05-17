#' @title prep.DAT.conversion
#' @description Select and rename DAT audit columns for toxval_source, calculate new source_hash
#' @param in_dat PARAM_DESCRIPTION
#' @param hash_id_list PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{rename}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{mutate}}
#'  \code{\link[tidyr]{unite}}, \code{\link[tidyr]{reexports}}
#'  \code{\link[purrr]{map}}
#' @rdname prep.DAT.conversion
#' @export
#' @importFrom dplyr rename select mutate
#' @importFrom tidyr unite any_of
#' @importFrom purrr map_chr
prep.DAT.conversion <- function(in_dat=NULL, hash_id_list=NULL, hashing_type = "base"){
  in_dat = in_dat %>%
    dplyr::rename(parent_hash = src_record_id) %>%
    # Alphabetize the columns to ensure consistent hashing column order
    .[, sort(colnames(.))]

  if(hashing_type == "base"){
    in_dat.temp = in_dat %>%
      dplyr::select(-dplyr::any_of(hash_id_list))
    # Old hashing system
    for (i in 1:nrow(in_dat)){
      row <- in_dat.temp[i,]
      in_dat[i,"source_hash"] <- digest::digest(paste0(row,collapse=""), serialize = FALSE)
      if(i%%1000==0) cat(i," out of ",nrow(in_dat),"\n")
    }
  } else if(hashing_type == "vectorized"){
    in_dat.temp = source_hash_vectorized(in_dat, hashing_cols=toxval.config()$hashing_cols)
    in_dat$source_hash = in_dat.temp$source_hash
  } else {
    stop("'", hashing_type, "' not a supported hashing_type. Set to 'base' or 'vectorized'.")
  }

  return(in_dat)
}
