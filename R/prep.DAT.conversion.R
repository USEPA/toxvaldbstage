#' @title prep.DAT.conversion
#' @description Select and rename DAT audit columns for toxval_source, calculate new source_hash
#' @param in_dat PARAM_DESCRIPTION
#' @param hash_id_list PARAM_DESCRIPTION
#' @param source PARAM_DESCRIPTION
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
prep.DAT.conversion <- function(in_dat, hash_id_list, source){
  in_dat = in_dat %>%
    dplyr::rename(parent_hash = src_record_id) %>%
    # Remove extraneous DAT fields
    dplyr::select(-uuid, -description, -total_fields_changed, -dataset_description, -DAT_domain_name,
                  -domain_description, -DAT_source_name, -source_description, -status_description) %>%
    # Alphabetize the columns to ensure consistent hashing column order
    .[, sort(colnames(.))] %>%
    tidyr::unite("pre_source_hash", tidyr::any_of(names(.)[!names(.) %in% hash_id_list]),
                 sep="", remove = FALSE) %>%
    # Set source_hash
    dplyr::mutate(source_hash = purrr::map_chr(pre_source_hash, digest, serialize=FALSE)) %>%
    dplyr::select(-pre_source_hash)

  return(in_dat)
}
