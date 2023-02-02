#' @title set_chemical_id.R
#' Helper function to externalize the chemical_id creation process outsie of import.
#' This helps in cases where the chemical_id hashing process is changed, and changes can
#' be implemented easily.
#' @param db The version of toxval_source into which the source is loaded.
#' @hash.check.only Boolean for only checking the hash to compare systems. (Default = TRUE)
#' @import dplyr digest
#' @return Hashes for input data "in_data"
#' @export
set_chemical_id <- function(db, hash.check.only=TRUE){
  # Old approach to chemical hashing
  old_chemical_hash <- function(chems){
    for(i in 1:nrow(chems)) {
      prefix = chems$prefix[i]
      chems[i,"chemical_id"] = paste0(prefix,"_",digest(paste0(chems[i,c("raw_casrn","raw_name","cleaned_casrn","cleaned_name")],collapse=""),
                                                        algo="xxhash64",
                                                        serialize = FALSE))
      if(i%%1000==0) cat(i," out of ",nrow(chems),"\n")
    }
    return(chems)
  }

  src_chem = runQuery("SELECT chemical_id, raw_name, raw_casrn, cleaned_name, cleaned_casrn FROM source_chemical", db)

  compare = src_chem %>%
    separate(chemical_id, c("prefix", "id"), remove=FALSE) %>%
    dplyr::rename(old_chemical_id = chemical_id) %>%
    old_chemical_hash() %>%
    mutate(compare = old_chemical_id == chemical_id)
}


