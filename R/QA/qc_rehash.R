#--------------------------------------------------------------------------------------
#' Updates the source_hash for an input dataframe
#'
#' @param in_dat Input dataframe to re-hash
#' @param non_hash_cols List/vector of fields to not hash
#' @param source.table Name of the source table in toxval_source
#' @param source.name Name of the source in human readable format
#' @param db the name of the database
#' @param generic.fixes Boolean to apply generic fixes to data before rehashing, default = TRUE
#' @export
#--------------------------------------------------------------------------------------
qc_rehash <- function(in_dat, non_hash_cols, source.table, source.name, db, generic.fixes=TRUE){

  if(generic.fixes){
    ###########################################################
    ### Generic fixes/cleaning based on source_prep_and_load.R
    ###########################################################
    in_dat = fix.non_ascii.v2(in_dat,source.name)

    #
    # make sure all characters are in UTF8 - moved from runInsertTable.R
    # so it is applied BEFORE hashing and loading
    #
    desc <- runQuery(paste0("desc ",source.table),db)
    desc <- desc[is.element(desc[,"Field"],names(in_dat)),]
    for(i in 1:dim(desc)[1]) {
      col <- desc[i,"Field"]
      type <- desc[i,"Type"]
      if(contains(type,"varchar") || contains(type,"text")) {
        # if(verbose) cat("   enc2utf8:",col,"\n")
        x <- as.character(in_dat[[col]])
        x[is.na(x)] <- "-"
        x <- enc2native(x)
        x <- iconv(x,from="latin1",to="UTF-8")
        x <- iconv(x,from="LATIN1",to="UTF-8")
        x <- iconv(x,from="LATIN2",to="UTF-8")
        x <- iconv(x,from="latin-9",to="UTF-8")
        in_dat[[col]] <- enc2utf8(x)
      }
    }

    in_dat = source_set_defaults(res=in_dat, source=source.name)
  }

  # Hash new
  in_dat = in_dat %>%
    dplyr::rename(parent_hash = source_hash) %>%
    # Alphabetize the columns to ensure consistent hashing column order
    .[, sort(colnames(.))] %>%
    tidyr::unite("pre_source_hash", any_of(names(.)[!names(.) %in% non_hash_cols]),
                 sep="", remove = FALSE)

  for (i in 1:nrow(in_dat)){
    in_dat[i,"source_hash"] <- digest(paste0(in_dat$pre_source_hash[i],collapse=""), serialize = FALSE)
    if(i%%1000==0) cat(i," out of ",nrow(in_dat),"\n")
  }

  in_dat %>%
    select(-pre_source_hash) %>%
    return()
}
