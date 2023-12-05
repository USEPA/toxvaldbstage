#--------------------------------------------------------------------------------------
#' Set the name and casrn in the source_chemical table based on CCTE API
#' @param source.db The database version to use
#--------------------------------------------------------------------------------------
update_chemical_preferred_info_by_dtxsid <- function(source.db){
  printCurrentFunction(source.db)
  dsstox.db <- toxval.config()$dsstox.db

  # Pull list of DTXSID values as a vector
  dlist = runQuery(paste0("select distinct dtxsid from source_chemical where dtxsid IS NOT NULL"),source.db)[,1]

  # Test of API is up and running
  api_test <- httr::GET("https://api-ccte.epa.gov/docs/chemical.html") %>%
    httr::content()

  # Use bulk DTXSID CCTE Chemicals API pull (limit 200 per call)
  if(!is.null(API_AUTH) & !grepl("404 Not Found", api_test)){
    cat("...Pulling DSSTox preferred_name and casrn using CCTE API...\n")
    # Split list into subsets of 200
    updated_chem_details <- dlist %>%
      split(., rep(1:ceiling(length(.)/200), each=200, length.out=length(.)))
    # Loop through the groups of 200 DTXSID values
    for(i in seq_along(updated_chem_details)){
      # Wait between calls for API courtesy
      Sys.sleep(0.25)
      cat("...Pulling DSSTox details", i , " of ", length(updated_chem_details), "\n")
      updated_chem_details[[i]] <- httr::POST(
        "https://api-ccte.epa.gov/chemical/detail/search/by-dtxsid/",
        httr::accept_json(),
        httr::content_type_json(),
        # Use API Key for authorization
        httr::add_headers(`x-api-key` = API_AUTH),
        encode = "json",
        body=as.list(updated_chem_details[[i]])
      ) %>%
        httr::content() %>%
        dplyr::bind_rows() %>%
        dplyr::select(dtxsid, name=preferredName,casrn=casrn)
    }
    # Combine all results
    updated_chem_details = dplyr::bind_rows(updated_chem_details)
  }

  # # Compare old and new identifiers
  # old = runQuery(paste0("select distinct dtxsid, name as old_name, casrn as old_casrn from source_chemical where dtxsid IS NOT NULL"), source.db)
  #
  # tmp = updated_chem_details %>%
  #   dplyr::left_join(old,
  #                    by="dtxsid") %>%
  #   dplyr::mutate(compare_name = old_name != name,
  #                 compare_casrn = old_casrn != casrn)

  # Fix non-ascii
  updated_chem_details = fix.non_ascii.v2(updated_chem_details, source=NULL)

  if(nrow(updated_chem_details)){
    # Query to inner join and make updates with updated_chem_details dataframe (temp table added/dropped)
    updateQuery = paste0("UPDATE source_chemical a INNER JOIN z_updated_df b ",
                         "ON (a.dtxsid = b.dtxsid) SET a.name = b.name, a.casrn = b.casrn")
    # Run update query
    runUpdate(table="source_chemical", updateQuery=updateQuery, updated_df=updated_chem_details, db=source.db)
  }
}
