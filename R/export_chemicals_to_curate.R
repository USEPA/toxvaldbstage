# Script to export chemical entries that need curation
# 2023-11-29

# export_chemicals_to_curate
# Export XLSX files by source for chemical curation

export_chemicals_to_curate <- function(db, export_all=FALSE){

  message("Pulling chemicals to curate...")
  chems_curate <- runQuery(paste0("SELECT chemical_id as external_id, raw_name,	",
                                  "raw_casrn, cleaned_name, cleaned_casrn ",
                                  "FROM source_chemical ",
                                  "WHERE DTXSID is NULL"),
                           db=db) %>%
    tidyr::separate(external_id, into=c("chemical_index", "chemical_hash"), sep="_", remove=FALSE)

  if(!export_all){
    message("...Filtering to only active Chemical ID entries for curation...")
    # Pull chemical ID values to curate
    tbl_list <- runQuery(paste0("SELECT source_table FROM chemical_source_index ",
                                "WHERE source_table is not NULL and source_status = 'active'"),
                         db=db)

    message("......Pulling chemical IDs from active source tables...")
    # Get list of chemical_id values to check
    active_chem_ids <- lapply(tbl_list$source_table, function(src){
      paste0("SELECT chemical_id FROM ", src) %>%
        runQuery(., db=db)
    }) %>%
      dplyr::bind_rows()

    # Filter to only active Chemical ID values
    chems_curate <- chems_curate %>%
      dplyr::filter(external_id %in% active_chem_ids$chemical_id)
  }

  # Export into subfiles by chemical index
  out = chems_curate %>%
    dplyr::group_split(chemical_index)

  # Prepare output directory
  if(!dir.exists("Repo/chemical_mapping/to_ticket_dsstox")){
    dir.create("Repo/chemical_mapping/to_ticket_dsstox", recursive = TRUE)
  }

  message("Exporting chemicals to curate...")
  for(f in out){
    writexl::write_xlsx(f %>%
                          dplyr::select(-chemical_index, -chemical_hash),
                        paste0("Repo/chemical_mapping/to_ticket_dsstox/",
                               f$chemical_index %>% unique(),
                               # "_", digest::digest(Sys.Date()) %>% stringr::str_sub(1, 4),
                               "_a",
                               ".xlsx"))
  }
}
