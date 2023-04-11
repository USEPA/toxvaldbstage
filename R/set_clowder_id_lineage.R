#--------------------------------------------------------------------------------------
#' Create document records and associations in toxval_source based on input source table and document map
#' @param source_table The source table name (e.g. source_test)
#' @param map_file A dataframe of Clowder document mapping info. If NULL, will try to load a hardcoded map for the source
#' @param map_clowder_id_field Column name for the Clowder ID field of the map
#' @param clowder_url URL to Clowder
#' @param clowder_api_key API key to access Clowder resources
#' @param sync_clowder_metadata Boolean whether to sync Clowder metadata for new document records. Default is False.
#' @return Returns an updated map with newly associated toxval_source table ID values
#--------------------------------------------------------------------------------------
set_clowder_id_lineage <- function(source_table,
                                   map_clowder_id_field,
                                   map_file,
                                   clowder_url,
                                   clowder_api_key,
                                   sync_clowder_metadata=FALSE) {
  printCurrentFunction(source_table)

  if(is.null(map_file)){
    # Switch case to load specific source document map files
    map_file = switch(source_table,
                      "source_cal_oehha" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                             "clowder_v3/cal_oehha_log_with_names_20221019.xlsx")),
                      "source_iris" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                        "clowder_v3/iris_document_map_2022_08_01.xlsx")),
                      "source_pprtv_ornl" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                "clowder_v3/pprtv_ornl_docment_map_08172022_mmille16.xlsx")),
                      "source_pprtv_ncea" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                "clowder_v3/pprtv_ncea_document_map_01122023.xlsx")),
                      "source_efsa2" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                         "clowder_v3/efsa_combined_new_matched_checked_ids_07142022_jwilli29.xlsx")),
                      "source_hawc_pfas_150" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                 "clowder_v3/hawc_pfas_150_document_map_20221123.xlsx")) %>%
                        dplyr::rename(study_name=`Study Name`),
                      "source_hawc_pfas_430" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                 "clowder_v3/hawc_pfas_430_doc_map_20230120.xlsx")),
                      "source_pfas_150_sem_v2" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                   "clowder_v3/pfas_150_sem_document_map_10032022_mmille16.xlsx")),
                      "source_hpvis" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                         "clowder_v3/source_hpvis_document_map_jwall01_20221129.xlsx")),
                      "source_oppt" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                            "clowder_v3/source_oppt_doc_map_20221206.xlsx")),
                      "source_efsa" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                        "clowder_v3/source_efsa_matched_mmille16_09212022.xlsx")),
                      "source_hawc" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                        "clowder_v3/hawc_original_matched_07072022_mmille16.xlsx")),
                      "source_pprtv_cphea" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                               "clowder_v3/pprtv_cphea_doc_map_mmille16.xlsx")),
                      data.frame()
    )

  }

  # Ensure clowder_id column is provided and/or standardized
  if(is.null(map_clowder_id_field) || is.na(map_clowder_id_field)){
    map_clowder_id_field <- "clowder_id"
    if(!"clowder_id" %in% names(map_file)){
      stop("Must provide a 'map_clowder_id_field' present in the provided 'map_file'...")
    }
  } else {
    # Rename custom ID field to common name
    map_file <- map_file %>%
      dplyr::rename(clowder_id = !!map_clowder_id_field)
  }

  # Filter documents to only new documents
  pushed_docs <- runQuery(paste0("SELECT clowder_id FROM documents where clowder_id in ('",
                       paste0(unique(map_file$clowder_id), collapse="', '"),
                       "')"),
                       db)

  mat <- map_file %>%
    filter(!clowder_id %in% pushed_docs$clowder_id) %>%
    select(clowder_id)

  if(nrow(mat)){
    message("...pushing ", nrow(mat), " new document entries...")
    # Push new document records to documents table
    runInsertTable(mat=mat,
                   table="documents",
                   db=db)
  } else {
    message("...no new document entries to push...moving on...")
  }

  if(sync_clowder_metadata){
    if(is.null(clowder_url) || is.null(clowder_api_key) ||
       is.na(clowder_url) || is.na(clowder_api_key)){
      cat("\nCannot sync clowder metadata without clowder_url and clowder_api_key...")
    } else {
      # TODO Implement function with this subset of new documents
      # doc_lineage_sync_clowder_metadata()
    }
  }

  # Pull source table document records
  pushed_docs <- runQuery(paste0("SELECT id as fk_doc_id, clowder_id FROM documents where clowder_id in ('",
                                 paste0(unique(map_file$clowder_id), collapse="', '"),
                                 "')"),
                          db)
  # Match records to documents table records
  map_file <- map_file %>%
    left_join(pushed_docs,
              by="clowder_id")

  if(!"parent_document_id" %in% names(map_file)){
    message("'parent_document_id' field not in map, cannot map document association lineage...skipping...")
  } else {
    # TODO Push document-to-document associations
  }

  # PUll source table data
  res <- runQuery(paste0("SELECT * FROM ", source_table), db=db)
  # TODO custom mapping source_hash to document ID
  # Map documents to source_hash records like normal
  res <- switch(source_table,

                "source_pprtv_cphea" = {
                  res <- res %>%
                    select(source_hash, name) %>%
                    left_join(map_file %>%
                                select(fk_doc_id, clowder_id, Chemical),
                              by=c("name"="Chemical"))

                  #Checking and outing cat statement
                  res2 <- res %>%
                    filter(is.na(clowder_id))
                  records_missing <- length(unique(res2$source_hash))
                  total_records <- length(unique(res$source_hash))
                  cat("Mapped records: ", total_records-records_missing, "| Missing: ", records_missing, "(", round(records_missing/total_records*100, 3),"%)\n")
                  # Return res
                  res
                },

                # Default case
                data.frame())

  if(!nrow(res)){
    message("...No source table data pulled...returning...")
    return()
  }

  # Filter documents_records to only new documents_records
  pushed_doc_records <- runQuery(paste0("SELECT source_hash, fk_doc_id FROM documents_records where source_hash in ('",
                                 paste0(unique(res$source_hash), collapse="', '"),
                                 "')"),
                          db) %>%
    tidyr::unite(col="pushed_docs", source_hash, fk_doc_id)

  mat <- res %>%
    tidyr::unite(col="pushed_docs", source_hash, fk_doc_id, remove=FALSE) %>%
    filter(!pushed_docs %in% pushed_doc_records$pushed_docs,
           !is.na(fk_doc_id)) %>%
    select(source_hash, fk_doc_id)

  if(nrow(mat)){
    message("...pushing ", nrow(mat), " new documents_records entries...")
    # Push new document records to documents table
    runInsertTable(mat=mat %>%
                     mutate(source_table = source_table),
                   table="documents_records",
                   db=db)
  } else {
    message("...no new documents_records entries to push...moving on...")
  }
}

