#-------------------------------------------------------------------------------------
#' @title toxval_source_status_report
#' @description Generate a report of sources in toxval_source. Includes general stats,
#' chemical curation stats, document catloging, and QC stats.
#' @param source.db The source database name
#' @param toxval.db The database version to use
#' @return List of report dataframes. Export XLSX file is also produced.
#' @import RMySQL dplyr readxl magrittr
#--------------------------------------------------------------------------------------
toxval_source_status_report <- function(source.db, toxval.db){

  # Pull chemical index table
  chem_index = runQuery("SELECT * FROM chemical_source_index",
                        db=source.db) %>%
    select(chemprefix, everything()) %>%

    # Alter chem_index to allow for direct load joins
    dplyr::mutate(
      source_table = dplyr::case_when(
        source_table %in% c("direct load", "direct_load") ~ source,
        TRUE ~ source_table
      )
    )

  # List of all source tables
  tblList = runQuery(query = paste0("SHOW TABLES FROM ", db=source.db),
                     db=source.db) %>% unlist() %>% unname() %>%
    # Filter to those named "source_*"
    .[grepl("source_", .)] %>%
    # Ignore those like source_audit or source_chemical
    .[!grepl("chemical|source_audit|_old|documents", .)]

  ################################################################################
  ### General stats
  ################################################################################
  cat("\n...Generating general stats")
  gen_stats = lapply(chem_index$source_table[!is.na(chem_index$source_table)], function(s_tbl){
    # Check if table exists
    direct_load = FALSE
    s_check = runQuery(paste0("SHOW TABLES LIKE '", s_tbl, "'"), db=source.db)
    if(!nrow(s_check)) {
      s_check = runQuery(paste0("SELECT DISTINCT source FROM toxval WHERE source='", s_tbl, "'"), toxval.db)
      direct_load = TRUE
    }
    if(!nrow(s_check)) return(NULL)
    # Pull data for existing table
    if(direct_load) {
      direct_query = paste0("SELECT chemical_id, datestamp AS create_time, qc_status FROM toxval WHERE source='", s_tbl, "'")
      s_dat = runQuery(direct_query, toxval.db)
      min_create = min_create = min(s_dat$create_time)
    } else {
      s_dat = runQuery(paste0("SELECT chemical_id, create_time, qc_status FROM ", s_tbl), db=source.db)
      s_audit = runQuery(paste0("SELECT MIN(create_time) as initial_import FROM source_audit where src_tbl_name = '", s_tbl,"'"), db=source.db)
      min_create = s_audit$initial_import
      if(is.na(min_create)) min_create = min(s_dat$create_time)
    }

    tmp = s_dat %>%
      dplyr::summarise(n_records = n(),
                       n_chems = length(unique(chemical_id[!is.na(chemical_id)]))) %>%
      dplyr::mutate(source_table = !!s_tbl,
                    first_uploaded = !!min_create)

    tmp$last_updated = s_dat$create_time[s_dat$qc_status == "not determined"] %>%
      max(na.rm=TRUE)

    return(tmp)
  }) %>%
    dplyr::bind_rows() %>%
    left_join(x=chem_index, y=., by="source_table") %>%
    dplyr::mutate(across(c(n_records, n_chems), ~tidyr::replace_na(., 0)))

  ################################################################################
  ### Get chemical curation stats
  ################################################################################
  cat("\n...Generating chemical curation stats")
  chem_stats = lapply(chem_index$source_table[!is.na(chem_index$source_table)], function(s_tbl){
    # Check if table exists
    direct_load = FALSE
    query = paste0("SELECT source, dtxsid, dtxrid FROM source_chemical ",
                   "WHERE chemical_id ",
                   "IN (SELECT chemical_id FROM ", s_tbl, ")")
    db = source.db
    s_check = runQuery(paste0("SHOW TABLES LIKE '", s_tbl, "'"), db=source.db)
    if(!nrow(s_check)) {
      s_check = runQuery(paste0("SELECT DISTINCT source FROM toxval WHERE source='", s_tbl, "'"), toxval.db)
      query = paste0("SELECT source, dtxsid, dtxrid FROM source_chemical ",
                     "WHERE chemical_id ",
                     "IN (SELECT chemical_id FROM toxval WHERE source='", s_tbl,
                     "' OR source_table='", s_tbl, "')")
      db = toxval.db
    }
    if(!nrow(s_check)) return(NULL)
    # Pull source_chemical data for existing table by chemical_id
    runQuery(query, db=db) %>%
      return()
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(source) %>%
    dplyr::summarise(total_chems = n(),
                     dtxrid = length(unique(dtxrid[!is.na(dtxrid)])),
                     dtxsid = length(dtxsid[!is.na(dtxsid)])) %>%
    get_percent_summary("perc_dtxsid", "dtxsid", "total_chems") %>%
    get_percent_summary("perc_dtxrid", "dtxrid", "total_chems") %>%
    left_join(x=chem_index, y=., by="source") %>%
    dplyr::mutate(across(c("total_chems", "dtxrid", "dtxsid", "perc_dtxsid", "perc_dtxrid"),
                         ~tidyr::replace_na(., 0)))

  ################################################################################
  ### Get QC Stats
  ################################################################################
  cat("\n...Generating QC stats")
  qc_stats = lapply(chem_index$source_table[!is.na(chem_index$source_table)], function(s_tbl){
    # Check if table exists
    direct_load = FALSE
    query = paste0("SELECT qc_status FROM ", s_tbl)
    db = source.db
    s_check = runQuery(paste0("SHOW TABLES LIKE '", s_tbl, "'"), db=source.db)
    if(!nrow(s_check)) {
      s_check = runQuery(paste0("SELECT DISTINCT source FROM toxval WHERE source='", s_tbl, "'"), toxval.db)
      query = paste0("SELECT qc_status FROM toxval WHERE source='", s_tbl, "' OR source_table='", s_tbl, "'")
      db = toxval.db
    }
    if(!nrow(s_check)) return(NULL)
    # Pull QC data for current source
    runQuery(query, db=db) %>%
      dplyr::summarise(total_records = n(),
                       total_qc = sum(qc_status != "not determined"),
                       pass = sum(qc_status %in% c("pass", "PASS")),
                       fail = sum(grepl("fail", qc_status, ignore.case=TRUE)),
                       pending = sum(qc_status == "not determined"),
                       qc_perc = round(total_qc / total_records * 100, 3)
      ) %>%
      dplyr::mutate(source_table = s_tbl) %>%
      return()
  }) %>%
    dplyr::bind_rows() %>%
    get_percent_summary("perc_pass", "pass", "total_qc") %>%
    get_percent_summary("perc_fail", "fail", "total_qc") %>%
    left_join(x=chem_index, y=., by="source_table") %>%
    dplyr::mutate(across(c(-chemprefix, -source, -source_table, -source_status, -curation_type), ~tidyr::replace_na(., 0)))

  ################################################################################
  ### Get Document Cataloging/Lineage Stats
  ################################################################################
  cat("\n...Generating document cataloging stats")
  # Check all required document lineage tables are present
  doc_lineage_check <- runQuery(query = paste0("SHOW TABLES FROM ", db=source.db),
                                db=source.db) %>%
    .[[1]] %>%
    .[grepl("documents", .)]

  doc_lineage_tbls <- c("documents", "documents_lineage", "documents_records")

  doc_lineage_check <- doc_lineage_tbls[!doc_lineage_tbls %in% doc_lineage_check]

  if(length(doc_lineage_check)){
    message("Missing document lineage table: ", paste0(doc_lineage_check, collapse = ", "))
    doc_cat_stats = data.frame()
  } else {
    # Generate stats once the document lineage schema is established
    doc_cat_stats <- runQuery(paste0("SELECT distinct a.source_hash, a.source_table, b.document_type ",
                                     "FROM documents_records a ",
                                     "LEFT JOIN documents b on a.fk_doc_id = b.id"), db=source.db)

    # Filter to only source_hash values in the source tables (can be orphan linkages due to archived tables not
    # updated in the document_records table...)
    doc_cat_stats = lapply(unique(doc_cat_stats$source_table), function(s_tbl){
      # Check if table exists
      if(s_tbl %in% c("ChemIDPlus", "Uterotrophic Hershberger DB", "ToxRefDB", "ECOTOX")){
        hashes = runQuery(paste0("SELECT source_hash FROM toxval WHERE source = '", s_tbl, "'"), db=toxval.db)
      } else {
        s_check = runQuery(paste0("SHOW TABLES LIKE '", s_tbl, "'"), db=source.db)
        if(!nrow(s_check)) return(NULL)
        # Pull data for existing table
        hashes = runQuery(paste0("SELECT DISTINCT source_hash FROM ", s_tbl), db=source.db)
      }

      doc_cat_stats %>%
        dplyr::filter(source_table == s_tbl,
                      source_hash %in% hashes$source_hash) %>%
        return()
    }) %>%
      dplyr::bind_rows()

    # Summarize
    doc_cat_stats = doc_cat_stats %>%
      dplyr::group_by(source_table, document_type) %>%
      dplyr::summarise(recs_w_docs = n()) %>%
      dplyr::ungroup() %>%
      left_join(x=chem_index, y=., by="source_table") %>%
      left_join(gen_stats %>% select(source_table, n_records) %>% distinct(),
                by="source_table") %>%
      get_percent_summary(col="perc_w_docs", num="recs_w_docs", den="n_records") %>%
      dplyr::mutate(across(c(-chemprefix, -source, -source_table, -source_status, -document_type, -curation_type), ~tidyr::replace_na(., 0))) %>%
      dplyr::arrange(source, document_type)
  }

  ################################################################################
  ### Check chemical index versus source table usage
  ################################################################################
  cat("\n...Generating chemical index check stats")
  # Pull Chemical indexes in use in source tables
  src_table = lapply(tblList, function(s_tbl){
    # Check if table exists
    s_check = runQuery(paste0("SHOW TABLES LIKE '", s_tbl, "'"), db=source.db)
    if(!nrow(s_check)) return(NULL)
    # Pull data for existing table
    row = runQuery(paste0("SELECT * FROM ", s_tbl, " LIMIT 1"),
                   db=source.db)
    if("chemical_id" %in% names(row)){
      row = row %>%
        select(chemical_id) %>%
        dplyr::mutate(source_table = s_tbl,)
      return(row)
    }
    return(NULL)
  }) %>%
    purrr::compact() %>%
    dplyr::bind_rows() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(chemical_id = strsplit(chemical_id, split="_") %>%
                    unlist() %>%
                    .[1]) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(chem_index = gsub("[^0-9.-]", "", chemical_id) %>%
                    as.numeric()) %>%
    # Required for later filtering avoiding "$ operator invalid for atomic vectors" error
    as.data.frame()

  ################################################################################
  ### Check chemical index versus source_chemical table usage
  ################################################################################
  # Pull chemical indexes in use in chemical table
  curated_chems = runQuery(paste0("SELECT chemical_id, source FROM source_chemical"),
                           db=source.db) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(chemical_id = strsplit(chemical_id, split="_") %>%
                    unlist() %>%
                    .[1]) %>%
    dplyr::ungroup() %>%
    distinct()

  # Check indexes not in curated chemicals
  index_no_chems = chem_index %>%
    dplyr::filter(!chemprefix %in% curated_chems$chemical_id)
  # Check indexes without source table
  index_no_table = chem_index %>%
    dplyr::filter(!chemprefix %in% src_table$chemical_id)

  cat("\n...exporting")
  # Compile report for export
  out = list(gen_stats = gen_stats,
             chem_curation = chem_stats,
             doc_cat_stats = doc_cat_stats,
             QC = qc_stats,
             index_no_table = index_no_table,
             index_no_chems = index_no_chems) %T>%
    writexl::write_xlsx(paste0("Repo/toxval_source_status_reports/toxval_source_status_",
                               Sys.Date(), ".xlsx"))

  # Return report
  return(out)
}

#' @title get_percent_summary
#' @description Helper function to calculate percentages based on input dataframe and fields
#' @return Modified dataframe with new percentage 'col' based on 'num' divided by 'den'
get_percent_summary <- function(df, col, num, den, n_digits=3){
  df[[col]] = round(df[[num]] / df[[den]] * 100, digits = n_digits)
  # Replace "Inf" with NA
  df[sapply(df, is.infinite)] <- NA
  return(df)
}
