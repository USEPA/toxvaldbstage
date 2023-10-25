#-------------------------------------------------------------------------------------
#' @title toxval_source_status_report
#' @description Generate a report of sources in toxval_source. Includes general stats,
#' chemical curation stats, document catloging, and QC stats.
#' @param db The version of toxval source database to use.
#' @return List of report dataframes. Export XLSX file is also produced.
#' @import RMySQL dplyr readxl magrittr
#--------------------------------------------------------------------------------------
toxval_source_status_report <- function(db){

  # Pull chemical index table
  chem_index = runQuery("SELECT * FROM chemical_source_index",
                        db) %>%
    select(chemprefix, everything())

  # List of all source tables
  tblList = runQuery(query = paste0("SHOW TABLES FROM ", db),
                     db=db) %>% unlist() %>% unname() %>%
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
    s_check = runQuery(paste0("SHOW TABLES LIKE '", s_tbl, "'"), db)
    if(!nrow(s_check)) return(NULL)
    # Pull data for existing table
    s_dat = runQuery(paste0("SELECT chemical_id, create_time, qc_status FROM ", s_tbl), db=db)
    s_audit = runQuery(paste0("SELECT MIN(create_time) as initial_import FROM source_audit where src_tbl_name = '", s_tbl,"'"), db=db)

    if(is.na(s_audit$initial_import)) {
      s_audit$initial_import = min(s_dat$create_time)
    }
    tmp = s_dat %>%
      dplyr::summarise(n_records = n(),
                       n_chems = length(unique(chemical_id)),
                       first_uploaded = s_audit$initial_import) %>%
      dplyr::mutate(source_table = s_tbl)

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
    s_check = runQuery(paste0("SHOW TABLES LIKE '", s_tbl, "'"), db)
    if(!nrow(s_check)) return(NULL)
    # Pull source_chemical data for existing table by chemical_id
    runQuery(paste0("SELECT source, dtxsid, dtxrid FROM source_chemical ",
                             "WHERE chemical_id in (SELECT chemical_id FROM ", s_tbl, ")"), db) %>%
      return()
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(source) %>%
    dplyr::summarise(total_chems = n(),
                     dtxrid = length(unique(dtxrid)),
                     dtxsid = length(unique(dtxsid))) %>%
    get_percent_summary("perc_dtxsid", "dtxsid", "total_chems") %>%
    get_percent_summary("perc_dtxrid", "dtxrid", "total_chems") %>%
    left_join(x=chem_index, y=., by="source") %>%
    dplyr::mutate(across(c(-chemprefix, -source, -source_table), ~tidyr::replace_na(., 0)))

  ################################################################################
  ### Get QC Stats
  ################################################################################
  cat("\n...Generating QC stats")
  qc_stats = lapply(chem_index$source_table[!is.na(chem_index$source_table)], function(s_tbl){
    # Check if table exists
    s_check = runQuery(paste0("SHOW TABLES LIKE '", s_tbl, "'"), db)
    if(!nrow(s_check)) return(NULL)
    # Pull data for existing table
    runQuery(paste0("SELECT qc_status FROM ", s_tbl), db=db) %>%
      dplyr::summarise(total_records = n(),
                       total_qc = sum(qc_status != "not determined"),
                       pass = sum(qc_status %in% c("pass", "PASS")),
                       fail = sum(qc_status %in% c("fail", "FAIL")),
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
    dplyr::mutate(across(c(-chemprefix, -source, -source_table), ~tidyr::replace_na(., 0)))

  ################################################################################
  ### Get Document Cataloging/Lineage Stats
  ################################################################################
  cat("\n...Generating document cataloging stats")
  # Check all required document lineage tables are present
  doc_lineage_check <- runQuery(query = paste0("SHOW TABLES FROM ", db),
                                db=db) %>%
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
                                     "LEFT JOIN documents b on a.fk_doc_id = b.id"), db)

    # Filter to only source_hash values in the source tables (can be orphan linkages due to archived tables not
    # updated in the document_records table...)
    doc_cat_stats = lapply(unique(doc_cat_stats$source_table), function(s_tbl){
      # Check if table exists
      s_check = runQuery(paste0("SHOW TABLES LIKE '", s_tbl, "'"), db)
      if(!nrow(s_check)) return(NULL)
      # Pull data for existing table
      hashes = runQuery(paste0("SELECT DISTINCT source_hash FROM ", s_tbl), db)
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
      dplyr::mutate(across(c(-chemprefix, -source, -source_table, -source_status, -document_type), ~tidyr::replace_na(., 0))) %>%
      dplyr::arrange(source, document_type)
  }

  ################################################################################
  ### Check chemical index versus source table usage
  ################################################################################
  cat("\n...Generating chemical index check stats")
  # Pull Chemical indexes in use in source tables
  src_table = lapply(tblList, function(s_tbl){
    # Check if table exists
    s_check = runQuery(paste0("SHOW TABLES LIKE '", s_tbl, "'"), db)
    if(!nrow(s_check)) return(NULL)
    # Pull data for existing table
    row = runQuery(paste0("SELECT * FROM ", s_tbl, " LIMIT 1"),
                   db)
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
                           db) %>%
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
