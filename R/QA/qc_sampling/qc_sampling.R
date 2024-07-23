#-----------------------------------------------------------------------------------
#' Sample ToxVal source tables
#'
#' @param toxval.db Database version
#' @param source.db Source database version
#' @param fraction Sampling fraction, default of 0.1 (10%)
#' @param source Name of source to sample
#' @param curation_method 'automated' or 'manual', Determines what sampling rule to use
#' @param data_profiling_file Path to input data profiling file to use for sampling
#' @param refresh_qc_pull Whether to reset the stored QC RData, toxval_for_qc_prioritization.RData
#' @param dir_output Custom output directory path. Default of "Repo/qc_sampling"
#' @param source_specific_filter Boolean whether to apply source specific filtering Default is TRUE.
#' @return sampled toxval_source records
#-----------------------------------------------------------------------------------
qc_sampling <- function(toxval.db,
                        source.db,
                        fraction=0.1,
                        source=NULL,
                        source_table=NULL,
                        curation_method=NULL,
                        data_profiling_file=NULL,
                        refresh_qc_pull=TRUE,
                        dir_output = "Repo/qc_sampling",
                        source_specific_filter=TRUE) {
  printCurrentFunction(toxval.db)
  # Check input params
  if(is.null(source) || is.na(source)) stop("Input param 'source' must not be NULL or NA.")
  if(is.null(curation_method) || is.na(curation_method)) stop("Input param 'curation_method' must not be NULL or NA.")
  if(!curation_method %in% c("automated", "manual")) stop("Input param 'curation_method' must be 'automated' or 'manual'")

  # Directory paths for QC sampling assets
  dir_input = "Repo/qc_sampling/sampling_input"

  # Get source table by input source name
  source_table = runQuery(query=paste0("SELECT source_table FROM chemical_source_index WHERE source = '", source,"'"),
                          db=source.db)[,1]
  # Get source record count
  if(source_table == "direct load"){
    src_n = runQuery(paste0("SELECT count(*) FROM toxval WHERE source = '", source, "'"),
                     db=toxval.db)[,1]
    # Base query
    query = paste0("SELECT source_hash, source FROM toxval ",
                   "WHERE source = '", source, "'")# AND qc_status = 'not determined'")
  } else {
    src_n <- runQuery(paste0("SELECT count(*) FROM ", source_table),
                      db=source.db)[,1]
    # Base query
    query = paste0("SELECT source_hash, source FROM ", source_table, " WHERE qc_status = 'not determined'")
  }

  if(source_specific_filter){
    # IRIS only sample IRIS Export data, not IRIS Summary, etc.
    if(source == "IRIS"){
      src_n <- runQuery(paste0("SELECT count(*) FROM ", source_table,
                               " WHERE document_type = 'IRIS Export'"),
                        db=source.db)[,1]
      query = paste0("SELECT source_hash, source FROM ", source_table,
                     " WHERE qc_status = 'not determined' ",
                     "AND document_type = 'IRIS Export'")
    }
  }

  # Check source records in need of QC
  src_tbl_to_qc <- runQuery(query=query,
                            db=ifelse(src_tbl == "direct load", toxval.db, source.db)) %>%
    dplyr::mutate(source_table = !!source_table)

  # Return wihtout sampling if no records to QC in source table
  if(!nrow(src_tbl_to_qc)){
    message("No source records with qc_status of 'not determined'...returning...")
    return()
  }

  ##############################################################################
  ### Sample by Record Type
  ##############################################################################
  # Pull data to sample by record type
  if(!exists("TOXVAL_ALL")) {
    file = file.path(dir_input,"toxval_for_qc_prioritization.RData")
    # Pull if refresh is TRUE or not previously pulled
    if(refresh_qc_pull | !file.exists(file)){
      export.for.toxvaldb.qc_prioritization(toxval.db)
    }
    load(file=file)
    TOXVAL_ALL <<- res
  }

  # Pull source specific data
  sub_res <- TOXVAL_ALL %>%
    dplyr::filter(source == !!source | source_table == !!source_table)

  if(grepl("IUCLID", source)){
    sub_res <- TOXVAL_ALL %>%
      dplyr::filter(source == "ECHA IUCLID", source_table == !!source_table)
  }

  # Sample by record type
  if(!nrow(sub_res)){
    message("Source '", source,"' not found in TOXVAL_ALL...skipping...")
    # Do not skip unless we want to...
    browser()
    sampled_records = data.frame()
  } else {
    # pull by record
    sampled_records <- prioritize.toxval.records(toxval.db=toxval.db, res=sub_res, fraction=fraction) %>%
      dplyr::distinct() %>%
      # Filter only to those needing QC
      dplyr::filter(source_hash %in% src_tbl_to_qc$source_hash)
  }

  ##############################################################################
  ### Sample by Data Profiling Flagged Records
  ##############################################################################
  # Add data profiling records based on curation_method, if sample limits aren't already met
  if((nrow(sampled_records) < 100 & curation_method == "automated") |
     ((nrow(sampled_records) < (src_n*0.2)) & curation_method == "manual")){

    # Sample from data profiling if provided
    if(!is.null(data_profiling_file) && !is.na(data_profiling_file)){
      if(!file.exists(data_profiling_file)){
        message("Input data_profiling_file '", data_profiling_file, "' not found...skipping...")
      } else {
        # Read provided data profiling file
        dp_flags = readxl::read_xlsx(data_profiling_file)
        # Filter to source
        dp_source <- dp_flags %>%
          dplyr::filter(source == !!source) %>%
          dplyr::select(source_hash, source_table)
        # Append data profiling records to sample
        sampled_records <- sampled_records %>%
          dplyr::bind_rows(dp_source) %>%
          dplyr::distinct() %>%
          # Filter only to those needing QC
          dplyr::filter(source_hash %in% src_tbl_to_qc$source_hash)
      }
    }
  }

  ##############################################################################
  ### Random Sample to Fill Threshold
  ##############################################################################
  # Sample additional input records if thresholds not met
  if((nrow(sampled_records) < 100 & curation_method == "automated") |
     ((nrow(sampled_records) < (src_n*0.2)) & curation_method == "manual")){
    slist = src_tbl_to_qc %>%
      dplyr::filter(!source_hash %in% sampled_records$source_hash)
    if(curation_method == "manual"){
      # Calc additional record count needed in 20% sample
      sample_diff = ceiling((src_n*0.2) - nrow(sampled_records))
    } else {
      # Calc additional record count needed to reach 100 records
      sample_diff = 100-nrow(sampled_records)
    }
    # Sample records to fill record thresholds
    cur_sample <- src_tbl_to_qc %>%
      dplyr::slice_sample(n = sample_diff)

    sampled_records <- sampled_records %>%
      dplyr::bind_rows(cur_sample) %>%
      dplyr::distinct()
  }

  # ##############################################################################
  # ### Sample Down if Over Threshold
  # ##############################################################################
  # if((nrow(sampled_records) > 100 & curation_method == "automated") |
  #    ((nrow(sampled_records) > (src_n*0.2)) & curation_method == "manual")){
  #   slist = src_tbl_to_qc %>%
  #     dplyr::filter(!source_hash %in% sampled_records$source_hash)
  #   if(curation_method == "manual"){
  #     # Sample threshold for manual
  #     sample_diff = ceiling(src_n*0.2)
  #   } else {
  #     # Sample threshold for automated
  #     sample_diff = 100
  #   }
  #   # Sample down records to meet threshold
  #   sampled_records <- sampled_records %>%
  #     dplyr::slice_sample(n = sample_diff) %>%
  #     dplyr::distinct()
  # }

  # Pull all data for sampled records
  if(source_table == "direct load"){
    full_source = runQuery(query=paste0(
      "SELECT b.raw_name, b.raw_casrn, a.* from toxval a ",
      "LEFT JOIN source_chemical b on a.chemical_id = b.chemical_id ",
      "WHERE source_hash in ('",
      paste0(sampled_records$source_hash, collapse="', '"),
      "')"),
      db=toxval.db)
    # Export QC Sample
    writexl::write_xlsx(full_source,
                        paste0(dir_output,"/toxval_qc_", source, "_",Sys.Date(),".xlsx"))
  } else {
    full_source = runQuery(query=paste0("SELECT * FROM ", source_table, " WHERE source_hash in ('",
                                        paste0(sampled_records$source_hash, collapse="', '"),
                                        "')"),
                           db=source.db)
    # Export QC Sample
    writexl::write_xlsx(full_source,
                        paste0(dir_output,"/toxval_qc_", source_table, "_",Sys.Date(),".xlsx"))
  }

  # Return sampled data
  return(full_source)

}
