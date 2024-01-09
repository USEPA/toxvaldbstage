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
#' @return sampled toxval_source records
#-----------------------------------------------------------------------------------
qc_sampling <- function(toxval.db="res_toxval_v95",
                        source.db="res_toxval_source_V5",
                        fraction=0.1,
                        source=NULL,
                        curation_method=NULL,
                        data_profiling_file=NULL,
                        refresh_qc_pull=TRUE) {
  printCurrentFunction(toxval.db)
  # Check input params
  if(is.null(source) || is.na(source)) stop("Input param 'source' must not be NULL or NA.")
  if(is.null(curation_method) || is.na(curation_method)) stop("Input param 'curation_method' must not be NULL or NA.")
  if(!curation_method %in% c("automated", "manual")) stop("Input param 'curation_method' must be 'automated' or 'manual'")

  # Directory paths for QC sampling assets
  dir_input = "Repo/qc_sampling/sampling_input"
  dir_output = "Repo/qc_sampling"

  # Get source table by input source name
  source_table = runQuery(query=paste0("SELECT source_table FROM chemical_source_index WHERE source = '", source,"'"),
                          db=source.db)[,1]
  # Get source record count
  src_n <- runQuery(paste0("SELECT count(*) FROM ", source_table),
                    db=source.db)[,1]

  # Check source records in need of QC
  src_tbl_to_qc <- runQuery(query=paste0("SELECT source_hash, source FROM ", source_table, " WHERE qc_status = 'not determined'"),
                            db=source.db) %>%
    dplyr::mutate(source_table = !!source_table)

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
    dplyr::filter(source == !!source)

  # Sample by record type
  if(!nrow(sub_res)){
    message("Source '", source,"' not found in TOXVAL_ALL...skipping...")
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
    } else{
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

  # pull source records
  full_source = runQuery(query=paste0("SELECT * FROM ", source_table, " WHERE source_hash in ('",
                                      paste0(sampled_records$source_hash, collapse="', '"),
                                      "')"),
                         db=source.db)
  # Export QC Sample
  writexl::write_xlsx(full_source,
                      paste0(dir_output,"/toxval_qc_", source_table, "_",Sys.Date(),".xlsx"))
  # Return sampled data
  return(full_source)

}
