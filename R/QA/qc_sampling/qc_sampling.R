#-----------------------------------------------------------------------------------
#' Sample ToxVal source tables
#'
#' @param toxval.db Database version
#' @param source.db Source database version
#' @param sys.date Date string for ToxValDB export
#' @param curation_method 'automated' or 'manual', Determines what sampling rule to use
#' @return sampled toxval_source records
#-----------------------------------------------------------------------------------
qc_sampling <- function(toxval.db="res_toxval_v95",source.db='res_toxval_source_V5',fraction=0.1,source=NULL,curation_method=NULL) {
  printCurrentFunction(toxval.db)
  dir = "Repo/data/qc_prioritization/"
  dir_input = "Repo/data/qc_prioritization/input_files/"

  # get all data where qc_status is not determined
  if(!exists("TOXVAL_ALL")) {
    file = paste0(dir,"/toxval_for_qc_prioritization.RData")
    load(file=file)
    TOXVAL_ALL <<- res
  }
  source_name=source
  sub_res <- subset(TOXVAL_ALL, source == source_name)

  # pull by record
  sampled_records <- prioritize.toxval.records(toxval.db='res_toxval_v95',res=sub_res,fraction=0.1)
  sampled_records <- sampled_records %>% distinct(source_hash, .keep_all = TRUE)
  #stlist = sort(unique(sampled_records$source_table))

  # pull by data profile
  file = paste0(dir_input,"dp_flagged_records_2024-01-05.xlsx")
  dp_flags = read.xlsx(file)

  # Add data profiling records based on curation_method
  if((nrow(sampled_records) < 200 & curation_method == "automated") |
     (nrow(sampled_records) < (nrow(sub_res)*.2)) & curation_method == "manual"){
    dp_source <- subset(dp_flags, source == source_name)
    dp_final <- dp_source[, c("source_hash", "source_table")]
    sampled_records <- bind_rows(sampled_records, dp_final)
    sampled_records <- sampled_records %>% distinct(source_hash, .keep_all = TRUE)
  }


  # if necessary, pull randomly to reach thresholds by curation method


  source_table = unique(sampled_records$source_table)
  # pull source records and write out
  query = paste0("select * from ",source_table)
  full_source = runQuery(query, source.db)
  full_records = merge(x=sampled_records, y=full_source, by='source_hash', all.x=FALSE, all.y=FALSE)
  file = paste0(dir_input,"toxval_qc_", source_table, "_",Sys.Date(),".xlsx")
  write.xlsx(full_records, file)

}
