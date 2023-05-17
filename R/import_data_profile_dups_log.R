#--------------------------------------------------------------------------------------
#' Data Profiling Dups Log
#' Load Source Info into toxval source.
#' The information is in the file ./data_profile/data_profile_files/data_profiling_dups_log3.xlsx
#' @param db The version of toxval into which the source info is loaded.
#' @param dups_log_file The name of the duplicates log file to load
#' @export
#--------------------------------------------------------------------------------------
import.dup.log.info <- function(db, dups_log_file="True_Duplicate_for_DAT.xlsx") {
  printCurrentFunction("data_profile")
  # runInsert("data_profile",res_toxval_source_v5)

  dir = paste0(toxval.config()$datapath,"data_profile/data_profile_files/")
  file = paste0(dir, dups_log_file)
  # Load latest log
  res0 = readxl::read_xlsx(file)

  # Remove hash from duplicate if it is itself
  # res0 <- res0 %>%
  #   dplyr::rowwise() %>%
  #   dplyr::mutate(dup_of_source_hash_clean = dup_of_source_hash %>%
  #                   gsub(paste0(source_hash, ", "), "", fixed=TRUE))

  # Pull current database log of duplicates
  db_dups_log <- tryCatch({
    runQuery("SELECT * FROM data_profiling_dups_log", db)
  },
  # Error handling (such as if table doesn't exist)
  error = data.frame())

  if(nrow(db_dups_log)){

  }
  runInsertTable(res0,"data_profiling_dups_log",res_toxval_source_v5,do.halt=T,verbose=T)

}




