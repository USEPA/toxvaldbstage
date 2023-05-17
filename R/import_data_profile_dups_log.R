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

  # Separate into individual hash pairs
  res <- res0 %>%
    tidyr::separate_rows(dup_of_source_hash, sep=", ") %>%
    # Remove hash from duplicate if it is itself
    filter(source_hash != dup_of_source_hash) %>%
    tidyr::unite(col="dup_key", source_hash, dup_of_source_hash, remove=FALSE)

  # Pull current database log of duplicates
  db_dups_log <- tryCatch({
    runQuery("SELECT * FROM data_profiling_dups_log", db) %>%
      tidyr::unite(col="dup_key", source_hash, dup_of_source_hash, remove=FALSE)
    },
    # Error handling (such as if table doesn't exist)
    error = data.frame()
  )

  # Filter out pairs that are already present (same dup_key pair)
  if(nrow(db_dups_log)){
    res <- res %>%
      filter(!dup_key %in% db_dups_log$dup_key)
  }

  # Only attempt insert if records present to push
  if(nrow(res)){
    runInsertTable(mat=res %>%
                     select(-dup_key),
                   table="data_profiling_dups_log",
                   db=db,
                   do.halt=TRUE,
                   verbose=TRUE)
  }
}




