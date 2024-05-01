#--------------------------------------------------------------------------------------
#' @#' Data Profiling Dups Log
#' Load Source Info into toxval source.
#' The information is in the file ./data_profile/data_profile_files/data_profiling_dups_log3.xlsx
#' @param db The version of toxval into which the source info is loaded.
#' @param dups_log_file The name of the duplicates log file to load
#' @export 
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [read_xlsx][readxl::read_xlsx]
#'  [separate_rows][tidyr::separate_rows], [unite][tidyr::unite]
#'  [filter][dplyr::filter], [select][dplyr::select]
#' @rdname import_data_profile_dups_log
#' @importFrom readxl read_xlsx
#' @importFrom tidyr separate_rows unite
#' @importFrom dplyr filter select
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
    dplyr::filter(source_hash != dup_of_source_hash) %>%
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
      dplyr::filter(!dup_key %in% db_dups_log$dup_key)
  }

  # Only attempt insert if records present to push
  if(nrow(res)){
    runInsertTable(mat=res %>%
                     dplyr::select(-dup_key),
                   table="data_profiling_dups_log",
                   db=db,
                   do.halt=TRUE,
                   verbose=TRUE)
  }
}




