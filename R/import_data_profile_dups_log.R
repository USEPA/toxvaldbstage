#--------------------------------------------------------------------------------------
#' Data Profiling Dups Log
#' Load Source Info into toxval source. 
#' The information is in the file ./data_profile/data_profile_files/data_profiling_dups_log3.xlsx
#' @param res_toxval_source_v5 The version of toxval into which the source info is loaded.
#' @export
#--------------------------------------------------------------------------------------
import.dup.log.info <- function(res_toxval_source_v5) {
  printCurrentFunction(res_toxval_source_v5)
  runInsert("data_profile",res_toxval_source_v5)
  
  source = "Data Profiling Dups Log"
  source_table = "data_profile"
  dir = paste0(toxval.config()$datapath,"data_profile/data_profile_files/")
  file = paste0(dir,"True_Duplicate_for_DAT.xlsx")
  res0 = readxl::read_xlsx(file)
 
  
  
  runInsertTable(res0,"data_profiling_dups_log",res_toxval_source_v5,do.halt=T,verbose=T)
  
}




