library("openxlsx")
library('dplyr')
library('stringr')
library('data.table')
library('tidyr')
#--------------------------------------------------------------------------------------
#' Load ECHA echemportal api Source into dev_toxval_source_v4. 
#' @param toxval.db The version of toxval into which the source is loaded.
#' @param filepath The path for all the input xlsx files ./echa_echemportal_api/echa_echemportal_api_files
#--------------------------------------------------------------------------------------
import_echa_echemportal_api_source <- function(toxval.db,filepath) {
  printCurrentFunction(toxval.db)
  
  #####################################################################
  cat("Build echa_echemportal_api_original table\n")
  #####################################################################
  
  files.list <- list.files(path = filepath, pattern = "*.xlsx")
  #print(files.list)

  any_temp_files <- grep("^\\~\\$.*", files.list, value = T)
  files.list <- files.list[! files.list %in% any_temp_files]
  files.list <- paste0(filepath, "/", files.list)
  res <- lapply(files.list,read.xlsx)
  names(res) <- gsub("(.*\\/eChemPortalAPI_)(.*)(_.*)", "\\2", files.list)
  for (i in 1:length(res)) {
    res[[i]] <- lapply(res[[i]], function(x) type.convert(as.character(x), as.is = T))
    res[[i]] <- data.frame(res[[i]], stringsAsFactors = F)
  }

  # res[[16]] and res[[18]] has no data in excel files in latest data retrieval on 9-14-21, cause identified as echa portal changes

  for (i in c(1:15,17,19:26)) {
    res[[i]][sapply(res[[i]], function(x) all(is.na(x) == T))] <- ""
  }


  res1 <- rbindlist(res, fill = TRUE, idcol = 'source_table')
  res1 <- data.frame(res1, stringsAsFactors = F)

  res1$Name <- enc2utf8(res1$Name)
  names(res1) <- tolower(names(res1))
  res1$year <- res1$years
  
  res1 <- res1[ , !(names(res1) %in% c("years"))]
  res1$experimental.value <- enc2utf8(res1$experimental.value)
  names(res1) <- gsub("\\.+","_", names(res1))

  
  runInsertTable(res1,"original_echa_echemportal_api",toxval.db,do.halt=T,verbose=F)
  
}
