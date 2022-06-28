#--------------------------------------------------------------------------------------
#' Export the source chemical table
#' @param db The name of the database String to be cleaned
#' @param dir The directory where the file will be saved
#'
#--------------------------------------------------------------------------------------
export.source_chemical <- function(db,dir="../source_chemical/") {
  printCurrentFunction(db)
  res = runQuery("select * from source_chemical",db)
  res = res[res$raw_name!=res$cleaned_name,]
  file = paste0(dir,"source_chemical ",Sys.Date(),".xlsx")
  openxlsx::write.xlsx(res,file)
}
