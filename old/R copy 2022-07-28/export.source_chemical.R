#--------------------------------------------------------------------------------------
#' Export the source chemical table
#' @param db The name of the database String to be cleaned
#'
#--------------------------------------------------------------------------------------
export.source_chemical <- function(db) {
  printCurrentFunction(db)
  dir = paste0(toxval.config()$datapath,"source_chemical/")
  res = runQuery("select * from source_chemical",db)
  res = res[res$raw_name!=res$cleaned_name,]
  file = paste0(dir,"source_chemical ",Sys.Date(),".xlsx")
  openxlsx::write.xlsx(res,file)
}
