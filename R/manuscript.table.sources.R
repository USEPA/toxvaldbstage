#-------------------------------------------------------------------------------------
#'
#' Export the table of the sources for the manuscript
#' #' @param toxval.db Database version
#-------------------------------------------------------------------------------------
manuscript.table.sources <- function(toxval.db) {
  printCurrentFunction()
  mat = runQuery("select distinct source,description,origin_category, origin_agency, source_year, url from source_info",db)
  mat$chemicals = 0
  mat$records = 0

  for(i in 1:nrow(mat)) {
    source = mat[i,"source"]
    query = paste0("select count(*) from toxval where source='",source,"'")
    mat[i,"records"] = runQuery(query,db)[1,1]
    query = paste0("select count(distinct dtxsid) from toxval where source='",source,"'")
    mat[i,"chemicals"] = runQuery(query,db)[1,1]
  }
  dir = paste0(toxval.config()$datapath,"manuscript_data")

  file = paste0(dir,"/source table.xlsx")
  write.xlsx(mat,file)
  browser()
}
