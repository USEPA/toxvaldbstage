#-------------------------------------------------------------------------------------
#'
#' Export the table of the sources for the manuscript
#' #' @param toxval.db Database version
#-------------------------------------------------------------------------------------
export.clowder.status <- function(toxval.db,source.db) {
  printCurrentFunction()
  mat = runQuery("select source,source_table from source_info",db)
  mat = unique(mat)
  mat = mat[!is.na(mat$source_table),]
  mat = mat[mat$source_table!="-",]
  mat$records = 0
  mat$w.clowder_id = 0
  mat$w.document_name = 0
  mat$status = NA
  for(i in 1:nrow(mat)) {
    table = mat[i,"source_table"]
    mat[i,"records"] = runQuery(paste0("select count(*) from ",table),source.db)[1,1]
    mat[i,"w.clowder_id"] = runQuery(paste0("select count(*) from ",table," where clowder_id!='-'"),source.db)[1,1]
    mat[i,"w.document_name"] = runQuery(paste0("select count(*) from ",table," where document_name!='-'"),source.db)[1,1]
    if(mat[i,"w.document_name"]==mat[i,"records"] && mat[i,"w.clowder_id"]==mat[i,"records"]) mat[i,"status"] = "complete"
    else if(mat[i,"w.document_name"]>0 || mat[i,"w.clowder_id"]>0) mat[i,"status"] = "partial"
  }
  dir = paste0(toxval.config()$datapath,"clowder_v3/")
  browser()
  file = paste0(dir,"toxval_clowder_status ",Sys.Date(),".xlsx")
  write.xlsx(mat,file)

}
