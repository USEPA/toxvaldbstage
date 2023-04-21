#-------------------------------------------------------------------------------------
#'
#' Update the clowder links in toxval_source
#' @param db Database version of the source database
#' @param dir The directory where the update files sit
#-------------------------------------------------------------------------------------
correct.clowder.docs <- function(toxval.db,source.db,dir="updates_2022-08-09") {
  printCurrentFunction()
  dir = paste0(toxval.config()$datapath,"clowder_v3/",dir,"/")
  file = paste0(dir,"corrected_clowder_id_20220630.xlsx")
  mat = read.xlsx(file)
  for(i in 1:nrow(mat)) {
    dnm = mat[i,"document_name"]
    cid_wrong = mat[i,"wrong_clowder_id"]
    cid_right = mat[i,"corrected_clowder_id"]
  }
}
