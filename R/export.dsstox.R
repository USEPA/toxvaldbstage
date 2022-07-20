#-------------------------------------------------------------------------------------
#'
#' Export the DSSTox chemical table
#-------------------------------------------------------------------------------------
export.dsstox <- function() {
  printCurrentFunction()
  dsstox.db <- toxval.config()$dsstox.db
  DSSTOX <- runQuery("select * from generic_substances",dsstox.db)
  file <- paste0(toxval.config()$datapath,"/DSSTox/DSSTox_",Sys.Date(),".RData")
  save(DSSTOX,file=file)
}
