#-------------------------------------------------------------------------------------
#'
#' Load DSSTox if needed
#'
#' @param toxval.db The version of toxvaldb to use.
#' @param source.db The source database version
#' @param source The source to update for
#' @param verbose If TRUE, print out extra diagnostic messages
#-------------------------------------------------------------------------------------
load.dsstox <- function() {
  printCurrentFunction()

  if(!exists("DSSTOX")) {
    sys.date <- "2022-07-20"
    file = paste0(toxval.config()$datapath,"/DSSTox/DSSTox_",sys.date,".RData")
    load(file)
    dsstox = DSSTOX
    rownames(dsstox) = dsstox$casrn
    names(dsstox)[is.element(names(dsstox),"dsstox_substance_id")] <- "dtxsid"
    DSSTOX <<- dsstox
  }
}
