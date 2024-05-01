#--------------------------------------------------------------------------------------
#' @title import_source_iuclid_orchestrate
#' @description Load the various IUCLID subsources into ToxVal
#' @param dir directory containing the various IUCLID subsource subdirectories
#' @param db The version of toxval_source into which the source is loaded.
#' @param do.insert If TRUE, insert data into the database, default TRUE
#' @param chem.check.halt If TRUE, stop the execution if there are errors in the
#' @return None, subsources loaded
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname import_source_iuclid_orchestrate
#' @export 
#--------------------------------------------------------------------------------------

import_source_iuclid_orchestrate <- function(dir=paste0(toxval.config()$datapath, "iuclid")) {
  # Loop through all subdirectories of current wd and load the source files within into ToxVal
  subdirs <- list.files(dir, pattern="iuclid") %>%
    .[!. %in% c("iuclid_code")]

  # Check against already imported IUCLID tables
  iuclid_imported = runQuery("SHOW TABLES", db) %>%
    unlist() %>% unname() %>%
    .[grepl("iuclid", .)] %>%
    unique()

  for (subf in subdirs) {
    # Skip already imported
    if(paste0("source_", subf) %in% iuclid_imported) next

    message("Pushing: ", subf)
    import_source_iuclid(db=db,
                         subf=subf,
                         chem.check.halt=FALSE,
                         do.reset=FALSE,
                         do.insert=TRUE)
  }
}
