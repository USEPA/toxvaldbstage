#--------------------------------------------------------------------------------------
#' Load the various IUCLID subsources into ToxVal
#' @param dir directory containing the various IUCLID subsource subdirectories
#' @return None, subsources loaded
#' @param db The version of toxval_source into which the source is loaded.
#' @param do.reset If TRUE, delete data from the database for this source before
#' inserting new data. Default FALSE
#' @param do.insert If TRUE, insert data into the database, default TRUE
#' @param chem.check.halt If TRUE, stop the execution if there are errors in the
#--------------------------------------------------------------------------------------

orchestrate_import_source_iuclid <- function(dir) { # TODO: figure out input/param stuff
  # Set the correct directory -- could be changed later
  dir <- "/ccte/ACToR1/ToxValDB9/Repo/iuclid"
  # Ensure the script is run in the correct directory
  if (getwd() != dir) {
    stop("Working directory must be ", dir)
  }
  # Loop through all subdirectories of current wd and load the source files within into ToxVal
  subdirs <- list.files()
  for (source in subdirs) {
    source_path <- paste0(source,"/",source,"_files",source,".csv")
    sql_path <- paste0(source,"/",source,"_MySQL/")
    res <- read.csv(source_path)
    source_prep_and_load(db=db,source="IUCLID",table=paste0("source_IUCLID_",source),
                         res=res,do.reset=FALSE,do.insert=FALSE,chem.check.halt=FALSE)
  }
}
