#--------------------------------------------------------------------------------------
#' A generic template for adding data to toxval_source for a new source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param subf The subfolder containing the IUCLID subsource
#--------------------------------------------------------------------------------------
import_source_iuclid <- function(db, subf, chem.check.halt=F) {
  printCurrentFunction(db)
  source = paste0("IUCLID_", subf)
  source_table = paste0("source_iuclid_", subf) %>% tolower()
  dir = paste0(toxval.config()$datapath,"iuclid/",subf,"/",subf,"_files/")
  file = list.files(dir, pattern=".xlsx", full.names = TRUE)
  if(length(file) > 1) stop("More than 1 IUCLID file stored in '", dir, "'")
  res = readr::read_xlsx(file)

  if(!nrow(res)){
    return("...No rows found in file...skipping")
  }
  # Rename chemical identifier columns to fit ToxVal chemical cleaning
  res = res %>%
    dplyr::rename(name = DossSubstanceName, casrn = SubstanceCAS)
  # Standardize the names
  names(res) <- names(res) %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    stringr::str_squish() %>%
    tolower()

  #####################################################################
  cat("Load the data\n")
  #####################################################################
  source_prep_and_load(db,source=source,table=source_table,res=res,
                       do.reset=FALSE,do.insert=TRUE,chem.check.halt=TRUE)
}

#--------------------------------------------------------------------------------------
#' Load the various IUCLID subsources into ToxVal
#' @param dir directory containing the various IUCLID subsource subdirectories
#' @param db The version of toxval_source into which the source is loaded.
#' @param do.insert If TRUE, insert data into the database, default TRUE
#' @param chem.check.halt If TRUE, stop the execution if there are errors in the
#' @return None, subsources loaded
#--------------------------------------------------------------------------------------

orchestrate_import_source_iuclid <- function(dir="Repo/iuclid") {
  # Loop through all subdirectories of current wd and load the source files within into ToxVal
  subdirs <- list.files(dir)
  for (subf in subdirs) {
    import_source_iuclid(db, subf, chem.check.halt = FALSE)
  }
}
