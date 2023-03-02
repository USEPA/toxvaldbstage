#--------------------------------------------------------------------------------------
#' A generic tmpalte for adding data to toxval_source for a new source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param test_import If TRUE, save RData of import and return default False
#--------------------------------------------------------------------------------------
import_generic_source <- function(db,chem.check.halt=FALSE, test_import=FALSE) {
  printCurrentFunction(db)
  source = "name of the source"
  source_table = "source_{source}"
  dir = paste0(toxval.config()$datapath,"{source}/{source}_files/")
  file = paste0(dir,"name of the source file.xlsx")
  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  #
  # the final file should have column names that include "name" and "casrn"
  # additionally, the names in res need to match names in the source
  # database table. You do not need to add any of the generic columns
  # described in the SOP - they will get added in source_prep_and_load
  #

  # Standardize the names
  names(res0) <- names(res0) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  res = source.specific.transformations(res0)


  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db=db,
                       source=source,
                       table=source_table,
                       res=res,
                       do.reset=FALSE,
                       do.insert=TRUE,
                       chem.check.halt=TRUE,
                       test_import=test_import)
}




