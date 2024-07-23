# library("openxlsx")
# library("stringr")
#--------------------------------------------------------------------------------------
#' @description Load ECHA Source from dev_toxval_source_v4(used in v8) saved as infile to dev_toxval_source_v4
#' @param toxval.db The version of toxval into which the source is loaded.
#' @param infile The input file ./echa/echa_files/echa_raw.xlsx
#' @title FUNCTION_TITLE
#' @param verbose PARAM_DESCRIPTION, Default: T
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[openxlsx]{read.xlsx}}
#' @rdname import_echa_source
#' @importFrom openxlsx read.xlsx

#--------------------------------------------------------------------------------------
import_echa_source <- function(toxval.db,infile, verbose = T) {
  printCurrentFunction(toxval.db)

  #####################################################################
  cat("Build echa2 table\n")
  #####################################################################

  res <- openxlsx::read.xlsx(infile, 1)
  # print(str(res))
  runInsertTable(res,"echa2",toxval.db,do.halt=T,verbose=F)

  #####################################################################
  cat("Build echa_chemical_information table from res\n")
  #####################################################################
  chemical_information <- res[,c("casrn","name")]
  chemical_information <- unique(chemical_information[,1:2])
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  chemical_information <- chemical_information[c('chemical_id','name','casrn')]

  runInsertTable(chemical_information,"echa_chemical_information",toxval.db,do.halt=T,verbose=F)

}
