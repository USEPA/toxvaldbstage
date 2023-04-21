#--------------------------------------------------------------------------------------
#' @#' Load OPPT Source Info into toxval_source
#' @param db The version of toxval_source into which the source info is loaded.
#' @param infile The input file ./oppt/oppt_files/OPPT_data_20181219.xlsx
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
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
#' @rdname import_oppt_source
#' @export 
#' @importFrom openxlsx read.xlsx
#--------------------------------------------------------------------------------------
import_oppt_source <- function(db,
                               infile="OPPT_data_20181219.xlsx",
                               chem.check.halt=T) {
  printCurrentFunction(db)
  infile = paste0(toxval.config()$datapath,"oppt/oppt_files/",infile)
  #####################################################################
  cat("Build original_oppt_table \n")
  #####################################################################
  res1 <- openxlsx::read.xlsx(infile)
  res1 = res1[res1$casrn!="NOCAS",]
  res1 = res1[res1$name!="NONAME",]
  #####################################################################
  cat("Prep and load the data\n")
  source_prep_and_load(db,source="EPA OPPT",table="source_oppt",res=res1,F,T,T)
}
