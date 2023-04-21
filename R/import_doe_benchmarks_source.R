#--------------------------------------------------------------------------------------
#' @description Load doe_benchmarks Source into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile The input file ./doe_benchmarks/doe_benchmarks_files/DOE_Wildlife_Benchmarks_1996.xlsx
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#' @title FUNCTION_TITLE
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
#' @rdname import_doe_benchmarks_source
#' @export 
#' @importFrom openxlsx read.xlsx
#--------------------------------------------------------------------------------------
import_doe_benchmarks_source <- function(db,
                                         infile="DOE_Wildlife_Benchmarks_1996.xlsx",
                                         chem.check.halt=F) {
  printCurrentFunction(db)
  infile = paste0(toxval.config()$datapath,"doe_benchmarks/doe_benchmarks_files/",infile)
  #####################################################################
  cat("Build original_doe_benchmarks_table \n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile ,1,colNames = T)
  names(res) = c("source_name_sid","casrn","name","url",
                 "data_collection","source_name_cid","analyte","form",
                 "test_species","test_species_noael","test_species_loael",
                 "endpoint_species",
                 "wildlife_noael","noael_food","noael_water","noael_piscivore",
                 "wildlife_loael","loael_food","loael_water","loael_piscivore" )

  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="DOE Wildlife Benchmarks",table="source_doe_benchmarks",res=res,F,T,T)
}
