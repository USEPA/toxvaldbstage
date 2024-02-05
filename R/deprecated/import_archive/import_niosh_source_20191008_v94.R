#--------------------------------------------------------------------------------------
#' @description Load NIOSH Source into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile The input file ./niosh/niosh_files/niosh_IDLH_2020.xlsx
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
#' @rdname import_niosh_source
#' @export 
#' @importFrom openxlsx read.xlsx
#--------------------------------------------------------------------------------------
import_niosh_source <- function(db,
                                infile="niosh_IDLH_2020.xlsx",
                                chem.check.halt=T) {
  printCurrentFunction(db)

  infile = paste0(toxval.config()$datapath,"niosh/niosh_files/",infile)
  #####################################################################
  cat("Build new_niosh table\n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile)
  res["niosh_id"] <- c(1:length(res[,1]))
  res <- res[c('niosh_id', names(res[-8]))]
  res = res[res$casrn!="-",]
  res = res[res$casrn!="- ",]
  res = res[!is.na(res$toxval_numeric),]

  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="NIOSH",table="source_niosh",res=res,F,T,T)
}
