#--------------------------------------------------------------------------------------
#' @description Load PPRTV ORNL Source into toxval_source
#' @param db The version of toxval into which the source is loaded.
#' @param infile The input file ./pprtv_ornl/pprtv_ornl_files/new_PPRTV_ORNL cancer noncancer.xlsx
#' @param chem.check.halt If TRUE and there are problems with chemicals CASRN checks, halt the program
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
#' @rdname import_pprtv_ornl_source
#' @importFrom openxlsx read.xlsx
#' @importFrom generics is.element
#--------------------------------------------------------------------------------------
import_pprtv_ornl_source <- function(db,
                                     infile="new_PPRTV_ORNL cancer noncancer.xlsx",
                                     chem.check.halt=F) {
  printCurrentFunction(db)

  infile = paste0(toxval.config()$datapath,"pprtv_ornl/pprtv_ornl_files/",infile)
  #####################################################################
  cat("Build new_pprtv_ornl table\n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile)
  # res["pprtv_ornl_id"] <- c(1:length(res[,1]))
  # res <- res[c('pprtv_ornl_id', names(res[-23]))]

  res = res[!generics::is.element(res$casrn,c("VARIOUS","MULTIPLE")),]
  cat(nrow(res),"\n")
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="PPRTV (ORNL)",table="source_pprtv_ornl",res=res,F,T,T)
}
