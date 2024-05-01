#--------------------------------------------------------------------------------------
#' @description Load copper manufacturers Source into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile The input file ./copper/copper_files/Copper Data Entry - Final.xlsx
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
#' @rdname import_copper_source
#' @export
#' @importFrom openxlsx read.xlsx
#--------------------------------------------------------------------------------------
import_copper_source <- function(db,
                                 infile="Copper Data Entry - Final.xlsx",
                                 chem.check.halt=F) {
  printCurrentFunction(db)
  infile = paste0(toxval.config()$datapath,"copper/copper_files/",infile)
  #####################################################################
  cat("create original_copper_table from source file\n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile)
  res = res[!is.na(res$casrn),]
  names(res)[c(7,34)]<- c("toxval_subtype1","year1")
  res$toxval_numeric_qualifier <- as.character(res$toxval_numeric_qualifier)

  res = res %>%
    dplyr::mutate(
      toxval_subtype = dplyr::case_when(
        toxval_subtype %in% c(as.character(NA), "-") ~ toxval_subtype1,
        TRUE ~ toxval_subtype
      )
    ) %>%
    dplyr::select(-toxval_subtype1) %>%
    dplyr::distinct()

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"

  # Perform deduping
  # res = toxval.source.import.dedup(res, dedup_fields=c("title", "author", "journal", "volume", "year1", "issue", "url"))
  res = toxval.source.import.dedup(res)

  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="Copper Manufacturers",table="source_copper",res=res,F,T,T)
}
