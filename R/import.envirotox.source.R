#--------------------------------------------------------------------------------------
#' @description Load EnviroTox.V2 Source data into dev_toxval_source_v4.
#' @param toxval.db The version of toxval into which the source info is loaded.
#' @param infile The input file ./envirotox/envirotox_files/envirotox_taxonomy.xlsx
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
#' @rdname import.envirotox.source
#' @export 
#' @importFrom openxlsx read.xlsx
#--------------------------------------------------------------------------------------

import_envirotox_source <- function(toxval.db,infile) {
  printCurrentFunction(toxval.db)

  #####################################################################
  cat("Read envirotox file sheet1(test) as res \n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile, 1)

  #####################################################################
  cat("change colnames to lowercase and convert dots in names to underscore \n")
  #####################################################################

  names(res) <- tolower(names(res))
  names(res) <- gsub("\\.","_",names(res))

  #####################################################################
  cat("fix casrn for cas and original_cas fields \n")
  #####################################################################

  for (i in 1:dim(res)[1]){
    res[i, "cas"] <- fix.casrn(res[i, "cas"])
  }


  for (i in 1:dim(res)[1]){
    res[i, "original_cas"] <- fix.casrn(res[i, "original_cas"])
  }

  names.list <- names(res)

  res[,"source_id"] <- c(1:length(res[,1]))
  res[,"source_hash"] <- "-"
  res[,"clowder_id"] <- "61f14c70e4b0ebacf2ec476c"

  res <- res[,c("source_id","source_hash","clowder_id", names.list)]
  #print(View(res))

  runInsertTable(res,"original_envirotox",toxval.db,do.halt=T,verbose=F)



}
