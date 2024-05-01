#--------------------------------------------------------------------------------------
#' @description Deal with the process of making the source_chemical information
#' @param db The version of toxval into which the source info is loaded.
#' @param res The input dataframe to which chemical information will be added
#' @param source The source to process
#' @param table Name of the database table
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#' @param casrn.col The name of the column containing the CASRN
#' @param name.col The name ofhte column containing hte chemical name
#' @param verbose If TRUE, write out diagnostic messages #'
#' @return Returns the original dataframe with a chemical_id appended
#' @export
#' @title source_chemical.process
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[tidyr]{unite}}
#'  \code{\link[utils]{head}}
#'  \code{\link[digest]{digest}}
#' @rdname source_chemical.process
#' @importFrom tidyr unite
#' @importFrom utils tail
#' @importFrom digest digest
#' @importFrom tidyselect all_of
#' @importFrom dplyr distinct
#' @importFrom generics is.element
#--------------------------------------------------------------------------------------
source_chemical.process <- function(db,
                                    res,
                                    source,
                                    table,
                                    chem.check.halt=FALSE,
                                    casrn.col="casrn",
                                    name.col="name",
                                    verbose=FALSE) {
  printCurrentFunction(paste0(db,"\n",source))
  #####################################################################
  cat("Do the chemical checking\n")
  #####################################################################
  # res$chemical_index = paste(res[,casrn.col],res[,name.col])
  res = res %>%
    tidyr::unite(col="chemical_index", tidyselect::all_of(c(casrn.col, name.col)), sep=" ", remove=FALSE)
  # result = chem.check(res,name.col=name.col,casrn.col=casrn.col,verbose=verbose,source)
  result = chem.check.v2(res0=res, source=source, verbose=verbose)
  if(chem.check.halt) if(!result$name.OK || !result$casrn.OK || !result$checksum.OK) browser()

  #####################################################################
  cat("Build the chemical table\n")
  #####################################################################
  chems = cbind(res[,c(casrn.col,name.col)], result$res0[,c(casrn.col,name.col)])
  names(chems) = c("raw_casrn","raw_name","cleaned_casrn","cleaned_name")
  chems = dplyr::distinct(chems)
  chems$source = source

  prefix = runQuery(paste0("select chemprefix from chemical_source_index where source='",source,"'"),db)[1,1]
  if(is.na(prefix)){
    # Grab last entry to add a new prefix
    prefix = runQuery("SELECT chemprefix FROM chemical_source_index", db) %>%
      utils::tail(1) %>%
      .[1,] %>%
      gsub("ToxVal", "", .) %>%
      as.numeric() %>%
      # Add 1 as well as an extra 10,000 padding to prevent overlap from manual entry
      {. <- . + 1 } %>%
      # Add 0 padding
      formatC(width = 5, format = "d", flag = "0") %>%
      paste0("ToxVal", .)
    # Insert to chemical_source_index table
    data.frame(`source`= source, chemprefix = prefix, source_table=table) %>%
      runInsertTable(., "chemical_source_index", db, get.id = FALSE)
    # Pull prefix
    prefix = runQuery(paste0("select chemprefix from chemical_source_index where source='",source,"'"),db)[1,1]
    # Final error handling just in case
    if(is.na(prefix)) stop("No entry in chemical_source_index for source: ", source, "'")
  }

  # Prep chemical ID hash
  ilist = seq(from=1,to=nrow(chems))
  chems$chemical_id = "-"
  for(i in 1:nrow(chems)) {
    chems[i,"chemical_id"] = paste0(prefix,"_",digest::digest(paste0(chems[i,c("raw_casrn","raw_name")],collapse=""), algo="xxhash64", serialize = FALSE))
  }
  # check for duplicates
  x = chems$chemical_id
  y=sum(duplicated(x))
  if(y>0) {
    cat("******************************************************************\n")
    cat("some chemical hash keys are duplicated for ",source,"\n")
    cat("******************************************************************\n")
    browser()
  }
  chems$chemical_index = paste(chems$raw_casrn,chems$raw_name)
  res$chemical_id = NA
  for(i in 1:nrow(chems)) {indx=chems[i,"chemical_index"]; cid=chems[i,"chemical_id"];res[generics::is.element(res$chemical_index,indx),"chemical_id"]=cid}
  chems = subset(chems,select=-c(chemical_index))
  cids = runQuery(paste0("select distinct chemical_id from source_chemical where source='",source,"'"),db)[,1]
  chems.new = chems[!generics::is.element(chems$chemical_id,cids),]
  n0 = length(cids)
  n1 = nrow(chems)
  n01 = nrow(chems.new)
  newfrac = 100*(n01)/n1
  cat("**************************************************************************\n")
  cat(source,"\n")
  cat("chem matching: original,new,match:",n0,n1,n01," new percent: ",format(newfrac,digits=2),"\n")
  cat("**************************************************************************\n")
  runInsertTable(chems.new,"source_chemical",db,do.halt=T,verbose=F)
  return(res)
}
