#--------------------------------------------------------------------------------------
#' @description Special process to deal with source chemicals for ToxRefDB. This will put the
#' chemicals into the source database source_chemical table
#' @param toxval.db The version of toxval into which the source info is loaded.
#' @param source.db The source database version
#' @param source The name of the source
#' @param res The dataframe to which the chemical_id sill be added
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#' @param casrn.col The name of the column containing the CASRN
#' @param name.col The name ofhte column containing hte chemical name
#' @param verbose If TRUE, write out diagnostic messages
#' @return Returns the input dataframe with the chemical_id added
#' @title FUNCTION_TITLE
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[digest]{digest}}
#' @rdname source_chemical.toxrefdb
#' @export 
#' @importFrom digest digest
#--------------------------------------------------------------------------------------
source_chemical.toxrefdb <- function(toxval.db,
                                     source.db,
                                     res,
                                     source="ToxRefDB",
                                     chem.check.halt=FALSE,
                                     casrn.col="casrn",
                                     name.col="name",
                                     verbose=F) {
  printCurrentFunction(paste0(db,"\n",source))
  #####################################################################
  cat("Do the chemical checking\n")
  #####################################################################
  res$chemical_index = paste(res[,casrn.col],res[,name.col])
  result = chem.check(res,name.col=name.col,casrn.col=casrn.col,verbose=verbose,source)
  if(chem.check.halt) if(!result$name.OK || !result$casrn.OK || !result$checksum.OK) browser()

  #####################################################################
  cat("Build the chemical table\n")
  #####################################################################
  chems = cbind(res[,c("dtxsid",casrn.col,name.col)],result$res0[,c(casrn.col,name.col)])
  names(chems) = c("dtxsid","raw_casrn","raw_name","cleaned_casrn","cleaned_name")
  chems = unique(chems)
  chems$source = source
  prefix = runQuery(paste0("select chemprefix from chemical_source_index where source='",source,"'"),source.db)[1,1]
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
  chems$casrn = chems$cleaned_casrn
  chems$name = chems$cleaned_name

  res$chemical_id = NA
  for(i in 1:nrow(chems)) {indx=chems[i,"chemical_index"]; cid=chems[i,"chemical_id"];res[generics::is.element(res$chemical_index,indx),"chemical_id"]=cid}
  chems = subset(chems,select=-c(chemical_index))
  cids = runQuery(paste0("select distinct chemical_id from source_chemical where source='",source,"'"),source.db)[,1]
  chems.new = chems[!generics::is.element(chems$chemical_id,cids),]
  n0 = length(cids)
  n1 = nrow(chems)
  n01 = nrow(chems.new)
  newfrac = 100*(n01)/n1
  cat("**************************************************************************\n")
  cat(source,"\n")
  cat("chem matching: original,new,match:",n0,n1,n01," new percent: ",format(newfrac,digits=2),"\n")
  cat("**************************************************************************\n")
  #browser()
  runInsertTable(chems.new,"source_chemical",source.db,do.halt=T,verbose=F)
  return(res)
}
