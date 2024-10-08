#--------------------------------------------------------------------------------------
#' @description special process to deal with source chemicals for extra source (cancer, genetox, skin_eye, etc)
#' @param toxval.db The version of toxval into which the source info is loaded.
#' @param source.db The source database version
#' @param source The source to be processed
#' @param chem.check.halt If TRUE, halt if there are errors in the chemical checking
#' @param casrn.col Name of the column containing the CASRN
#' @param name.col Name of the column containing chemical names
#' @param verbose If TRUE, output extra diagnostics information
#' @title FUNCTION_TITLE
#' @param res PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[digest]{digest}}
#' @rdname source_chemical.extra
#' @export 
#' @importFrom digest digest
#--------------------------------------------------------------------------------------
source_chemical.extra <- function(toxval.db,
                                  source.db,
                                   res,
                                   source,
                                   chem.check.halt=FALSE,
                                   casrn.col="casrn",
                                   name.col="name",
                                   verbose=F) {
  printCurrentFunction(paste0(db,"\n",source))
  if(!exists("DSSTOX")) load.dsstox()

  #####################################################################
  cat("Do the chemical checking\n")
  #####################################################################
  res$chemical_index = paste(res[,casrn.col],res[,name.col])
  result = chem.check.v2(res,verbose=verbose,source)
  if(chem.check.halt) if(!result$name.OK || !result$casrn.OK || !result$checksum.OK) browser()

  #####################################################################
  cat("Build the chemical table\n")
  #####################################################################
  chems = cbind(res[,c(casrn.col,name.col)],result$res0[,c(casrn.col,name.col)])
  names(chems) = c("raw_casrn","raw_name","cleaned_casrn","cleaned_name")
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
  cat("add the dtxsid\n")
  # file = paste0(toxval.config()$datapath,"/ecotox/ecotox_files/ECOTOX chemical mapping 2022-07-21.xlsx")
  # echems = read.xlsx(file)
  # rownames(echems) = as.character(echems$casrn)
  dsstox = DSSTOX[generics::is.element(DSSTOX$casrn,chems$cleaned_casrn),]

  chems$dtxsid = "NODTXSID"
  for(i in 1:nrow(chems)) {
    casrn = as.character(chems[i,"raw_casrn"])
    ccasrn = chems[i,"cleaned_casrn"]
    # if(is.element(casrn,echems$casrn)) {
    #   chems[i,"cleaned_name"] = echems[casrn,"name"]
    #   chems[i,"name"] = echems[casrn,"name"]
    #   chems[i,"casrn"] = ccasrn
    #   chems[i,"dtxsid"] = echems[casrn,"dtxsid"]
    # }
    # else {
      if(generics::is.element(ccasrn,dsstox$casrn)) {
        chems[i,"cleaned_name"] = dsstox[ccasrn,"preferred_name"]
        chems[i,"casrn"] = ccasrn
        chems[i,"name"] = dsstox[ccasrn,"preferred_name"]
        chems[i,"dtxsid"] = dsstox[ccasrn,"dtxsid"]
      }
    # }
  }

  cat("add the chemical_id to res\n")
  res$chemical_id = NA
  res$dtxsid = NA
  for(i in 1:nrow(chems)) {
    indx = chems[i,"chemical_index"]
    cid = chems[i,"chemical_id"]
    dtxsid = chems[i,"dtxsid"]
    res[generics::is.element(res$chemical_index,indx),"chemical_id"]=cid
    res[generics::is.element(res$chemical_index,indx),"dtxsid"]=dtxsid
    if(i%%1000==0) cat(" finished ",i," out of ",nrow(chems),"\n")
  }
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

  runInsertTable(chems.new,"source_chemical",source.db,do.halt=T,verbose=F)
  cids = runQuery(paste0("select distinct chemical_id from source_chemical where source='",source,"'"),toxval.db)[,1]
  chems.new = chems[!generics::is.element(chems$chemical_id,cids),]
  runInsertTable(chems.new,"source_chemical",toxval.db,do.halt=T,verbose=F)
  return(res)
}
