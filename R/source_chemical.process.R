#--------------------------------------------------------------------------------------
#' Deal with the process of making the source_chemical informaiton
#' @param db The version of toxval into which the source info is loaded.
#' @param infile1 The input file ./test/test_files/TEST data.xlsx
#' @param infile2 The input file ./test/test_files/test_chemicals_invitrodb.csv to map casrn to names from prod_internal_invitrodb_v3_2.chemical
#--------------------------------------------------------------------------------------
source_chemical.process <- function(db,
                                    res,
                                    source,
                                    chem.check.halt=T,
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
  chems = cbind(res[,c(casrn.col,name.col)],result$res0[,c(casrn.col,name.col)])
  names(chems) = c("raw_casrn","raw_name","cleaned_casrn","cleaned_name")
  chems = unique(chems)
  chems$source = source
  prefix = runQuery(paste0("select chemprefix from chemical_source_index where source='",source,"'"),db)[1,1]
  ilist = seq(from=1,to=nrow(chems))
  chems$chemical_id = "-"
  for(i in 1:nrow(chems)) {
    chems[i,"chemical_id"] = paste0(prefix,"_",digest(paste0(chems[i,c("raw_casrn","raw_name","cleaned_casrn","cleaned_name")],collapse=""),algo="xxhash64", serialize = FALSE))
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
  for(i in 1:nrow(chems)) {indx=chems[i,"chemical_index"]; cid=chems[i,"chemical_id"];res[is.element(res$chemical_index,indx),"chemical_id"]=cid}
  chems = subset(chems,select=-c(chemical_index))
  cids = runQuery(paste0("select distinct chemical_id from source_chemical where source='",source,"'"),db)[,1]
  chems.new = chems[!is.element(chems$chemical_id,cids),]
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
