#--------------------------------------------------------------------------------------
#' Load EnviroTox.V2 Source data into dev_toxval_source_v4.
#' @param db The version of toxval into which the source info is loaded.
#' @param infile The input file ./envirotox/envirotox_files/envirotox_taxonomy.xlsx
#--------------------------------------------------------------------------------------
import_envirotox_source <- function(db,
                                    infile="../envirotox/envirotox_files/envirotox_taxonomy clean casrn.xlsx",
                                    chem.check.halt=T) {
  printCurrentFunction(db)
  #####################################################################
  cat("Read envirotox file sheet1(test) as res \n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile)

  #####################################################################
  cat("change colnames to lowercase and convert dots in names to underscore \n")
  #####################################################################
  names(res) <- tolower(names(res))
  names(res) <- gsub("\\.","_",names(res))

  #####################################################################
  cat("fix casrn for cas and original_cas fields \n")
  #####################################################################
  clist = unique(res$cas)
  for(cas0 in clist) {
    cas = fix.casrn(cas0)
    res[is.element(res$cas,cas0),"cas"] = cas
  }
  clist = unique(res$original_cas)
  for(cas0 in clist) {
    cas = fix.casrn(cas0)
    res[is.element(res$original_cas,cas0),"original_cas"] = cas
  }
  # for (i in 1:dim(res)[1]){
  #   res[i, "cas"] <- fix.casrn(res[i, "cas"])
  # }
  # for (i in 1:dim(res)[1]){
  #   res[i, "original_cas"] <- fix.casrn(res[i, "original_cas"])
  # }
  names.list <- names(res)
  res[,"source_id"] <- c(1:length(res[,1]))
  res[,"source_hash"] <- "-"
  res[,"clowder_id"] <- "61f14c70e4b0ebacf2ec476c"
  res <- res[,c("source_id","source_hash","clowder_id", names.list)]

  #####################################################################
  cat("Do the chemical checking\n")
  #####################################################################
  source = "EnviroTox_v2"
  res = as.data.frame(res)
  res = res[!is.element(res$cas,"NOCAS"),]
  res = fix.non_ascii.v2(res,source)
  res = source_chemical.process(db,res,source,chem.check.halt,casrn.col="cas",name.col="chemical_name",verbose=F)
  #####################################################################
  cat("Build the hash key and load the data \n")
  #####################################################################
  res = subset(res,select=-c(chemical_index))
  toxval_source.hash.and.load(db,source,"original_envirotox",F,F,res)
  browser()
  return(1)
  runInsertTable(res,"original_envirotox",db,do.halt=T,verbose=F)

}
