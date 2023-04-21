#-------------------------------------------------------------------------------------
#' Load The chemical lists for corresponding sources to toxval. All Excel files in the folder ../chemicals/for_load/
#' are loaded. The must have the columns casrn, name, list_name
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param verbose If TRUE, print debugging messages
#--------------------------------------------------------------------------------------
toxval.load.chemical.list.by.source <- function(toxval.db,source,verbose=T) {
  printCurrentFunction(paste(toxval.db,":", source))

  runQuery(paste0("delete from chemical_list where chemical_id in (select chemical_id from source_chemical where source='",source,"')"),toxval.db)
  chemical_lists <- paste0("chemical_lists_from_",source)
  runQuery(paste0("delete from chemical_list where chemical_id in (select chemical_id from source_chemical where source='",chemical_lists,"')"),toxval.db)


  dir <- "./chemicals/for_load/"
  flist <- list.files(dir)
  mat <- NULL
  for(fname in flist) {
    file <- paste0(dir,fname)
    temp <- read.xlsx(file)
    temp <- temp[,2:4]
    names(temp) <- c("casrn","name","list_name")
    cat(fname,length(unique(temp$list_name)),"\n")
    #if(length(unique(temp$list_name))>2) browser()
    temp <- unique(temp)
    mat <- rbind(mat,temp)
  }
  #browser()
  mat <- mat[!is.na(mat[,"casrn"]),]
  cat("start fixing casrn\n");
  for(i in 1:nrow(mat)) {
    mat[i,"casrn"] <- fix.casrn(mat[i,"casrn"])
  }
  cat("finish fixing casrn\n");

  cat("add the dtxsid\n");

  source.casrn.list <- runQuery(paste0("select source_casrn from source_chemical where chemical_id in (select chemical_id from toxval where source='",source,"')"),toxval.db)

  mat <- mat[which(mat$casrn %in% source.casrn.list$source_casrn),]
  print(dim(mat))
  casrn.list = unique(mat$casrn)

  cat("length of casrn.list:",length(casrn.list),"\n")
  for(i in 1:length(casrn.list)) {
    casrn = casrn.list[i]
    if(is.element(casrn,DSSTOX$casrn)) {
      dtxsid <- DSSTOX[is.element(DSSTOX$casrn,casrn),"dsstox_substance_id"]
      pname <- DSSTOX[is.element(DSSTOX$casrn,casrn),"preferred_name"]
      pname <- str_replace_all(pname,"\'","\\\\'")
      mat[is.element(mat$casrn,casrn),"dtxsid"] = dtxsid
      mat[is.element(mat$casrn,casrn),"name"] = pname
    }
    if(verbose) if(i%%100==0) cat("chemicals updated:",i," out of ",length(casrn.list),"\n")
  }

  cas.list = mat[,c("casrn","name")]
  cid.list = get.cid.list.toxval(toxval.db, cas.list,chemical_lists)
  mat$chemical_id <- cid.list$chemical_id
  cat("get the cid list\n")


  runInsertTable(mat, "chemical_list", toxval.db,verbose)
}
