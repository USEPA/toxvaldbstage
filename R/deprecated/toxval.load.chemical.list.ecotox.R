#-------------------------------------------------------------------------------------
#' Load The chemical lists for corresponding sources to toxval. All Excel files in the folder ../chemicals/for_load/
#' are loaded. The must have the columns casrn, name, list_name
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param verbose If TRUE, print debugging messages
#--------------------------------------------------------------------------------------
toxval.load.chemical.list.ecotox <- function(toxval.db,source,verbose=T) {
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

  source.casrn.list <- runQuery(paste0("select source_casrn,source_name,dtxsid from source_chemical where chemical_id in (select chemical_id from toxval where source='",source,"')"),toxval.db)
  source.casrn.list <- data.frame(source.casrn.list, stringsAsFactors = F)
  print(dim(source.casrn.list))

  mat <- mat[which(mat$casrn %in% source.casrn.list$source_casrn),]
  #print(View(mat))
  casrn.list = unique(mat$casrn)

  cat("length of casrn.list:",length(casrn.list),"\n")

  mat2 <- merge(x = mat, y = source.casrn.list, by.x = "casrn", by.y = "source_casrn")
  mat2 <- mat2[,names(mat2)[!names(mat2) %in% "name"]]
  names(mat2)[names(mat2) %in% "source_name"] <- "name"
  mat2 <- unique(mat2)
  #print(View(mat2))



  cas.list = mat2[,c("casrn","name","dtxsid")]
  cid.list = get.cid.list.toxval.ecotox(toxval.db, cas.list,chemical_lists)
  mat2$chemical_id <- cid.list$chemical_id
  cat("get the cid list\n")


  runInsertTable(mat2, "chemical_list", toxval.db,verbose)
}
