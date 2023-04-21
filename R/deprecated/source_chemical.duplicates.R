#-------------------------------------------------------------------------------------
#' Find duplicated chemicals in the source_chemical table. THis will help get rid of
#' records that have been repalced
#' @param db The version of toxval into which the tables are loaded.
#--------------------------------------------------------------------------------------
source_chemical.duplicates <- function(db) {
  printCurrentFunction()
  chems = runQuery("select * from source_chemical",db)
  slist = sort(unique(chems$source))
  res = NULL
  for(source in slist) {
    temp = chems[chems$source==source,]
    clist = temp$cleaned_name
    dups = clist[duplicated(clist)]
    if(length(dups)>0) {
      cat(source,length(dups),"\n")
      temp = temp[is.element(temp$cleaned_name,dups),]
      temp = temp[order(temp$cleaned_name),]
      res = rbind(res,temp)
    }
  }
  file = paste0(toxval.config()$datapath,"/export/duplicated.chemicals.xlsx")
  write.xlsx(res,file)
}


