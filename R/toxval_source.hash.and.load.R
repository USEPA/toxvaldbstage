#--------------------------------------------------------------------------------------
#' Add the hash key to the source tables and add the new rows
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param source Name of the source
#' @param table Name of the database table
#' @param do.reset If TRUE, delete data from the database for this source before
#' inserting new data. Default FALSE
#' @param do.insert If TRUE, insert data into the database, default TRUE
#' @param res The data frame to be processed
#--------------------------------------------------------------------------------------
toxval_source.hash.and.load <- function(db="dev_toxval_source_v5",
                                        source,
                                        table,
                                        do.reset=F,
                                        do.insert=F,
                                        res) {

  printCurrentFunction(paste(db,source,table))

  non_hash_cols = c("chemical_id","source_id","clowder_id","document_name","source_hash","qc_status",
                    "parent_hash","create_time","modify_time","created_by", "qc_flags", "qc_notes", "version")

  if(is.element("chemical_index",names(res))) res = subset(res,select=-c(chemical_index))
  res$source_hash = "-"
  res$parent_hash = "-"
  res$create_time = Sys.time()
  # res$modify_time = NA
  res$created_by = "toxval source script"
  cat("dimension of res:",dim(res),"\n")
  #####################################################################
  cat("Build the hash key \n")
  #####################################################################
  nold = runQuery(paste0("select count(*) from ",table),db)
  if(nold>0) {
    sample0 = runQuery(paste0("select * from ",table, " limit 1"),db)
    nlist = names(sample0)
    nlist = nlist[!is.element(nlist,non_hash_cols)]
    sh0 = sample0[1,"source_hash"]
    sample = sample0[,sort(nlist)]
    sh1 = digest(paste0(sample,collapse=""), serialize = FALSE)
    cat("test that the columns are right for the hash key: ",sh0,sh1,"\n")
    #if(sh0!=sh1) browser()
  } else {
    nlist = runQuery(paste0("desc ",table),db)[,1]
    nlist = nlist[!is.element(nlist,non_hash_cols)]
  }

  #####################################################################
  cat("check the columns\n")
  #####################################################################
  nlist0 = names(res)
  nlist0 = nlist0[!is.element(nlist0,non_hash_cols)]
  nlist01 = nlist0[!is.element(nlist0,nlist)]
  nlist10 = nlist[!is.element(nlist,nlist0)]
  if(length(nlist01)>0) {
    cat("res has columns not in db\n")
    print(nlist01)
    cat("db has columns not in res\n")
    print(nlist10)
    browser()
  }

  res$source_hash = "-"
  nlist1 = names(res)
  to.remove = nlist1[!is.element(nlist1,nlist)]
  nlist1 = nlist1[!is.element(nlist1,to.remove)]
  to.add = nlist[!is.element(nlist,nlist1)]
  if(length(to.add)>0) {
    cat("columns to add:",to.add,"\n")
    browser()
    temp = as.data.frame(matrix(nrow=nrow(res),ncol=length(to.add)))
    names(temp) = to.add
    temp[] = "-"
    res = cbind(res,temp)
  } else cat("no columns need to be added\n")
  res.temp = res[,sort(nlist)]

  for (i in 1:nrow(res)){
    row <- res.temp[i,]
    res[i,"source_hash"] <- digest(paste0(row,collapse=""), serialize = FALSE)
    if(i%%1000==0) cat(i," out of ",nrow(res),"\n")
  }
  #####################################################################
  cat("See what is new \n")
  #####################################################################
  shlist0 = runQuery(paste("select source_hash from ",table),db)[,1]
  shlist1 = res$source_hash
  n0 = length(shlist0)
  n1 = length(shlist1)
  n01 = length(shlist0[is.element(shlist0,shlist1)])
  newfrac = 100*(n1-n01)/n1
  cat("**************************************************************************\n")
  cat(source,"\n")
  cat("hash matching: original,new,match:",n0,n1,n01," new percent: ",format(newfrac,digits=2),"\n")
  cat("**************************************************************************\n")

  if(do.reset) {
    #####################################################################
    cat("Do you really want to clean the database?\n")
    browser()
    #####################################################################
    runQuery(paste("delete from ",table),db)
  } else res = res[!is.element(res$source_hash,shlist0),]

  #####################################################################
  cat("Add to the database \n")
  #####################################################################
  if(nrow(res)>0) {
    cat("entering new rows:",nrow(res),"\n")
    if(do.insert) runInsertTable(res,table,db,do.halt=T,verbose=F)
  } else cat("no new rows to add\n")
}

