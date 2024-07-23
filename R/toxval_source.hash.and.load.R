#--------------------------------------------------------------------------------------
#' @description Add the hash key to the source tables and add the new rows
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param source Name of the source
#' @param table Name of the database table
#' @param do.reset If TRUE, delete data from the database for this source before #' inserting new data. Default FALSE
#' @param do.insert If TRUE, insert data into the database, default False
#' @param res The data frame to be processed
#' @param hashing_cols Optional list of columns to use for generating source_hash
#' @title toxval_source.hash.and.load
#' @return None
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[digest]{digest}}
#'  \code{\link[dplyr]{distinct}}
#' @rdname toxval_source.hash.and.load
#' @export
#' @importFrom digest digest
#' @importFrom dplyr distinct
#' @importFrom generics is.element
#--------------------------------------------------------------------------------------
toxval_source.hash.and.load <- function(db="dev_toxval_source_v5",
                                        source,
                                        table,
                                        do.reset=FALSE,
                                        do.insert=FALSE,
                                        res,
                                        hashing_cols=NULL) {
  # Testing purposes hardcoding insert False
  # do.insert = FALSE
  printCurrentFunction(paste(db,source,table))

  non_hash_cols <- toxval.config()$non_hash_cols

  if(generics::is.element("chemical_index",names(res))) res = subset(res,select=-c(chemical_index))
  res$source_hash = "-"
  res$parent_hash = "-"
  res$create_time = Sys.time()
  # res$modify_time = NA
  res$created_by = "toxval source script"
  cat("dimension of res:",dim(res),"\n")
  #####################################################################
  cat("Build the hash key \n")
  #####################################################################
  if(is.null(hashing_cols)){
    nold = runQuery(paste0("select count(*) from ",table),db)
    if(nold>0) {
      sample0 = runQuery(paste0("select * from ",table, " limit 1"),db)
      nlist = names(sample0)
      nlist = nlist[!generics::is.element(nlist,non_hash_cols)]
      sh0 = sample0[1,"source_hash"]
      sample = sample0[,sort(nlist)]
      sh1 = digest::digest(paste0(sample,collapse=""), serialize = FALSE)
      cat("test that the columns are right for the hash key: ",sh0,sh1,"\n")
      #if(sh0!=sh1) browser()
    } else {
      nlist = runQuery(paste0("desc ",table),db)[,1]
      nlist = nlist[!generics::is.element(nlist,non_hash_cols)]
    }

    #####################################################################
    cat("check the columns\n")
    #####################################################################
    nlist0 = names(res)
    nlist0 = nlist0[!generics::is.element(nlist0,non_hash_cols)]
    nlist01 = nlist0[!generics::is.element(nlist0,nlist)]
    nlist10 = nlist[!generics::is.element(nlist,nlist0)]
    if(length(nlist01)>0) {
      cat("res has columns not in db\n")
      print(nlist01)
      cat("db has columns not in res\n")
      print(nlist10)
      browser()
    }
  } else {
    nlist = runQuery(paste0("desc ",table),db)[,1]
    nlist = hashing_cols[hashing_cols %in% nlist]
  }

  res$source_hash = "-"
  nlist1 = names(res)
  to.remove = nlist1[!generics::is.element(nlist1,nlist)]
  nlist1 = nlist1[!generics::is.element(nlist1,to.remove)]
  to.add = nlist[!generics::is.element(nlist,nlist1)]
  if(length(to.add)>0) {
    cat("columns to add:",to.add,"\n")
    browser()
    temp = as.data.frame(matrix(nrow=nrow(res),ncol=length(to.add)))
    names(temp) = to.add
    temp[] = "-"
    res = cbind(res,temp)
  } else cat("no columns need to be added\n")
  res.temp = res[,sort(nlist)]

  if(is.null(hashing_cols)){
    # Old hashing system
    for (i in 1:nrow(res)){
      row <- res.temp[i,]
      res[i,"source_hash"] <- digest::digest(paste0(row,collapse=""), serialize = FALSE)
      if(i%%1000==0) cat(i," out of ",nrow(res),"\n")
    }
  } else {
    res.temp = source_hash_vectorized(res, hashing_cols=hashing_cols)
    res$source_hash = res.temp$source_hash

    # Check for immediate duplicate hashes
    dup_hashes = res %>%
      dplyr::group_by(source_hash) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::filter(n > 1)
    # res %>% filter(source_hash == "ToxVal_1612dd1200de3751846c110f5c2cf026") %>% select(all_of(hashing_cols)) %>% View()
    # Stop if duplicate source_hash values present
    if(nrow(dup_hashes)){
      cat("Duplicate source_hash values present in res...\n")
      browser()
      stop("Duplicate source_hash values present in res...")
    }
  }

  #####################################################################
  cat("See what is new \n")
  #####################################################################
  # Get all hash values in source table
  hash_check = runQuery(paste("select source_hash, parent_hash from ",table),db) %>%
    # Append audit table to hash_check for full hash check (audit and live)
    rbind(.,
          tryCatch({runQuery(paste0("SELECT fk_source_hash as source_hash, parent_hash FROM source_audit WHERE src_tbl_name = '",
                                    table, "'"), db, do.halt=FALSE)},
                   error=function(cond){ return(NULL) })) %>%
    dplyr::distinct()
  # Convert into unique vector of hash values
  hash_check = c(hash_check$source_hash, hash_check$parent_hash) %>% unique() %>% unlist()
  shlist1 = res$source_hash
  total = length(shlist1)
  new = length(shlist1[!shlist1 %in% hash_check])
  newfrac = 100*(new)/total
  cat("**************************************************************************\n")
  cat(source,"\n")
  cat("hash matching: new,total:",new,total," new percent: ",format(newfrac,digits=2),"\n")
  cat("**************************************************************************\n")

  # Export RData copy to inspect later
  if(!file.exists(paste0(toxval.config()$datapath, "z_source_import_processed/", table, "_import_processed_",Sys.Date(),".RData"))){
    cat("Exporting RData...\n")
    save(res, file=paste0(toxval.config()$datapath, "z_source_import_processed/", table, "_import_processed_",Sys.Date(),".RData"))
  }

  if(do.reset) {
    #####################################################################
    cat("Do you really want to clean the database?\n")
    browser()
    #####################################################################
    runQuery(paste("delete from ",table),db)
    save(res, file=paste0(toxval.config()$datapath, "z_source_import_processed/", table, "_import_processed.RData"))
  } else {
    # Check parent_hash and source_hash fields for all previous hashes
    res = res[!res$source_hash %in% hash_check,]
  }

  #####################################################################
  cat("Add to the database \n")
  #####################################################################
  if(nrow(res)>0) {
    if(do.insert) {
      cat("entering new rows:",nrow(res),"\n")
      runInsertTable(res,table,db,do.halt=T,verbose=F)
    } else {
      cat("Set do.insert to TRUE to insert new rows\n")
    }
  } else cat("no new rows to add\n")
}

