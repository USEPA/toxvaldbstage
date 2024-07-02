#' Helper function to compare source_hash generation fidelity between RData
#' files of data pushed to database to pulls from the database. This ensures
#' no unknown truncations, transformations, or encoding issues happen between the
#' processes. WOrkflow inspired from qa_chemical_id_hashing.R
qa_source_hashing <- function(){

  qa_source_hash <- function(db,
                             source,
                             table,
                             do.reset=F,
                             do.insert=F,
                             res){

    non_hash_cols <- toxval.config()$non_hash_cols

    nlist = runQuery(paste0("desc ",table),db)[,1]
    nlist = nlist[!is.element(nlist,non_hash_cols)]

    #####################################################################
    cat("check the columns\n")
    #####################################################################
    nlist0 = names(res)
    nlist0 = nlist0[!is.element(nlist0,non_hash_cols)]
    nlist01 = nlist0[!is.element(nlist0,nlist)]
    nlist10 = nlist[!is.element(nlist,nlist0)]

    res$source_hash = "-"
    nlist1 = names(res)
    to.remove = nlist1[!is.element(nlist1,nlist)]
    nlist1 = nlist1[!is.element(nlist1,to.remove)]
    to.add = nlist[!is.element(nlist,nlist1)]

    res.temp = res[,sort(nlist)]

    for (i in 1:nrow(res)){
      row <- res.temp[i,]
      res[i,"source_hash"] <- digest(paste0(row,collapse=""), serialize = FALSE)
      if(i%%1000==0) cat(i," out of ",nrow(res),"\n")
    }
    return(res)
  }

  #######
  # Generate comparison

  # Get list of source tables to add triggers
  tblList = runQuery(query = paste0("SHOW TABLES FROM ", db),
                     db=db) %>% unlist() %>% unname() %>%
    # Filter to those named "source_*"
    .[grepl("source_", .)] %>%
    # Ignore those like source_audit or source_chemical
    .[!grepl("chemical|source_audit", .)]

  for(table in tblList){
    message("Checking ", table, " - ", Sys.time())
    if(file.exists(paste0("Repo/source_hash_check/", table, ".RData"))) next
    # Get prefix to find source name
    prefix = runQuery(paste0("SELECT chemical_id FROM ", table, " LIMIT 1"), db) %>%
      toString() %>%
      strsplit(., "_") %>%
      .[[1]] %>%
      .[1]
    # Get source name
    source = runQuery(paste0("SELECT source FROM chemical_source_index where chemprefix = '", prefix, "'"), db) %>%
      toString()
    # Run comparison
    compare = runQuery(paste0("SELECT * FROM ", table), db) %>%
      dplyr::rename(old_source_hash = source_hash) %>%
      qa_source_hash(db,source,table,F,T,res=.) %>%
      mutate(compare = old_source_hash == source_hash) %>%
      filter(compare == FALSE)
    # Export if differences found
    if(nrow(compare)){
      save(compare, file=paste0("Repo/source_hash_check/", table, ".RData"))
    }
  }

  # Load cached comparisons
  out = list.files("Repo/source_hash_check/") %>%
    lapply(., function(table){
      load(paste0("Repo/source_hash_check/", table))
      return(compare)
    }) %T>% {
      names(.) <- list.files("Repo/source_hash_check/") }
}



