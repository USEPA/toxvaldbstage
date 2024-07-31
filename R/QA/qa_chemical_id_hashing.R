qa_chemical_id_hashing <- function(){
  # Local copy of chemical cleaning/hashing script, minus source_chemical loading
  qa_clean_chems_compare <- function(db,
                                     res,
                                     source,
                                     chem.check.halt=FALSE,
                                     casrn.col="casrn",
                                     name.col="name",
                                     verbose=F){
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
    if(is.na(prefix)){
      # Grab last entry to add a new prefix
      prefix = runQuery("SELECT chemprefix FROM chemical_source_index", db) %>%
        tail(1) %>%
        .[1,] %>%
        gsub("ToxVal", "", .) %>%
        as.numeric() %>%
        # Add 1 as well as an extra 10,000 padding to prevent overlap from manual entry
        {. <- . + 1 + 10000 } %>%
        # Add 0 padding
        formatC(width = 5, format = "d", flag = "0") %>%
        paste0("ToxVal", .)
      # Insert to chemical_source_index table
      data.frame(`source`= source, chemprefix = prefix) %>%
        runInsertTable(., "chemical_source_index", db, get.id = FALSE)
      # Pull prefix
      prefix = runQuery(paste0("select chemprefix from chemical_source_index where source='",source,"'"),db)[1,1]
      # Final error handling just in case
      if(is.na(prefix)) stop("No entry in chemical_source_index for source: ", source, "'")
    }
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
    return(res %>% left_join(chems %>% select(chemical_id, cleaned_name, cleaned_casrn),
                             by="chemical_id"))
  }

  # Old approach to chemical hashing
  old_chemical_hash <- function(chems){
    for(i in 1:nrow(chems)) {
      prefix = chems$prefix[i]
      chems[i,"chemical_id"] = paste0(prefix,"_",digest(paste0(chems[i,c("raw_casrn","raw_name","cleaned_casrn","cleaned_name")],collapse=""),
                                                        algo="xxhash64",
                                                        serialize = FALSE))
      if(i%%1000==0) cat(i," out of ",nrow(chems),"\n")
    }
    return(chems)
  }

  src_chem = runQuery("SELECT chemical_id, source, raw_name, raw_casrn, cleaned_name, cleaned_casrn FROM source_chemical", db)
  # Pull comparison from candidate source tables (previously ran wihtout a filter and identified by prefix)
  compare = src_chem %>%
    filter(source %in% c(
      'Cal OEHHA',
      'Cancer Summary',
      'DOE Protective Action Criteria',
      'EFSA',
      'EPA OPP',
      'Pennsylvania DEP MSCs',
      'Pennsylvania DEP ToxValues',
      'ChemIDplus',
      'ECOTOX',
      'GeneTox Details',
      'Genetox Summary'
    )) %>%
    separate(chemical_id, c("prefix", "id"), remove=FALSE) %>%
    dplyr::rename(old_chemical_id = chemical_id) %>%
    old_chemical_hash() %>%
    mutate(compare = old_chemical_id == chemical_id) %>%
    filter(compare == FALSE)
  #####################################
  # Filter comparison by identified source
  res_compare = compare %>%
    filter(prefix == "ToxVal00048") %>%
    select(old_chemical_id, chemical_id, raw_name, raw_casrn, cleaned_name, cleaned_casrn)
  # Pull source's RData import processed file
  load("Repo/z_source_import_processed/source_opp_import_processed.RData")
  source = "EPA OPP"
  # Clean chemical information again for comparison
  res = res %>%
    mutate(old_chemical_id = chemical_id) %>%
    qa_clean_chems_compare(db,res=.,source,chem.check.halt=FALSE,casrn.col="casrn",name.col="name") %>%
    filter(old_chemical_id %in% res_compare$old_chemical_id)  %>%
    dplyr::rename(raw_name= name,
                  raw_casrn = casrn)  %>%
    # old_chemical_hash() %>%
    # mutate(compare = old_chemical_id == chemical_id)
    select(old_chemical_id, chemical_id, raw_name, raw_casrn, cleaned_name, cleaned_casrn) %>%
    distinct()

  # Compare RData to Database data for inconsistencies
  all(arrange(res, old_chemical_id) %>% select(raw_name) %>% unique() == arrange(res_compare, old_chemical_id) %>% select(raw_name) %>% unique())
  all(arrange(res, old_chemical_id) %>% select(cleaned_name) %>% unique() == arrange(res_compare, old_chemical_id) %>% select(cleaned_name) %>% unique())
  all(arrange(res, old_chemical_id) %>% select(raw_casrn) %>% unique() == arrange(res_compare, old_chemical_id) %>% select(raw_casrn) %>% unique())
  all(arrange(res, old_chemical_id) %>% select(cleaned_casrn) %>% unique() == arrange(res_compare, old_chemical_id) %>% select(cleaned_casrn) %>% unique())


  ##############################
  # tbl_list = runQuery("show tables",db)[,1] %>%
  #   .[grepl("source_", .)] %>%
  #   .[!grepl("audit",. )] %>%
  #   .[!grepl("chemical_|source_chemical", .)]
  #
  # prefix_list = lapply(tbl_list, function(tbl){
  #   runQuery(paste0("SELECT chemical_id FROM ", tbl, " LIMIT 1"), db) %>%
  #     mutate(source_table = tbl)
  # }) %>%
  #   bind_rows()

}
