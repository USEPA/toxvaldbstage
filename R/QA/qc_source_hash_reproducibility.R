#' qa_source_hash_reproducibility
#' @description QC Function to programmatically assess the reproducibility of the source_hash system
qa_source_hash_reproducibility <- function(check.import=FALSE){
  non_hash_cols = c("chemical_id", "parent_chemical_id", "source_id","clowder_id","document_name","source_hash","qc_status",
                    "parent_hash","create_time","modify_time","created_by", "qc_flags", "qc_notes", "version",
                    "raw_input_file")

  tblList = runQuery(query = paste0("SHOW TABLES FROM ", db),
                     db=db) %>% unlist() %>% unname() %>%
    # Filter to those named "source_*"
    .[grepl("source_", .)] %>%
    # Ignore those like source_audit or source_chemical
    .[!grepl("chemical|source_audit", .)]

  out = data.frame()
  hashing_log_import = data.frame()
  hashing_log_database = data.frame()
  hashing_log = data.frame()
  problem_hashes = data.frame()

  for(table in tblList){
    nlist = runQuery(paste0("desc ",table),db)[,1]
    nlist = nlist[!is.element(nlist,non_hash_cols)]

    if(check.import){
      ########################################
      ### Pull from import RData
      import_list = list.files("Repo\\z_source_import_processed", pattern=table, full.names = TRUE)
      # Skip if no import RData available
      if(!length(import_list)) next
      # Load most recent impport RData
      load(max(import_list))
      df = res; rm(res)
      if(!nrow(df)) next
      df.temp = df[,sort(nlist)]
      for (i in 1:nrow(df)){
        row <- df.temp[i,]
        df[i,"test_hash"] <- digest(paste0(row,collapse=""), serialize = FALSE)
        if(i%%1000==0) cat(i," out of ",nrow(df),"\n")
        # tmp = rbind(tmp, data.frame(source_hash = df[i,"source_hash"],
        #                             hashcol = paste0(row,collapse="")))
        hashing_log_import = rbind(hashing_log_import,
                                   data.frame(import_rehash = df[i, "test_hash"],
                                              import=paste0(row,collapse="")))
      }
      df$compare = df$source_hash != df$test_hash

      # Vectorized way to eventaully implement after source_hash rehashing
      # df = df %>%
      #   tidyr::unite(hash_col, all_of(sort(names(.)[!names(.) %in% non_hash_cols])), sep="") %>%
      #   dplyr::rowwise() %>%
      #   dplyr::mutate(test_hash = digest(hash_col, serialize = FALSE),
      #          compare = source_hash != test_hash) %>%
      #   dplyr::ungroup()
      #
      # hashing_log_import = rbind(hashing_log_import,
      #                            data.frame(import_rehash = df$test_hash,
      #                                       import=df$hash_col))

      tmp = df %>%
        filter(compare == TRUE)
      if(nrow(tmp)){
        problem_hashes = rbind(problem_hashes,
                               tmp %>%
                                 select(source_hash, test_hash) %>%
                                 mutate(table = table,
                                        pull_type = "import_RData"))
      }

      df = data.frame(table_name = table,
                      n_diff_hash = nrow(df %>% filter(compare == TRUE)),
                      n_total = nrow(df)) %>%
        mutate(perc_diff = round((n_diff_hash / n_total * 100), 3),
               pull_type="import_RData")

      out = rbind(out, df)
    }
    ########################################
    ### Pull database
    ###

    df = runQuery(paste0("select * from ",table),db)
    # Old hashing approach
    df.temp = df[,sort(nlist)]
    for (i in 1:nrow(df)){
      row <- df.temp[i,]
      df[i,"test_hash"] <- digest(paste0(row,collapse=""), serialize = FALSE)
      if(i%%1000==0) cat(i," out of ",nrow(df),"\n")
      # tmp = rbind(tmp, data.frame(source_hash = df[i,"source_hash"],
      #                             hashcol = paste0(row,collapse="")))
      hashing_log_database = rbind(hashing_log_database,
                                 data.frame(db_rehash = df[i, "test_hash"],
                                            database=paste0(row,collapse="")))
    }
    df$compare = df$source_hash != df$test_hash

    # # Vectorized way to eventaully implement after source_hash rehashing
    # df = df %>%
    #   tidyr::unite(hash_col, all_of(sort(names(.)[!names(.) %in% non_hash_cols])), sep="") %>%
    #   dplyr::rowwise() %>%
    #   dplyr::mutate(test_hash = digest(hash_col, serialize = FALSE),
    #          compare = source_hash != test_hash) %>%
    #   dplyr::ungroup()
    #
    # hashing_log_import = rbind(hashing_log_import,
    #                            data.frame(db_rehash = df$test_hash,
    #                                       database=df$hash_col))

    tmp = df %>%
      filter(compare == TRUE)
    if(nrow(tmp)){
      problem_hashes = rbind(problem_hashes,
                             tmp %>%
                               select(source_hash, test_hash) %>%
                               mutate(table = table,
                                      pull_type = "database"))
    }

    df = data.frame(table_name = table,
                    n_diff_hash = nrow(df %>% filter(compare == TRUE)),
                    n_total = nrow(df)) %>%
      mutate(perc_diff = round((n_diff_hash / n_total * 100), 3),
             pull_type="database")

    out = rbind(out, df)

    message(table, " database  problem rows: ",  df$n_diff_hash, " of ", df$n_total, " (", df$perc_diff,"%)")

    hashing_log = rbind(hashing_log,
                        cbind(hashing_log_import, hashing_log_database) %>%
                          mutate(table=table))
    # Reset for next table
    hashing_log_import = data.frame()
    hashing_log_database = data.frame()
  }
  return(out)
}


