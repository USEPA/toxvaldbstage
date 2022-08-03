library("openxlsx")
library("logr")
#--------------------------------------------------------------------------------------
#' Replace record_source_hash with clowder record_source_hash by mapping using document names
#' @param toxval.db The version of toxval in which the data is altered.
#' @param source.db The version of toxval source into which clowder data is loaded.
#' @param apiKey The API key required for a user to access the Clowder dataset, prefix the apiKey value with "?key="
#' @param clowder_dataset A character string for the dataset name
#' First create a table in source db with clowder document name and record source hash
#' Update record_source_hash in toxval v9 record source table using data from clowder table 
#' apiKey="?key=d2547ff7-83ee-4f4e-b7b8-9875a5b18a83", clowder_dataset="CCTE ToxValDB PDFs"

#--------------------------------------------------------------------------------------

map.hash.record.clowder.document_name <- function(toxval.db, apiKey,clowder_dataset) {
  printCurrentFunction(toxval.db)
  
  # cat ("Program started running...")
  # cat ("\nPlease check 'map_record_source_hash.log' if you don't see notification 'program ran successfully'...")
  # 
  # con1 <- file.path("./dictionary/map_record_source_hash.log")
  # con1 <- log_open(con1)
  # file1 <- paste0("./dictionary/log/map_record_source_hash.log")
  # con <- file(file1)
  # sink(con, append=TRUE)
  # sink(con, append=TRUE, type="message")
  #####################################################################
  cat("create dataframe with clowder document name and record source hash \n")
  #####################################################################
  all_clowder_docs <- get_clowder_docList(apiKey, clowder_dataset)
  file <- paste0("./dictionary/all_clowder_docs_",Sys.Date(),".xlsx")
  write.xlsx(all_clowder_docs, file)
  cat("  dimension of table containing all clowder data:", dim(all_clowder_docs), "\n")
  
  
  #####################################################################
  cat("remove ^CCTE|^PFAS from filename (eg:CCTE_Deliverable11_ENL.ris) , ^\\d+\\.pdf$ from filename(eg: 1295637.pdf) \n")
  #####################################################################
  all_clowder_docs <-  all_clowder_docs[grep("^CCTE.*|^PFAS.*", all_clowder_docs$filename, invert = T), ]
  cat("  dimension of clowder table with titles excluded:", dim(all_clowder_docs), "\n")
  all_clowder_docs <-  all_clowder_docs[grep("^\\d+\\.pdf$", all_clowder_docs$filename, invert = T), ]
  cat("  dimension of clowder table with number.pdfs excluded:", dim(all_clowder_docs), "\n")
  file <- paste0("./dictionary/clean_all_clowder_docs_",Sys.Date(),".xlsx")
  write.xlsx(all_clowder_docs, file)
  
  #####################################################################
  cat("extract document name and record source hash from file name \n")
  #####################################################################
  document_name <- gsub("(^[a-zA-Z0-9]+\\-)(.*)","\\2",all_clowder_docs$filename)
  record_source_hash <- gsub("(^[a-zA-Z0-9]+)(\\-.*)","\\1",all_clowder_docs$filename)
  document_table <- data.frame(record_source_hash,document_name, stringsAsFactors = F)
  file <- paste0("./dictionary/clowder_document_name_table_",Sys.Date(),".xlsx")
  write.xlsx(document_table, file)
  cat("  dimension of final table containing document name and record source hash from clowder:", dim(document_table), "\n")
  rm(document_name,record_source_hash)
  print(View(document_table))
  #####################################################################
  cat("extract record source table information \n")
  #####################################################################
  query <- "select record_source_hash, document_name, source from record_source"
  res <- runQuery(query,toxval.db)

  x <- unique(res$document_name)
  cat("  unique number of document names in record source:", length(x), "\n")

  x <- x[!is.element(x,document_table[,2])]
  cat("  missing document names in clowder dictionary:",length(x),"\n")
  
  res1 <- res[which(res$document_name %in% x),c("document_name","source")]
  res1 <- unique(res1)
  file <- paste0("./dictionary/missing_clowder_document_name_",Sys.Date(),".xlsx")
  write.xlsx(res1, file)
  rm(res1)
  
  
  y <- unique(res$document_name)
  cat("  unique number of document names in record source:", length(y), "\n")
  
  y <- y[is.element(y,document_table[,2])]
  cat("  document names found in clowder dictionary:",length(y),"\n")
  
  res2 <- res[which(res$document_name %in% y),c("document_name","source")]
  res2 <- unique(res2)
  file <- paste0("./dictionary/clowder_document_name_in_record_source_",Sys.Date(),".xlsx")
  write.xlsx(res2, file)
  rm(res2)
  
  
  
  
  ######################################################################
  cat("Get the number of record source hashes replaced using icf dict \n")
  ######################################################################
  
  cat(" get record_source data \n")
  query <- "select * from record_source"
  mat <- runQuery(query,toxval.db)
  cat("   dimension of record source table:",dim(mat),"\n")
  # get unique record_source_hash and long_ref from record source table
  res <- data.frame(unique(mat[,c("record_source_hash","long_ref")]),stringsAsFactors = F)
  names(res) <- c("record_source_hash","long_ref")
  cat("   number of unique long ref in record source table:",dim(res)[1],"\n")
  
  
  cat(" get icf master dict data \n")
  file <- paste0(toxval.config()$datapath,"dictionary/CCTE_Deliverable_RefinfoSheet_Master_Aug_2021.xlsx")
  temp <- read.xlsx(file)
  # get the unique hash keys and long_ref from icf dictionary
  dict <- data.frame(unique(temp[,c("Record.Source.Hash","Long_Ref")]), stringsAsFactors = F)
  names(dict) <- c("record_source_hash","long_ref")
  cat("   dimension of icf dictionary with unique record source hash and long ref:",dim(dict),"\n")

  cat(" find number of record_source hashes that was replaced in record_source table \n")
  icf_hash_in_res <-res[which(res$record_source_hash %in% dict$record_source_hash), ]
  cat("   number of icf dictionary hashes in record source:",dim(icf_hash_in_res)[1],"\n")
  
  # writeLines(readLines(con1))
  # log_close()
  # closeAllConnections()
  # cat("\n\nprogram ran successfully, please check 'map_record_source_hash.log' for details \n")
  
  
}
   