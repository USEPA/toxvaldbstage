library("openxlsx")
#--------------------------------------------------------------------------------------

#' @param toxval.db The version of toxval in which the data is altered.
#' @param source.db The version of toxval source into which clowder data is loaded.
#' @param apiKey The API key required for a user to access the Clowder dataset, prefix the apiKey value with "?key="
#' @param clowder_dataset A character string for the dataset name
#' First create a table in source db with clowder document name and record source hash
#' Update missing document names in toxval v9 record source table using data from clowder table 
#' apiKey="", clowder_dataset="CCTE ToxValDB PDFs"

#--------------------------------------------------------------------------------------

load_clowder_document_name <- function(toxval.db, apiKey,clowder_dataset) {
  printCurrentFunction(toxval.db)
  #####################################################################
  cat("create dataframe with clowder document name and record source hash \n")
  #####################################################################
  all_clowder_docs <- get_clowder_docList(apiKey, clowder_dataset)
  # file <- paste0("./dictionary/all_clowder_docs_",Sys.Date(),".xlsx")
  # write.xlsx(all_clowder_docs, file)
  print(dim(all_clowder_docs))
  # remove ^CCTE|^PFAS from filename , ^\\d+\\.pdf$ from filename
  all_clowder_docs <-  all_clowder_docs[grep("^CCTE.*|^PFAS.*", all_clowder_docs$filename, invert = T), ]
  print(dim(all_clowder_docs))
  all_clowder_docs <-  all_clowder_docs[grep("^\\d+\\.pdf$", all_clowder_docs$filename, invert = T), ]
  print(dim(all_clowder_docs))
  file <- paste0("./dictionary/clean_all_clowder_docs_",Sys.Date(),".xlsx")
  write.xlsx(all_clowder_docs, file)
  
  
  document_name <- gsub("(^[a-zA-Z0-9]+\\-)(.*)","\\2",all_clowder_docs$filename)
  record_source_hash <- gsub("(^[a-zA-Z0-9]+)(\\-.*)","\\1",all_clowder_docs$filename)
  document_table <- data.frame(record_source_hash,document_name, stringsAsFactors = F)
  file <- paste0("./dictionary/clowder_document_name_table_",Sys.Date(),".xlsx")
  write.xlsx(document_table, file)

  print(dim(document_table))
  rm(document_name,record_source_hash)
  
  ##########commentation start on aug 25############
  # query <- "select record_source_hash, document_name, source from record_source"
  # res <- runQuery(query,toxval.db)
  # 
  # x <- unique(res$record_source_hash)
  # print(length(x))
  # x <- x[!is.element(x,document_table[,1])]
  # cat("   missing values in dictionary:",length(x),"\n")
  # 
  # run a check to see how many document_names in v9 are present in clowder
  query <- "select distinct(document_name) from record_source"
  res <- runQuery(query,toxval.db)
  print(View(res))
  z <- res$document_name
  print(length(z))
  #10376
  z <- z[!is.element(z,document_table[,2])]
  cat("   missing document_names in dictionary:",length(z),"\n")
  file <- paste0("./dictionary/missing_clowder_document_names_",Sys.Date(),".xlsx")
  write.xlsx(z,file )
  # # missing document_names in dictionary: 5659
  # # z <- data.frame(rep("-",length(z)),z, stringsAsFactors = F)
  # # names(z) <- c("record_source_hash","document_name")
  # # runInsertTable(z, "missing_clowder_document_names", toxval.db, do.halt=T,verbose=F)
  # #update missing_clowder_document_names a inner join record_source b on a.document_name = b.document_name and a.record_source_hash like '-' SET a.record_source_hash = b.record_source_hash
  # #delete from missing_clowder_document_names where document_name like "-"
  # 
  # 
  # query <- "select * from missing_clowder_document_names"
  # res <- runQuery(query,toxval.db)
  # 
  # filenames_in_actor <- list.files('./clowder_document_names')
  # filenames_in_actor <- filenames_in_actor[filenames_in_actor != "toxrefdb"]
  # 
  # filenames_in_toxrefdb <- list.files('./clowder_document_names/toxrefdb')
  # print(length(filenames_in_toxrefdb))
  # 
  # documents_in_drive <- c(filenames_in_actor,filenames_in_toxrefdb)
  # print(length(documents_in_drive))
  # 
  # a <- res$document_name
  # a1 <- a[!is.element(a,documents_in_drive)]
  # query <- "select distinct document_name, source from record_source where source not like 'ToxRefDB'"
  # res_new <- runQuery(query,toxval.db)
  # b <- res_new[is.element(res_new$document_name, a1),]
  # file <- paste0("./dictionary/missing_folder_document_names_with_source_",Sys.Date(),".xlsx")
  # write.xlsx(unique(b),file )
  # 
  # 
  # 
  # # look for pipe delimited values in documents in drive
  # a2 <- grep(".*\\|.*", a1, value = T)
  # a4 <- a1[!is.element(a1,a2)]
  # a2 <- unlist(strsplit(a2, "|", fixed = TRUE))
  # 
  # a3 <- a2[!is.element(a2,documents_in_drive)]
  # 
  # cat("   unpiped names not in missing_clowder_document_names:",length(unique(a3)),"\n")
  # # out of 943 piped vales, unpiped names not in missing_clowder_document_names: 76
  # file <- paste0("./dictionary/missing_unpiped_document_names_",Sys.Date(),".xlsx")
  # write.xlsx(unique(a3), file)
  # a4 <- c(a4,a3)
  # cat("   combined unpiped names not in missing_clowder_document_names:",length(unique(a4)),"\n")
  # file <- paste0("./dictionary/combined_missing_and_unpiped_document_names_",Sys.Date(),".xlsx")
  # write.xlsx(unique(a4), file)
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # cat("   folder document_names not in missing_clowder_document_names:",length(unique(a1)),"\n")
  # # folder document_names not in missing_clowder_document_names: 1835
  # a1 <- data.frame(rep("-",length(a1)),a1, stringsAsFactors = F)
  # names(a1) <- c("record_source_hash","document_name")
  # file <- paste0("./dictionary/missing_folder_document_names_",Sys.Date(),".xlsx")
  # write.xlsx(unique(a1),file )
  # 
  # 
  # a1 <- res$document_name
  # a1 <- a1[is.element(a1,documents_in_drive)]
  # cat("   folder document_names in missing_clowder_document_names:",length(unique(a1)),"\n")
  # # folder document_names in missing_clowder_document_names: 3823
  # a1 <- data.frame(rep("-",length(a1)),a1, stringsAsFactors = F)
  # names(a1) <- c("record_source_hash","document_name")
  # file <- paste0("./dictionary/folder_document_names_in_missing_clowder_names_",Sys.Date(),".xlsx")
  # write.xlsx(unique(a1),file )
  # a1$record_source_hash <-  res[match(a1$document_name, res$document_name),1]
  # #print(View(a1))
  # file <- paste0("./dictionary/folder_document_names_",Sys.Date(),".xlsx")
  # write.xlsx(unique(a1),file )
  # 
  # # runInsertTable(document_table, "clowder_document_name_dict", toxval.db, do.halt=T,verbose=F)
  # # 
  # # #####################################################################
  # # cat("Set empty cells in record_source table document name with clowder document names\n")
  # # #####################################################################
  # # 
  # # query <- "update record_source a inner join clowder_document_name_dict b on a.record_source_hash = b.record_source_hash and a.document_name like '-' SET a.document_name = b.document_name"
  # # 
  # # runQuery(query,toxval.db)
  # 
  # 
  
}
  