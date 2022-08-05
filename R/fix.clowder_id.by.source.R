library('openxlsx')
library('dplyr')
#-------------------------------------------------------------------------------------
#' Add clowder_id's to source db tables based on matching values with source table
#' @param source.db The version of toxval source in which the data is altered.
#' @return The database will be altered
#' @export
#--------------------------------------------------------------------------------------
fix.clowder_id.by.source <- function(source.db,source_table,source){
  printCurrentFunction(paste(source.db,":", source_table,":",source))
  
  #####################################################################
  cat("extract dictionary info \n")
  #####################################################################
  
  file <- paste0(toxval.config()$datapath,"clowder_id_mapping/source_doc_with_clowder_id.xlsx")
  clow1 <- read.xlsx(file)
  print(dim(clow1))
  
  clow1 <- clow1[,names(clow1)[grep("qa\\_",names(clow1), invert = T)]]
  
  names(clow1) <- gsub("_original","",names(clow1))
  names(clow1) <- gsub("source_","",names(clow1))
  
  
  #####################################################################
  cat("extract source info \n")
  #####################################################################
  
  mat = runQuery(paste0("select * from ",source_table," "), source.db)
  
  mat$clowder_id <- "-"
  print(names(mat))
  

  mat$source <- "PFAS 150 SEM"
  # names(mat)[names(mat) == "species"] <- "species_original"
  mat[is.na(mat[,"document_name"]),"document_name"] <- "-"
  names(mat)[names(mat) == "document_name"] <- "document_name_original"
  

  clow2 <- clow1[,names(clow1)[names(clow1) %in% names(mat)]]
  clow3 <- clow2[which(clow2$source == paste0("",source,"")),]
  clow3 <- unique(clow3)
  print(dim(clow3))
  
  
  # 
  mat <- mat[,names(mat)[names(mat) != "clowder_id" ]]
  mat <- unique(mat)
  print(dim(mat))
  mat <- mat %>% left_join(clow3)
  
  
  print(dim(mat))
  mat <- mat[,names(mat)[names(mat)!= "source"]]
  print(dim(mat))
  
  names(mat)[names(mat) == "document_name_original"] <- "document_name"
  # 
  print(unique(mat$clowder_id))
  print(dim(mat))
  cat("delete from given source \n")
  runQuery(paste0("delete from ",source_table," "), source.db)


  cat("insert source table with clowder_id\n")
  runInsertTable(mat, paste0("",source_table,""), source.db)
  
  
}