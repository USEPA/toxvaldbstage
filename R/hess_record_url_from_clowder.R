#--------------------------------------------------------------------------------------
#' Get record urls for hess documents from clowder
#' @param toxval.db The version of toxval in which the data is altered.
#' @param source.db The version of toxval source into which clowder data is loaded.
#' @param apiKey The API key required for a user to access the Clowder dataset, prefix the apiKey value with "?key="
#' @param clowder_dataset A character string for the dataset name
#' apiKey="", clowder_dataset="CCTE ToxValDB PDFs"

#--------------------------------------------------------------------------------------

hess_record_url_from_clowder <- function(toxval.db,source.db, apiKey,clowder_dataset) {
  printCurrentFunction(toxval.db)
  #####################################################################
  cat("Extract data from CCTE ToxValDB PDFs Dataset in clowder\n")
  #####################################################################
  all_clowder_docs <- get_clowder_docList(apiKey, clowder_dataset)
  #####################################################################
  cat("subset based on the required fields\n")
  #####################################################################
  hess_docs_data <- all_clowder_docs[,c("id","filename","date_created")]
  #####################################################################
  cat("extract rows with hess data based on date created in clowder\n")
  #####################################################################
  hess_docs_data <- hess_docs_data[grep("^Mon Jun 28", hess_docs_data$date_created),]
  #####################################################################
  cat("get the url for hess documents\n")
  #####################################################################
  hess_docs_data$doc_url <- ""
  for (i in 1:nrow(hess_docs_data)){
    hess_docs_data$doc_url[i] <- paste("https://clowder.edap-cluster.com/files/",hess_docs_data$id[i],"?dataset=5e31dc1e99323f93a9f5cec0&space=&folder=60da0d4ae4b0a676289dd261" , sep = "") 
    
  }
  write.xlsx(hess_docs_data, paste0(toxval.config()$datapath,"hess/hess_files/hess_record_urls_from_clowder.xlsx"))
}