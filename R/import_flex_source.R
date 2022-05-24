#-------------------------------------------------------------------------------------
#' Load the FLEX data (old ACToR data) from files to toxval source. This will load all
#' Excel file in the folder ACToR replacements/
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param filepath The path for all the input xlsx files ./ACToR replacements
#' @param verbose Whether the loaded rows should be printed to the console.
#' @export
#--------------------------------------------------------------------------------------
import_flex_source <- function(toxval.db,filepath,verbose=F) {
  printCurrentFunction(toxval.db)
  
  #####################################################################
  cat("Build all old ACToR tables \n")
  #####################################################################
  files.list <- list.files(path = filepath, pattern = "*.xlsx")
  any_temp_files <- grep("^\\~\\$.*", files.list, value = T)
  files.list <- files.list[! files.list %in% any_temp_files]
  files.list <- paste0( filepath, '/',files.list)
  res <- lapply(files.list,read.xlsx)
  

  names.list <- gsub("(.*)(\\/)(.*)(for.*)","\\3",files.list)
  names.list <- gsub("(.*)(\\_$|\\s+$)","\\1",names.list)
  names.list <- tolower(names.list)
  names.list <- gsub("\\s+","_",names.list)
  names.list[names.list == "penndep"] <- "penn_dep"
  
  names(res) <- names.list
  
  res <- lapply(res, cbind, clowder_id = c("-"))
  res <- lapply(res, cbind, source_hash = c("-"))

  for(i in 1:length(res)){
    res[[i]]$source_id <- seq_along(res[[i]][,1])
  }
  
  for(i in 1:length(res)){
    res[[i]] <- res[[i]][,c("source_id", "source_hash","clowder_id", names(res[[i]])[!names(res[[i]]) %in% c("source_id", "source_hash","clowder_id") ])]
  }
  
  
  stop = FALSE
  for( i in 1:length(res)){
    for (j in 1:length(names.list)){

      runInsertTable(res[[i]],names.list[j],toxval.db,do.halt=T,verbose=F)
      i <- i+1
      if (i == length(res)+1){
        stop = TRUE
        break
      }
    }
    if (stop){break}
  }


  
  
  
}  