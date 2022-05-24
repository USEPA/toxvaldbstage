library('dplyr')
library('httr')
library('purrr')
library('magrittr')
#--------------------------------------------------------------------------------------

#'@title Get Clowder Document List
#'@description This is a helper function to get a list of documents available in a Clowder dataset
#'@param apiKey The API key required for a user to access the Clowder dataset
#'@param clowder_dataset A character string for the dataset name
#'@return Returns a dataframe with file details of: file size, date_created, file type, file id, and filename
#'@export

#--------------------------------------------------------------------------------------

get_clowder_docList <- function(apiKey , clowder_dataset ){
  #baseurl <- "https://clowder.ncsa.illinois.edu/clowder/api"
  #get all datasets the user can view
  alldatasets <- httr::GET(paste0("https://clowder.edap-cluster.com/api/datasets",apiKey)) %>% httr::content()
  #look at the first (and only dataset)
  df_id = lapply(alldatasets, function(df){
    return(df$`id`[df$description == clowder_dataset])
  }) %>% purrr::compact() %>% unlist()   #look at the first (and only dataset)
  #save all information to a dataframe
  return(fileres <- httr::GET(paste0("https://clowder.edap-cluster.com/api/datasets/",df_id,"/files",apiKey)) %>% 
           httr::content() %>% {
             data.frame(
               id = purrr::map_chr(., "id"),
               filename = purrr::map_chr(., "filename"),
               size = purrr::map_chr(., "size"),
               date_created = purrr::map_chr(., "date-created"),
               filetype = purrr::map_chr(., "contentType"),       
               stringsAsFactors = F
             )
           }
  )
}





