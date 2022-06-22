# Script to pull Clowder document information to help map Clowder ID values to
# ToxVal records
# By: Jonathan Taylor Wall
# Created: 2022-06-28
# R version 4.1.2 (2021-11-01)
# stringr_1.4.0; plyr_1.8.7; purrr_0.3.4; writexl_1.4.0;
# magrittr_2.0.1; dplyr_1.0.7; httr_1.4.2

#-------------------------------------------------------------------------------------
#' @title get.clowder.file.maps
#' @description Pull Clowder file information to map to toxval records
#' @param apiKey User Clowder API key to access Clowder
#' @return Dataframe list of Clowder ID maps
#' @import httr plyr dplyr stringr magrittr writexl purrr
#' @export
#--------------------------------------------------------------------------------------
get.clowder.file.maps <- function(apiKey){
  # Pull all files from ToxValPDFs Dataset
  r <- httr::GET("https://clowder.edap-cluster.com/api/datasets/5e31dc1e99323f93a9f5cec0/files",
                 # API Key as header instead of query parameter
                 add_headers(`X-API-Key` = apiKey)) %>%
    httr::content() %>% {
      # Format content returned as named dataframe (mapping names)
      data.frame(
        id = purrr::map_chr(., "id"),
        filename = purrr::map_chr(., "filename"),
        size = purrr::map_chr(., "size"),
        date_created = purrr::map_chr(., "date-created"),
        filetype = purrr::map_chr(., "contentType"),
        stringsAsFactors = FALSE
      )
    } %T>% {
      # Store grouping names
      filetypes <<- unlist(dplyr::group_keys(., filetype))
    } %>%
    # Split by filetype into list of dataframes
    dplyr::group_split(filetype) %T>% {
      # Name the list of dataframes
      names(.) <- filetypes
    }

  # ICF uploaded file information
  ICF = r$`application/pdf` %>%
    filter(!grepl("^ToxValQA", filename))
  # CCTE uploaded file information
  toxvalQA = r$`application/pdf` %>%
    filter(grepl("^ToxValQA", filename))

  # Pull metadata for ToxValQA files
  CCTE = lapply(toxvalQA$id, function(id){
    # Rest between requests
    Sys.sleep(0.25)
    httr::GET(paste0("https://clowder.edap-cluster.com/api/files/",id,"/metadata.jsonld"),
              # API Key as header instead of query parameter
              add_headers(`X-API-Key` = apiKey)) %>%
      httr::content() %>%
      data.frame(stringsAsFactors = FALSE) %>%
      mutate(clowder_id = id)
  }) %>%
    # Combine and Fill Columns NA if missing
    plyr::rbind.fill() %>%
    # Filter to user uploaded metadata
    filter(`agent..type` == "cat:user" | `agent..type.1` == "cat:user") %>%
    # Select metadata tags
    select(clowder_id, starts_with("content.")) %>%
    # Rename columns
    dplyr::rename_all(~stringr::str_replace(.,"^content.","")) %>%
    dplyr::rename_all(~stringr::str_replace_all(., "\\.", "_"))

  # Create list to save and return
  out = list(ICF = ICF,
             CCTE = CCTE)

  #writexl::write_xlsx(out, "Repo/clowder_doc_maps.xlsx")
  return(out)
}


