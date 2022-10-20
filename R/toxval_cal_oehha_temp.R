if (source == "CAL_OEHHA"){
  # cut the map down to just the webpage PDF documents, no screenshots
  map_file <- map_file %>%
    filter(subDir1 == "pdf") %>%
    rename(clowder_id = "uuid", document_name = "File Name")
  # clear old names
  res$clowder_id = NULL
  res$document_name = NULL
  # Match by chemical name first
  res = res %>%
    left_join(map_file %>%
                select(Chemical, clowder_id, document_name),
              by=c("name" = "Chemical"))
  # Filter to those without a match
  res2 = res %>%
    filter(is.na(clowder_id))
  res = res %>%
    filter(!is.na(clowder_id))
  # Match by casrn
  res2 = res2 %>%
    select(-clowder_id, -document_name) %>%
    left_join(map_file %>%
                select(casrn, clowder_id, document_name),
              by="casrn")
  # Recombine all matches
  res = rbind(res, res2)
  # Report any that did not match
  if(any(is.na(res$clowder_id))){
    cat("IRIS records not matched to Clowder ID: ", nrow(res[is.na(res$clowder_id),]))
  }
}
