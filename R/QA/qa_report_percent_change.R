library(dplyr)

#--------------------------------------------------------------------------------------
#' Analyze QC completed data files to determine percentage change based on source_hash
#' difference.
#'
#' @import dplyr readxl
#' @return DataFrame summary by source name
#--------------------------------------------------------------------------------------
qa_report_percent_change <- function(){
  f_list = list.files(paste0(toxval.config()$datapath, "QC Pushed"), full.names = TRUE)

  lapply(f_list, function(f){
    tmp0 = readxl::read_xlsx(f, "live")
    tmp = tmp0 %>%
      select(source_hash, parent_hash) %>%
      filter(parent_hash != "-", !is.na(parent_hash)) %>%
      mutate(compare = source_hash != parent_hash)

    data.frame(stringsAsFactors = FALSE,
               source_name = basename(f),
               n_records = nrow(tmp0),
               n_change = tmp %>% filter(compare == TRUE) %>% nrow()) %>%
      mutate(perc_change = round((n_change/n_records)*100, 3)) %>%
      return()
  }) %>%
    dplyr::bind_rows() %>%
    return()
}

