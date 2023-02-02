# Script to push ChemReg curated chemical lists back to toxval_source tables
# Deprecated 2023-02-02 with Jira Ticket TOXVAL-353 in favor of toxval.source_push_mapped_chemicals.R
# By: Jonathan Taylor Wall
# Created: 2022-05-27
# R version 4.1.0 (2021-05-18)
# dplyr_1.0.8; RMySQL_0.10.23; DBI_1.1.2; readxl_1.3.1

#-------------------------------------------------------------------------------------
#' @title toxval.source.map.chemicals
#' @description push ChemReg curated chemicals to toxval_source db tables
#' @param source.db The version of toxval source database to use.
#' @param input.path Path to folder with original chemical lists
#' @param curated.path Path to folder with curated chemical lists
#' @return None. SQL statements are executed.
#' @import RMySQL dplyr readxl
#--------------------------------------------------------------------------------------
toxval.source.map.chemicals <- function(source.db, input.path, curated.path){
  message("Function still in draft phase - waiting for curated chemical files")
  return()
  # Get source chemical table name to ID map
  # Repo/chemical_mapping/renamed_source_chemical_files/
  source_table_list = list.files(input.path)
  source_table_list = lapply(source_table_list, function(t){
    t %>%
      # Remove file extension
      gsub(".xlsx", "", .) %>%
      # Split by "_"
      strsplit(., split="_") %>%
      unlist() %>%
    # Restructure split string parts into table
    data.frame(id = paste0(.[length(.)]),
               source_chem_table = paste(., collapse="_") %>% tolower(),
               stringsAsFactors = FALSE) %>%
      select(id, source_chem_table) %>%
      distinct() %>%
      # Remove parentheses from table name
      mutate(source_chem_table = gsub("[()]", "", source_chem_table) %>%
               gsub("-", "_", .)) %>%
      return()
  }) %>% dplyr::bind_rows()

  # Get curated chemical lists to map - takes 3 file sets per source table
  #Repo/chemical_mapping/DSSTOX_879/
  c_dirs = list.dirs(curated.path, recursive = FALSE)
  curated_list = lapply(c_dirs, function(d){
    tmp = list.files(d)
    # Remove Windows temp files that start with "~"
    return(tmp[!grepl("^~", tmp)])
  }) %T>% { names(.) <- basename(c_dirs) }


  for(c_list in curated_list$`DSSTox Files`){
    cat(paste0("...Processing curation file: ", c_list, "\n"))
    # Get curated chemical list
    chems = readxl::read_xlsx(paste0(curated.path, "DSSTox Files/", c_list)) %>%
      dplyr::rename(External_ID = Extenal_ID) %>%
      select(DSSTox_Source_Record_Id, External_ID, DSSTox_Substance_Id)
    # Get toxval source table ID from external ID
    tbl_id = strsplit(chems$External_ID[1], split="_")[[1]][1] %>%
      gsub("ToxVal", "", .)
    # Get toxval source chemical table name to query
    src_chem_tbl = source_table_list$source_chem_table[source_table_list$id == tbl_id]
    # Get chemical table for source
    chem_tbl = runQuery(paste0("SELECT * FROM ", src_chem_tbl), db=source.db)
    # Get BIN file information
    b_file = curated_list$`BIN Files`[grepl(paste0("ToxVal", tbl_id),
                                            curated_list$`BIN Files`)] %>%
      paste0(curated.path, "BIN Files/", .) %>%
      readxl::read_xlsx(path=.)
    # Get Jira cleaned information (connect BIN to external_id)
    j_file = curated_list$jira_chemical_files[grepl(paste0("ToxVal", tbl_id, ".xlsx"),
                                                    curated_list$jira_chemical_files)] %>%
      paste0(curated.path, "jira_chemical_files/", .) %>%
      readxl::read_xlsx(path=.) %>%
      # Match back to curated query which replaced "-" with NA for CASRN
      mutate(raw_casrn = ifelse(raw_casrn == "-", NA, raw_casrn))

    # Join chemical file information
    out = chem_tbl %>%
      # Join to joined Jira and BIN file
      left_join(j_file %>%
                  left_join(b_file,
                            by=c("raw_casrn"="Query Casrn",
                                 "raw_name"="Query Name")) %>%
                  select(-raw_casrn, -raw_name),
                by="chemical_id") %>%
      left_join(chems, by=c("chemical_id"="External_ID")) %T>% {
        # Output an intermediate check for incomplete cases to see if join successful
        out_check <<- filter(., !complete.cases(.))
      }
    # If any incomplete cases aren't "No Hits", error stop...
    if(any(!out_check$`Lookup Result` %in% c("No Hits"))){
      stop("Error processing ", c_list, "...incomplete join cases found...")
    }
    # # Drop old chemical table (we pulled it into the new table to be written)
    # runStatement(paste0("DROP TABLE ", src_chem_tbl), db=source.db)
    # # Replace with new chemical table
    # runInsertTable(chem_tbl,src_chem_tbl,source.db,do.halt=TRUE,verbose=FALSE)
  }
}
