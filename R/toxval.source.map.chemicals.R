# Script to push ChemReg curated chemical lists back to toxval_source tables
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
#' @export
#--------------------------------------------------------------------------------------
toxval.source.map.chemicals <- function(source.db, curated.path){
  message("Function still in draft phase - waiting for curated chemical files")
  return()
  # Get source table map files - clean up to match database chemical tables
  # Repo/chemical_mapping/renamed_source_chemical_files/
  source_table_list = list.files(input.path)
  source_table_list = lapply(source_table_list, function(t){
    t %>%
      gsub(".xlsx", "", .) %>%
      strsplit(., split="_") %>%
      unlist() %>%
    # Restructure split string parts into table
    data.frame(id = paste0(.[length(.)]),
               source_chem_table = paste(., collapse="_") %>% tolower(),
               stringsAsFactors = FALSE) %>%
      select(id, source_chem_table) %>%
      distinct() %>%
      # Remove parentheses from name
      mutate(source_chem_table = gsub("[()]", "", source_chem_table) %>%
               gsub("-", "_", .)) %>%
      return()
  }) %>% dplyr::bind_rows()

  # Get curated chemical lists to map
  #Repo/chemical_mapping/jira_chemical_files
  curated_list = list.files(curated.path,
                            pattern="_full.xlsx")

  for(c_list in curated_list){
    chems = readxl::read_xlsx(paste0(curated.path, c_list))
    tbl_id = c_list %>%
      gsub("ToxVal", "", .) %>%
      gsub("_full.xlsx", "", .)
    src_chem_tbl = source_table_list$source_chem_table[source_table_list$id == tbl_id]
    # Join chemical table with curated chemicals
    chem_tbl = runQuery(paste0("SELECT * FROM ", src_chem_tbl), db=source.db) %>%
      left_join(chems, by=c("chemical_id", "raw_name", "raw_casrn"))
    # Drop old chemical table
    # runStatement(paste0("DROP TABLE ", src_chem_tbl), db=source.db)
    # Write new chemical table
    #runInsertTable(chem_tbl,src_chem_tbl,source.db,do.halt=TRUE,verbose=FALSE)
  }
}
