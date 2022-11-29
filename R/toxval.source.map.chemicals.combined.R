# Script to push ChemReg curated chemical lists back to toxval_source tables
# By: Jonathan Taylor Wall
# Created: 2022-05-27
# R version 4.1.0 (2021-05-18)
# dplyr_1.0.8; RMySQL_0.10.23; DBI_1.1.2; readxl_1.3.1

#-------------------------------------------------------------------------------------
#' @title toxval.source.map.chemicals.combined
#' @description push ChemReg curated chemicals to toxval_source db tables
#' @param source.db The version of toxval source database to use.
#' @param input.path Path to folder with original chemical lists
#' @param curated.path Path to folder with curated chemical lists
#' @param match.raw Boolean whether to match by raw name/casrn values (Default FALSE)
#' @return None. SQL statements are executed.
#' @import RMySQL dplyr readxl
#' @export
#--------------------------------------------------------------------------------------
toxval.source.map.chemicals.combined <- function(source.db, input.path, curated.path, match.raw=FALSE){
  # message("Function still in draft phase - waiting for curated chemical files")
  # return()
  # Get source chemical table name to ID map
  # input.path = paste0(toxval.config()$datapath, "chemical_mapping/renamed_source_chemical_files/")
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
  # curated.path = paste0(toxval.config()$datapath, "chemical_mapping/DSSTOX_879/")
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
    #src_chem_tbl = source_table_list$source_chem_table[source_table_list$id == tbl_id]
    # Get chemical table for source
    chem_tbl = runQuery(paste0("SELECT * FROM source_chemical where ",
                               "chemical_id like 'ToxVal", tbl_id,"%' ",
                               "and dtxrid is NULL"), db=source.db)

    if(!nrow(chem_tbl)){
      message("No chemicals to map for ", tbl_id, "...skipping!")
      next
    }

    if(match.raw){
      orig_chem_file = curated_list$`DSSTox Files`[grepl(paste0("ToxVal", tbl_id),
                                                         curated_list$`DSSTox Files`)] %>%
        paste0(curated.path, "DSSTox Files/", .) %>%
        readxl::read_xlsx(path=.) %>%
        select(chemical_id = Extenal_ID, dtxrid = DSSTox_Source_Record_Id, quality=`DSSTox_QC-Level`)
    } else {
      orig_chem_file = curated_list$`DSSTox Files`[grepl(paste0("ToxVal", tbl_id),
                                                         curated_list$`DSSTox Files`)] %>%
        paste0(curated.path, "DSSTox Files/", .) %>%
        readxl::read_xlsx(path=.) %>%
        select(chemical_id = Extenal_ID, dtxsid = DSSTox_Substance_Id,
               dtxrid = DSSTox_Source_Record_Id, quality=`DSSTox_QC-Level`)
    }

    # Get BIN file information
    b_file = curated_list$`BIN Files`[grepl(paste0("ToxVal", tbl_id),
                                            curated_list$`BIN Files`)] %>%
      paste0(curated.path, "BIN Files/", .) %>%
      readxl::read_xlsx(path=.) %>%
      # Some BIN files have quotation marks around the query...removing...
      mutate(`Query Name` = gsub("\"", "", `Query Name`))
    # Get Jira cleaned information (connect BIN to external_id)
    # If matching by raw information (first curation round had chemical ID values that changed)
    if(match.raw){
      j_file = curated_list$jira_chemical_files[grepl(paste0("ToxVal", tbl_id, "_full.xlsx"),
                                                      curated_list$jira_chemical_files)] %>%
        paste0(curated.path, "jira_chemical_files/", .) %>%
        readxl::read_xlsx(path=.) %>%
        # Match back to curated query which replaced "-" with NA for CASRN
        mutate(raw_casrn = ifelse(raw_casrn == "-", NA, raw_casrn))

      # Join chemical file information
      chem_map = j_file %>%
        left_join(b_file,
                  by=c("raw_casrn"="Query Casrn",
                       "raw_name"="Query Name")) %>%
        left_join(orig_chem_file,
                  by="chemical_id") %>%
        select(-chemical_id, -raw_casrn, -raw_name) %>%
        mutate(original_casrn = as.character(original_casrn)) %>%
        distinct()

      out = chem_tbl %>%
        select(chemical_id, source, raw_casrn, raw_name, cleaned_casrn, cleaned_name) %>%
        left_join(chem_map,
                  # Joining by raw_name and raw_casrn for now since chemical_id changed
                  # between curation efforts
                  by=c("raw_name"="original_name",
                       "raw_casrn"="original_casrn")) %T>% {
                         # Output an intermediate check for incomplete cases to see if join successful
                         out_check <<- filter(., !complete.cases(.))
                       } #%>%
      # Rename columns casrn, name, dtxsid, dtxrid, quality, flags
      #dplyr::rename()
      # If any incomplete cases aren't "No Hits", error stop...
      # if(any(!out_check$`Lookup Result` %in% c("No Hits"))){
      #   stop("Error processing ", c_list, "...incomplete join cases found...")
      # }

      # Rename/select columns for final push to database
      #chemical_id, casrn, name, dtxsid, dtxrid, quality, flags
      out = out %>%
        select(chemical_id,
               casrn=`Top Hit Casrn`,
               name=`Top Hit Name`,
               dtxsid=`Top HIT DSSTox_Substance_Id`,
               dtxrid,
               quality,
               flags=`Lookup Result`) %>%
        # Filter out ones that didn't map
        filter(!is.na(dtxsid))
    } else {
      j_file = curated_list$jira_chemical_files[grepl(paste0("ToxVal", tbl_id),
                                                      curated_list$jira_chemical_files)] %>%
        paste0(curated.path, "jira_chemical_files/", .) %>%
        readxl::read_xlsx(path=.) %>%
        # Match back to curated query which replaced "-" with NA for CASRN
        mutate(raw_casrn = ifelse(raw_casrn == "-", NA, raw_casrn))

      # Join chemical file information
      chem_map = j_file %>%
        left_join(b_file,
                  by=c("raw_casrn"="Query Casrn",
                       "raw_name"="Query Name")) %>%
        left_join(orig_chem_file,
                  by="chemical_id") %>%
        select(-raw_casrn, -raw_name) %>%
        distinct()

      out = chem_tbl %>%
        select(chemical_id, source, raw_casrn, raw_name) %>%
        left_join(chem_map,
                  # Joining by raw_name and raw_casrn for now since chemical_id changed
                  # between curation efforts
                  by="chemical_id") %T>% {
                         # Output an intermediate check for incomplete cases to see if join successful
                         out_check <<- filter(., !complete.cases(.))
                  }
      # If any incomplete cases aren't "No Hits", error stop...
      # if(any(!out_check$`Lookup Result` %in% c("No Hits"))){
      #   stop("Error processing ", c_list, "...incomplete join cases found...")
      # }

      # Rename/select columns for final push to database
      #chemical_id, casrn, name, dtxsid, dtxrid, quality, flags
      out = out %>%
        select(chemical_id,
               casrn=`Top Hit Casrn`,
               name=`Top Hit Name`,
               dtxsid,
               dtxrid,
               quality,
               flags=`Lookup Result`) %>%
        # Filter out ones that didn't map
        #filter(!is.na(dtxsid)) %>%
        distinct()
    }

    if(nrow(out) > nrow(chem_tbl)){
      message("Error: mapped chemical rows have more than input chemical table data")
      next
    }
    out = out %>%
      filter(!is.na(dtxrid))
    # Push mappings
    for(c_id in out$chemical_id){
      map = out %>%
        filter(chemical_id == c_id) %>%
        # Only push columns that aren't NA
        select(where(~!all(is.na(.))))

      # NA values present for all columns or all but chemical_id, skip
      if(length(map) <= 1){
        message("No record to push...")
        next
      }
      if(nrow(map) > 1){
        message("Error: multiple rows mapped for single chemical ID")
        next
      }

      varSet <- lapply(map %>%
                         select(-chemical_id) %>%
                         names(), function(x){
                           paste0(x, ' = "', map[[x]] %>%
                                    # Escape quotation mark double-prime for names
                                    gsub('"', '\\\\"', .),
                                  '"')
                           }) %>%
        paste0(collapse = ', ')

      paste0("UPDATE source_chemical SET ",
             varSet,
             " WHERE chemical_id = '", c_id, "'") %>%
        runQuery(query=., db=source.db, do.halt = FALSE)
    }

    # Eventually convert to batch INNER JOIN logic
    # Requires CREATE TABLE rights on database to push tmp map table
    # paste0("UPDATE z_test_source_chemical AS t1 ",
    #        "INNER JOIN SELECT * FROM z_tmp_source_chemical_map WHERE ",
    #        "t1.chemical_id in (",
    #        #toString(out$chemical_id),
    #        ") ",
    #        "AS t2 USING (chemical_id) ",
    #        "SET ", varSet,
    #        " WHERE t1.chemical_id in (",
    #        #toString(out$chemical_id),
    #        ") "
    #        )
  }
}

# # First round where chemical_id changed, so match by raw
# toxval.source.map.chemicals.combined(source.db="res_toxval_source_v5",
#                                      input.path=paste0(toxval.config()$datapath,"chemical_mapping/renamed_source_chemical_files/"),
#                                      curated.path=paste0(toxval.config()$datapath,"chemical_mapping/DSSTOX_879/"),
#                                      match.raw=TRUE)
# # Second round where chemical_id was hashed, so don't match by raw
# toxval.source.map.chemicals.combined(source.db="res_toxval_source_v5",
#                                      input.path=paste0(toxval.config()$datapath,"chemical_mapping/renamed_source_chemical_files/"),
#                                      curated.path=paste0(toxval.config()$datapath,"chemical_mapping/DSSTOX_934/"),
#                                      match.raw=FALSE)

