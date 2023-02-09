#-------------------------------------------------------------------------------------
#' @title toxval.source_push_mapped_chemicals
#' @description Orchestrates the push of mapped chemical information to the selected toxval_source database.
#' Uses the map_curated_chemicals() helper function to generate mapped input.
#' @param db The version of toxval source database to use.
#' @param source.index The source chemical index. Can be full or just numeric (ex. ToxVal00001 vs. 00001)
#' @param curated.path Input path to the folder directory with expected subdirectories of
#' 'BIN Files', 'DSSTox Files', and 'jira_chemical_files'
#' @param ignore.curation.dups Boolean whether to match with any curated records flagged as "unresolved duplicates" (Default TRUE)
#' @param match.chemical.id Boolean whether to match by provided chemical_id external identifier (Default FALSE)
#' @param reset.mapping Boolean whether to reset chemical mappings in source_chemical table of database
#' @return None. Update SQL statements are executed.
#' @import RMySQL dplyr readxl magrittr
#--------------------------------------------------------------------------------------
toxval.source_push_mapped_chemicals <- function(db, source.index, curated.path, ignore.curation.dups=TRUE, match.chemical.id=FALSE, reset.mapping=FALSE){

  # Map chemical information from curated files and select source.index
  out = map_curated_chemicals(source.index=source.index, curated.path=curated.path,
                              ignore.curation.dups=ignore.curation.dups, match.chemical.id = match.chemical.id)

  # Get chemical table for source
  chem_tbl = runQuery(paste0("SELECT * FROM source_chemical where ",
                             "chemical_id like 'ToxVal", source.index,"%'"), db=db)

  # Check for duplicates
  if(nrow(out) > nrow(chem_tbl)){
    message("Error: mapped chemical rows have more than input chemical table data")
    browser()
  }
  # Filter only to those with DTXRID values
  out = out %>%
    filter(!is.na(dtxrid))
  # If not resetting, only filter to what is missing a mapping in the database
  if(!reset.mapping){
    out = out %>%
      filter(chemical_id %in% chem_tbl$chemical_id[is.na(chem_tbl$dtxrid)])
  }
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
      runQuery(query=., db=db, do.halt = FALSE)
  }
}


#-------------------------------------------------------------------------------------
#' @title map_curated_chemicals
#' @description Helper function to attempt to map raw/cleaned chemical information to ChemReg
#' returned curated files.
#' @param source.index The source chemical index. Can be full or just numeric (ex. ToxVal00001 vs. 00001)
#' @param curated.path Input path to the folder directory with expected subdirectories of
#' 'BIN Files', 'DSSTox Files', and 'jira_chemical_files'
#' @param ignore.curation.dups Boolean whether to match with any curated records flagged as "unresolved duplicates" (Default TRUE)
#' @param match.chemical.id Boolean whether to match by provided chemical_id external identifier (Default FALSE)
#' @return Mapped chemical information based on input file directory file chemical information
#' @import RMySQL dplyr readxl magrittr
#--------------------------------------------------------------------------------------
map_curated_chemicals <- function(source.index, curated.path, ignore.curation.dups = TRUE, match.chemical.id = FALSE){
  # curated.path = "Repo\\chemical_mapping\\DSSTOX_1142"
  if(is.null(source.index) || is.na(source.index)) return("Must provide 'source.index'...")
  # Clean up source.index to only the numeric value
  source.index = gsub("[^0-9.-]", "", source.index)

  c_dirs = list.dirs(curated.path, recursive = FALSE)
  curated_list = lapply(c_dirs, function(d){
    tmp = list.files(d) %>%
      .[grepl(source.index, .)]
    # Remove Windows temp files that start with "~"
    return(tmp[!grepl("^~", tmp)])
  }) %T>% { names(.) <- basename(c_dirs) }

  out = lapply(curated_list$`DSSTox Files`, function(c_list){
    message("...mapping file: ", c_list)
    # Load required files from curation
    c_files = list(orig_file = curated_list$jira_chemical_files[grepl(source.index, curated_list$jira_chemical_files)] %>%
                     paste0(curated.path, "/jira_chemical_files/", .),
                   b_file = curated_list$`BIN Files`[grepl(source.index, curated_list$`BIN Files`)] %>%
                     paste0(curated.path, "/BIN Files/", .),
                   d_file = curated_list$`DSSTox Files`[grepl(source.index, curated_list$`DSSTox Files`)] %>%
                     paste0(curated.path, "/DSSTox Files/", .))

    # Check files exist (need all 3)
    if(!all(lapply(c_files, file.exists) %>% unlist())) return("Missing input file...")
    # Load input files
    c_files = lapply(c_files, readxl::read_xlsx) %T>%
      { names(.) <- names(c_files) }

    # Select and rename columns
    c_files$b_file = c_files$b_file %>%
      dplyr::rename(flags=`Lookup Result`,
                    name=`Query Name`,
                    casrn=`Query Casrn`,
                    dtxsid=`Top HIT DSSTox_Substance_Id`) %>%
      distinct() %>%
      mutate(name = gsub("\"", "", name))
    # Select and rename columns
    c_files$d_file = c_files$d_file %>%
      select(chemical_id = Extenal_ID,
             dtxsid = DSSTox_Substance_Id,
             dtxrid = DSSTox_Source_Record_Id,
             quality=`DSSTox_QC-Level`) %>%
      distinct()

    # --- Collect matches ---
    out = data.frame()

    if(match.chemical.id){
      tmp = c_files$orig_file %>%
        select(chemical_id) %>%
        left_join(c_files$d_file,
                  by="chemical_id") %>%
        left_join(c_files$b_file %>%
                    select(dtxsid, flags, `Top Hit Name`, `Top Hit Casrn`) %>%
                    distinct(),
                  by="dtxsid") %>%
        # Filter out ones that didn't map
        filter(!is.na(dtxrid) | !is.na(flags))
      # Store matches
      out = bind_rows(out, tmp)
      # Remove previous matches
      c_files$orig_file = c_files$orig_file %>%
        filter(!chemical_id %in% out$chemical_id)
    }

    # Scenario #1 Match by clean names AND casrn
    if(all(c("cleaned_name", "cleaned_casrn") %in% names(c_files$orig_file))){
      tmp = c_files$orig_file %>%
        select(chemical_id, cleaned_casrn, cleaned_name) %>%
        left_join(c_files$b_file,
                  by=c("cleaned_casrn"="casrn",
                       "cleaned_name"="name")) %>%
        left_join(c_files$d_file,
                  by="dtxsid") %>%
        # Filter out ones that didn't map
        filter(!is.na(dtxrid) | !is.na(flags))
      # Store matches
      out = bind_rows(out, tmp)
      # Remove previous matches
      c_files$orig_file = c_files$orig_file %>%
        filter(!chemical_id %in% out$chemical_id)
    }

    if(all(c("cleaned_name") %in% names(c_files$orig_file))){
      # Scenario #2 Match by clean names only
      tmp = c_files$orig_file %>%
        select(chemical_id, cleaned_name) %>%
        left_join(c_files$b_file,
                  by=c("cleaned_name"="name")) %>%
        left_join(c_files$d_file,
                  by="dtxsid") %>%
        # Filter out ones that didn't map
        filter(!is.na(dtxrid) | !is.na(flags))
      # Store matches
      out = bind_rows(out, tmp)
      # Remove previous matches
      c_files$orig_file = c_files$orig_file %>%
        filter(!chemical_id %in% out$chemical_id)
    }

    if(all(c("raw_name", "raw_casrn") %in% names(c_files$orig_file))){
      # Scenario #3 Match by raw names and casrn
      tmp = c_files$orig_file %>%
        select(chemical_id, raw_casrn, raw_name) %>%
        left_join(c_files$b_file,
                  by=c("raw_casrn"="casrn",
                       "raw_name"="name")) %>%
        left_join(c_files$d_file,
                  by="dtxsid") %>%
        # Filter out ones that didn't map
        filter(!is.na(dtxrid) | !is.na(flags))
      # Store matches
      out = bind_rows(out, tmp)
      # Remove previous matches
      c_files$orig_file = c_files$orig_file %>%
        filter(!chemical_id %in% out$chemical_id)
    }

    if(all(c("raw_name") %in% names(c_files$orig_file))){
      # Scenario #4 Match by raw names only
      tmp = c_files$orig_file %>%
        select(chemical_id, raw_name) %>%
        left_join(c_files$b_file,
                  by=c("raw_name"="name")) %>%
        left_join(c_files$d_file,
                  by="dtxsid") %>%
        # Filter out ones that didn't map
        filter(!is.na(dtxrid) | !is.na(flags))
      # Store matches
      out = bind_rows(out, tmp)
      # Remove previous matches
      c_files$orig_file = c_files$orig_file %>%
        filter(!chemical_id %in% out$chemical_id)
    }

    # Filter out duplicate curation records if desired
    if(ignore.curation.dups){
      out = out %>%
        filter(flags != "Unresolved Duplicates")
    }

    # Rename/select fields and return
    out %>%
      select(chemical_id,
             casrn=`Top Hit Casrn`,
             name=`Top Hit Name`,
             dtxsid,
             dtxrid,
             quality,
             flags) %>%
      distinct() %>%
      return()
  }) %>%
    # Combine all subsets of a given source.index curation effort
    bind_rows()
  message("...returning...")
  return(out)
}
