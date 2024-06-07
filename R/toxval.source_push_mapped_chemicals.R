#-------------------------------------------------------------------------------------
#' @title toxval.source_push_mapped_chemicals
#' @description Orchestrates the push of mapped chemical information to the selected toxval_source database.
#' Uses the map_curated_chemicals() helper function to generate mapped input.
#' @param db The version of toxval source database to use.
#' @param source.index The source chemical index. Can be full or just numeric (ex. ToxVal00001 vs. 00001)
#' @param curated.path Input path to the folder directory with expected subdirectories of #' 'BIN Files', 'DSSTox Files', and 'jira_chemical_files'
#' @param ignore.curation.dups Boolean whether to match with any curated records flagged as "unresolved duplicates" (Default FALSE)
#' @param match.chemical.id Boolean whether to match by provided chemical_id external identifier (Default TRUE)
#' @param reset.mapping Boolean whether to reset chemical mappings in source_chemical table of database
#' @param bulk.push Boolean whether to bulk push updates, or one at a time. Default is TRUE
#' @return None. Update SQL statements are executed.
#' @import RMySQL dplyr readxl magrittr
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[dplyr]{rename}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{bind}}
#' @rdname toxval.source_push_mapped_chemicals
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr rename distinct mutate select left_join filter bind_rows
#--------------------------------------------------------------------------------------
toxval.source_push_mapped_chemicals <- function(db, source.index, curated.path,
                                                ignore.curation.dups = FALSE, match.chemical.id = TRUE,
                                                reset.mapping = FALSE, bulk.push = TRUE){

  message("Pushing mapped chemicals for chemical_source_index ToxVal", source.index)
  # Map chemical information from curated files and select source.index
  out = map_curated_chemicals(source.index=source.index, curated.path=curated.path,
                              ignore.curation.dups=ignore.curation.dups, match.chemical.id = match.chemical.id) %>%
    dplyr::group_by(chemical_id) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(names(.)[!names(.) %in% c("chemical_id")]),
                                # Ensure unique entries in alphabetic order
                                # Collapse with unique delimiter
                                ~paste0(sort(unique(.[!is.na(.)])), collapse="|::|") %>%
                                  dplyr::na_if("NA") %>%
                                  dplyr::na_if("")
    )) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()

  # Check De-duping collapse (only expect flags field to collapse)
  dup_collapsed_fields = lapply(names(out), function(f){
    if(sum(stringr::str_detect(out[[f]], '\\|::\\|'), na.rm = TRUE) > 0){
      return(f)
    }
  }) %>%
    purrr::compact() %>%
    unlist()

  # Error if any other field collapsed besides flags
  if(any(names(out)[!names(out) %in% c("flags")] %in% dup_collapsed_fields)){
    message("Duplicate entries found/collapsed beyond 'flags' field...need to resolve...")
    browser()
    stop("Duplicate entries found/collapsed beyond 'flags' field...need to resolve...")
  }

  out = out %>%
    # Replace unique delimiter with standard delimiter after checking passed
    dplyr::mutate(flags = flags %>%
                    gsub("|::|", "; ", ., fixed=TRUE))

  # Get chemical table for source
  chem_tbl = runQuery(paste0("SELECT * FROM source_chemical where ",
                             "chemical_id like 'ToxVal", source.index,"%'"), db=db)

  out = out %>%
    # Double-check chemical_id values from curation files still exist
    dplyr::filter(chemical_id %in% chem_tbl$chemical_id) %>%
    # Filter out duplicates
    dplyr::group_by(chemical_id) %>%
    dplyr::filter(n()==1) %>%
    dplyr::ungroup()

  # Check for duplicates
  if(nrow(out) > nrow(chem_tbl)){
    message("Error: mapped chemical rows have more than input chemical table data")
    browser()
  }
  # Filter only to those with DTXRID values
  out = out %>%
    dplyr::filter(!is.na(dtxrid))
  # Fill in blanks
  out$name[is.na(out$name)] = "-"
  out$casrn[is.na(out$casrn)] = "-"
  # If not resetting, only filter to what is missing a mapping in the database
  if(!reset.mapping){
    out = out %>%
      dplyr::filter(chemical_id %in% chem_tbl$chemical_id[is.na(chem_tbl$dtxrid)])
  }

  if(!nrow(out)){
    message("...no new chemical maps to push. Set reset.mapping to TRUE to reset mappings")
    return()
  }
  # Clean curated chemical information (sometimes has utf8 encoding issues)
  result = chem.check.v2(out, source="-", verbose=FALSE)
  out = result$res0

  if(bulk.push){
    updateQuery = paste0("UPDATE source_chemical a INNER JOIN z_updated_df b ",
                         "ON (a.chemical_id = b.chemical_id) SET ",
                         paste0("a.", names(out)[!names(out) %in% c("chemical_id")],
                                " = b.", names(out)[!names(out) %in% c("chemical_id")], collapse = ", ")
    )
    runUpdate(table="source_chemical", updateQuery=updateQuery, updated_df=out, db=db)
  } else {
    # Push mappings
    for(i in seq_len(nrow(out))){
      #for(c_id in out$chemical_id){
      if(i%%200==0) cat(i," out of ",nrow(out),"\n")
      c_id = out$chemical_id[i]
      map = out %>%
        dplyr::filter(chemical_id == c_id) %>%
        # Only push columns that aren't NA
        dplyr::select(tidyselect::where(~!all(is.na(.))))

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
                         dplyr::select(-chemical_id) %>%
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
}


#-------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
map_curated_chemicals <- function(source.index, curated.path, ignore.curation.dups = TRUE, match.chemical.id = FALSE){
  # curated.path = "Repo\\chemical_mapping\\DSSTOX_1142"
  if(is.null(source.index) || is.na(source.index)) return("Must provide 'source.index'...")
  # Clean up source.index to only the numeric value
  source.index = gsub("[^0-9.-]", "", source.index)

  c_dirs = list.dirs(curated.path, recursive = FALSE)
  curated_list = lapply(c_dirs, function(d){
    tmp = list.files(d, pattern=".xlsx") %>%
      .[grepl(source.index, .)]
    # Remove Windows temp files that start with "~"
    return(tmp[!grepl("^~", tmp)])
  }) %T>% { names(.) <- basename(c_dirs) }

  out = lapply(curated_list$`DSSTox Files`, function(c_list){
    message("...mapping file: ", c_list)
    # Get specific sub source.index (e.g., _a, _b, _c)
    curated_source.index = c_list %>%
      gsub("DSSTox_ToxVal", "", .) %>%
      strsplit("_") %>% unlist()
    curated_source.index = paste0(curated_source.index[1], "_", curated_source.index[2])
    # Load required files from curation
    c_files = list(orig_file = curated_list$jira_chemical_files[grepl(curated_source.index, curated_list$jira_chemical_files)] %>%
                     paste0(curated.path, "/jira_chemical_files/", .),
                   b_file = curated_list$`BIN Files`[grepl(curated_source.index, curated_list$`BIN Files`)] %>%
                     paste0(curated.path, "/BIN Files/", .),
                   d_file = curated_list$`DSSTox Files`[grepl(curated_source.index, curated_list$`DSSTox Files`)] %>%
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
      dplyr::distinct() %>%
      dplyr::mutate(name = gsub("\"", "", name))
    # Select and rename columns
    c_files$d_file = c_files$d_file %>%
      dplyr::select(chemical_id = Extenal_ID,
             dtxsid = DSSTox_Substance_Id,
             dtxrid = DSSTox_Source_Record_Id,
             quality=`DSSTox_QC-Level`) %>%
      dplyr::distinct()

    # --- Collect matches ---
    out = data.frame()

    if(match.chemical.id){
      if("external_id" %in% names(c_files$orig_file)){
        c_files$orig_file = c_files$orig_file %>%
          dplyr::rename(chemical_id = external_id)
      }
      tmp = c_files$orig_file %>%
        dplyr::select(chemical_id) %>%
        dplyr::left_join(c_files$d_file,
                  by="chemical_id") %>%
        # Filter out ones that didn't map
        dplyr::filter(!is.na(dtxrid)) %>%
        dplyr::left_join(c_files$b_file %>%
                    dplyr::select(dtxsid, flags, `Top Hit Name`, `Top Hit Casrn`) %>%
                    dplyr::distinct(),
                  by="dtxsid") %>%
        # Filter out ones that didn't map
        dplyr::filter(!is.na(dtxrid) | !is.na(flags))
      # Store matches
      out = dplyr::bind_rows(out, tmp)
      # Remove previous matches
      c_files$orig_file = c_files$orig_file %>%
        dplyr::filter(!chemical_id %in% out$chemical_id)
    } else {
      # Remove unneeded chemical ID column
      c_files$d_file = c_files$d_file %>%
        dplyr::mutate(chemical_id = NULL) %>%
        # Also filter out NA dtxSid values that will create false matches
        # Sacrifice not being able to map a dtxRid to records...
        # filter(!is.na(dtxsid)) %>%
        dplyr::distinct()
    }

    # Scenario #1 Match by clean names AND casrn
    if(all(c("cleaned_name", "cleaned_casrn") %in% names(c_files$orig_file))){
      tmp = c_files$orig_file %>%
        dplyr::select(chemical_id, cleaned_casrn, cleaned_name) %>%
        dplyr::left_join(c_files$b_file,
                  by=c("cleaned_casrn"="casrn",
                       "cleaned_name"="name")) %>%
        dplyr::left_join(c_files$d_file,
                  by="dtxsid") %>%
        # Filter out ones that didn't map
        dplyr::filter(!is.na(dtxrid) | !is.na(flags))
      # Store matches
      out = dplyr::bind_rows(out, tmp)
      # Remove previous matches
      c_files$orig_file = c_files$orig_file %>%
        dplyr::filter(!chemical_id %in% out$chemical_id)
    }

    if(all(c("cleaned_name") %in% names(c_files$orig_file))){
      # Scenario #2 Match by clean names only
      tmp = c_files$orig_file %>%
        dplyr::select(chemical_id, cleaned_name) %>%
        dplyr::left_join(c_files$b_file,
                  by=c("cleaned_name"="name")) %>%
        dplyr::left_join(c_files$d_file,
                  by="dtxsid") %>%
        # Filter out ones that didn't map
        dplyr::filter(!is.na(dtxrid) | !is.na(flags))
      # Store matches
      out = dplyr::bind_rows(out, tmp)
      # Remove previous matches
      c_files$orig_file = c_files$orig_file %>%
        dplyr::filter(!chemical_id %in% out$chemical_id)
    }

    if(all(c("raw_name", "raw_casrn") %in% names(c_files$orig_file))){
      # Scenario #3 Match by raw names and casrn
      tmp = c_files$orig_file %>%
        dplyr::select(chemical_id, raw_casrn, raw_name) %>%
        dplyr::left_join(c_files$b_file,
                  by=c("raw_casrn"="casrn",
                       "raw_name"="name")) %>%
        dplyr::left_join(c_files$d_file,
                  by="dtxsid") %>%
        # Filter out ones that didn't map
        dplyr::filter(!is.na(dtxrid) | !is.na(flags))
      # Store matches
      out = dplyr::bind_rows(out, tmp)
      # Remove previous matches
      c_files$orig_file = c_files$orig_file %>%
        dplyr::filter(!chemical_id %in% out$chemical_id)
    }

    if(all(c("raw_name") %in% names(c_files$orig_file))){
      # Scenario #4 Match by raw names only
      tmp = c_files$orig_file %>%
        dplyr::select(chemical_id, raw_name) %>%
        dplyr::left_join(c_files$b_file,
                  by=c("raw_name"="name")) %>%
        dplyr::left_join(c_files$d_file,
                  by="dtxsid") %>%
        # Filter out ones that didn't map
        dplyr::filter(!is.na(dtxrid) | !is.na(flags))
      # Store matches
      out = dplyr::bind_rows(out, tmp)
      # Remove previous matches
      c_files$orig_file = c_files$orig_file %>%
        dplyr::filter(!chemical_id %in% out$chemical_id)
    }

    # Filter out duplicate curation records if desired
    if(ignore.curation.dups){
      out = out %>%
        dplyr::filter(flags != "Unresolved Duplicates")
    }

    # Rename/select fields and return
    out %>%
      dplyr::select(chemical_id,
             casrn=`Top Hit Casrn`,
             name=`Top Hit Name`,
             dtxsid,
             dtxrid,
             quality,
             flags) %>%
      dplyr::distinct() %>%
      return()
  }) %>%
    # Combine all subsets of a given source.index curation effort
    dplyr::bind_rows()
  message("...returning...")
  return(out)
}
