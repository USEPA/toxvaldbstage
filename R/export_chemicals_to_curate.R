#' @title export_chemicals_to_curate
#' @description Export XLSX files by source for chemical curation
#' @param db Version of toxval_source to use
#' @param export_all Whether to export all chemicals, Default: FALSE
#' @return None
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  [separate][tidyr::separate]
#'  [bind_rows][dplyr::bind_rows], [filter][dplyr::filter], [select][dplyr::select], [mutate][dplyr::mutate], [rowwise][dplyr::rowwise], [case_when][dplyr::case_when], [ungroup][dplyr::ungroup], [group_split][dplyr::group_split]
#'  [read_xlsx][readxl::read_xlsx]
#'  [write_xlsx][writexl::write_xlsx]
#' @rdname export_chemicals_to_curate
#' @export
#' @importFrom tidyr separate
#' @importFrom dplyr bind_rows filter select mutate rowwise case_when ungroup group_split
#' @importFrom readxl read_xlsx
#' @importFrom writexl write_xlsx
export_chemicals_to_curate <- function(db, export_all=FALSE){

  message("Pulling chemicals to curate...")
  chems_curate <- runQuery(paste0("SELECT chemical_id as external_id, raw_name,	",
                                  "raw_casrn, cleaned_name, cleaned_casrn ",
                                  "FROM source_chemical ",
                                  "WHERE DTXSID is NULL"),
                           db=db) %>%
    tidyr::separate(external_id, into=c("chemical_index", "chemical_hash"), sep="_", remove=FALSE)

  if(!export_all){
    message("...Filtering to only active Chemical ID entries for curation...")
    # Pull chemical ID values to curate
    tbl_list <- runQuery(paste0("SELECT source_table FROM chemical_source_index ",
                                "WHERE source_table is not NULL and source_status = 'active'"),
                         db=db)

    message("......Pulling chemical IDs from active source tables...")
    # Get list of chemical_id values to check
    active_chem_ids <- lapply(tbl_list$source_table, function(src){
      # Check if table exists
      tbl_check <- runQuery(paste0("SHOW TABLES LIKE '", src, "'"),
                            db=db)
      if(nrow(tbl_check)){
        paste0("SELECT chemical_id FROM ", src, " WHERE qc_status not like '%fail%'") %>%
          runQuery(., db=db) %>%
          return()
      }
    }) %>%
      dplyr::bind_rows()

    # Filter to only active Chemical ID values
    chems_curate <- chems_curate %>%
      dplyr::filter(external_id %in% active_chem_ids$chemical_id)

    # Pull list of curated DSSTOX files
    dsstox_files = list.files(paste0(toxval.config()$datapath, "chemical_mapping/"),
                              full.names = TRUE,
                              recursive = TRUE,
                              pattern = "DSSTox_") %>%
      .[grepl("xlsx$", .)]

    dsstox_data = lapply(dsstox_files, function(f){
      tmp = readxl::read_xlsx(f)
      if(!"Extenal_ID" %in% names(tmp)) return(NULL)
      tmp %>%
        dplyr::select(Extenal_ID) %>%
        dplyr::mutate(Extenal_ID = as.character(Extenal_ID)) %>%
        unique() %>%
        dplyr::filter(grepl("ToxVal", Extenal_ID)) %>%
        return()
    }) %>%
      dplyr::bind_rows()

    # Filter out any previously attempted for curation
    chems_curate = chems_curate %>%
      dplyr::filter(!external_id %in% dsstox_data$Extenal_ID)
  }

  # Export into subfiles by chemical index
  out = chems_curate %>%
    dplyr::rowwise() %>%
    # check_sum replace casrn with "-" if failed
    dplyr::mutate(check_sum = cas_checkSum(cleaned_casrn),
                  cleaned_casrn = dplyr::case_when(
                    check_sum %in% c(0, NA) ~ "-",
                    TRUE ~ cleaned_casrn
                  )) %>%
    dplyr::ungroup() %>%
    dplyr::group_split(chemical_index)

  # Prepare output directory
  if(!dir.exists(paste0(toxval.config()$datapath, "chemical_mapping/to_ticket_dsstox"))){
    dir.create(paste0(toxval.config()$datapath, "chemical_mapping/to_ticket_dsstox"), recursive = TRUE)
  }

  message("Exporting chemicals to curate...")
  for(f in out){
    writexl::write_xlsx(f %>%
                          dplyr::select(-chemical_index, -chemical_hash),
                        paste0(toxval.config()$datapath, "chemical_mapping/to_ticket_dsstox/",
                               f$chemical_index %>% unique(),
                               # "_", digest::digest(Sys.Date()) %>% stringr::str_sub(1, 4),
                               "_a_",
                               Sys.Date() %>%
                                 gsub("-", "", .),
                               ".xlsx"))
  }
}
