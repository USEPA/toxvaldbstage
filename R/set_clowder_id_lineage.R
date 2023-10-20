#--------------------------------------------------------------------------------------
#' @description Create document records and associations in toxval_source based on input source table and document map
#' @param source_table The source table name (e.g. source_test)
#' @param map_file A dataframe of Clowder document mapping info. If NULL, will try to load a hardcoded map for the source
#' @param map_clowder_id_field Column name for the Clowder ID field of the map
#' @param clowder_url URL to Clowder
#' @param clowder_api_key API key to access Clowder resources
#' @param sync_clowder_metadata Boolean whether to sync Clowder metadata for new document records. Default is False.
#' @return Returns an updated map with newly associated toxval_source table ID values
#' @title FUNCTION_TITLE
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[dplyr]{rename}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{mutate}}
#'  \code{\link[tidyr]{separate_rows}}, \code{\link[tidyr]{unite}}
#' @rdname set_clowder_id_lineage
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr rename filter select left_join mutate
#' @importFrom tidyr separate_rows unite
#--------------------------------------------------------------------------------------
set_clowder_id_lineage <- function(source_table,
                                   map_clowder_id_field,
                                   map_file,
                                   clowder_url,
                                   clowder_api_key,
                                   sync_clowder_metadata=FALSE) {
  printCurrentFunction(source_table)

  if(is.null(map_file)){
    # Switch case to load specific source document map files
    map_file = switch(source_table,
                      "source_caloehha" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                   "clowder_v3/cal_oehha_log_with_names_20221019.xlsx")),
                      "source_cosmos" = { readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                   "clowder_v3/cosmos_document_map_05162023.xlsx"),
                                                            guess_max=21474836) %>%
                          filter(!is.na(clowder_id))
                      },
                      "source_iris" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                               "clowder_v3/source_iris_2023_document_map_20231017.xlsx")),
                      # "source_pprtv_ornl" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                      #                                           "clowder_v3/pprtv_ornl_docment_map_08172022_mmille16.xlsx")),
                      "source_pprtv_ncea" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                     "clowder_v3/pprtv_ncea_document_map_01122023.xlsx")),
                      # "source_efsa2" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                      #                                    "clowder_v3/efsa_combined_new_matched_checked_ids_07142022_jwilli29.xlsx")),
                      "source_hawc_pfas_150" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                        "clowder_v3/hawc_pfas_150_document_map_20221123.xlsx")),
                      "source_hawc_pfas_430" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                        "clowder_v3/hawc_pfas_430_doc_map_20230120.xlsx")),
                      "source_pfas_150_sem_v2" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                          "clowder_v3/pfas_150_sem_document_map_10032022_mmille16.xlsx")),
                      "source_hpvis" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                "clowder_v3/source_hpvis_document_map_jwall01_20221129.xlsx")),
                      "source_oppt" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                               "clowder_v3/source_oppt_doc_map_20231017.xlsx")),
                      "source_efsa" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                               "clowder_v3/source_efsa_20231018.xlsx"), col_types = "text"),
                      "source_hawc" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                               "clowder_v3/hawc_original_matched_07072022_mmille16.xlsx")),
                      "source_pprtv_cphea" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                      "clowder_v3/pprtv_cphea_doc_map_lineage_jwall01.xlsx")),
                      "source_who_jecfa" = readr::read_csv(paste0(toxval.config()$datapath,
                                                                  "clowder_v3/source_who_jecfa_document_map_20230920.csv")),
                      "source_epa_ow_npdwr" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                       "clowder_v3/source_epa_ow_npdwr_document_map.xlsx")),
                      "source_epa_ow_nrwqc_hhc" = readr::read_csv(paste0(toxval.config()$datapath,
                                                                         "clowder_v3/source_epa_ow-nrwqc-hhc_document_map.csv"),
                                                                  col_types = readr::cols()),
                      "source_epa_ow_nrwqc_alc" = readr::read_csv(paste0(toxval.config()$datapath,
                                                                         "clowder_v3/source_epa_ow_nrwqc_alc_document_map_20231004.csv"),
                                                                  col_types = readr::cols()),
                      "source_epa_ow_opp_alb" = readr::read_csv(paste0(toxval.config()$datapath,
                                                                         "clowder_v3/source_epa_ow_opp_alb_document_map.csv"),
                                                                  col_types = readr::cols()),
                      "source_atsdr_mrls" = readr::read_csv(paste0(toxval.config()$datapath,
                                                                       "clowder_v3/source_astdr_mrls_2023_document_map_20231012.csv"),
                                                                col_types = readr::cols()),
                      "source_ntp_pfas" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                   "clowder_v3/source_ntp_pfas_doc_map_20231019.xlsx")),
                      ### Hard coded document maps
                      "source_alaska_dec" = data.frame(clowder_id = "610038e1e4b01a90a3f9ae63",
                                                       document_name = "53dec438dd4a7efab7ca19ffd32e9e45-Alaska Department of Environmental Conservation-2008-Clean-up L.pdf"),
                      # "source_atsdr" = data.frame(clowder_id="610036c7e4b01a90a3f9879c",
                      #                          document_name="a6a427952aa24d5d9e1d1a229109ba7e-Agency for Toxic Substances and Disease Registry-2020.pdf"),
                      # "source_atsdr_pfas" = data.frame(clowder_id = "6238e943e4b0b18cb57ced5a",
                      #                     document_name = "tp200-c2.pdf"),
                      "source_atsdr_pfas_2021" = data.frame(clowder_id = "6238b97ae4b0b18cb57ce4f6",
                                                            document_name = "tp200.pdf"),
                      "source_chiu" = data.frame(clowder_id = "61003953e4b01a90a3f9b6d1",
                                                 document_name = "08b44893c3ec2ed2c917bd2962aefca2-Chiu-2018-Beyond the.pdf"),
                      "source_dod_meg" = data.frame(clowder_id = "61003ab1e4b01a90a3f9ce11",
                                                    document_name = "3606a83ea4293730c355bceca0900d9c-Anonymous-2013-Technical .pdf"),
                      "source_doe_benchmarks" = data.frame(clowder_id = "651ef0b2e4b0f0a60ddcffee",
                                                           document_name = "doe_wildlife_benchmarks_1996_tm86r3.pdf"),
                      "source_envirotox" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                    "clowder_v3/source_envirotox_doc_map_20231010.xlsx")),
                      "source_epa_aegl" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                   "clowder_v3/source_epa_aegl_document_map.xlsx")),
                      "source_opp" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                              "clowder_v3/epa_opp_doc_lineage_mmille16.xlsx")),
                      "source_health_canada" = data.frame(clowder_id = "61003a57e4b01a90a3f9c305",
                                                          document_name = "60683b9de75ea6aced60e004a919370b-Health Canada-2010-Part II H.pdf"),
                      "source_niosh" = data.frame(clowder_id = "61fabd3de4b04a563fdc9b99",
                                                  document_name = "ToxValQA33091630_NIOSH_2020_ImmediatelyDangerous-(IDLH)Values.pdf"),
                      "source_ow_dwsha" = data.frame(clowder_id = "610036ede4b01a90a3f98ae0",
                                                     document_name = "b5ffe2b7e16578b78213213141cfc3ad-United States Environmental Protection Agency (USEPA)-2018-2018 Drink.pdf"),
                      "source_penn_dep" = data.frame(clowder_id = "61003849e4b01a90a3f9a409",
                                                     document_name = "3d84a7d7e81b35b4e5ac9b820558be27-Pennsylvania Department of Environmental Protection (DEP)-2012.pdf "),
                      "source_penn" = data.frame(clowder_id = "61003849e4b01a90a3f9a409",
                                                 document_name = "3d84a7d7e81b35b4e5ac9b820558be27-Pennsylvania Department of Environmental Protection (DEP)-2012.pdf "),
                      "source_usgs_hbsl" = data.frame(clowder_id = "61fabdc3e4b04a563fdc9c0b",
                                                      document_name = "ToxValQA29186291_USGS_2018_Health-BasedScreening-Water-QualityData.pdf"),
                      "source_who_ipcs" = data.frame(clowder_id = "64b6e0a5e4b08a6b5a3adf7a",
                                                     document_name = "who_ipcs_2019.pdf"),
                      "source_osha_air_limits" = data.frame(clowder_id = "61fabd47e4b04a563fdc9bb8",
                                                            document_name = "ToxValQA29180809_OSHA_TABLEZ-1LimitsforAirContaminants.pdf"),
                      "source_fda_cedi" = data.frame(clowder_id = "619d2972e4b0993a3937de4f",
                                                     document_name = "ToxValQA29176904_FDA_CumulativeEstimatedDailyIntake.pdf"),
                      # "source_wignall" = data.frame(clowder_id = "62b30a1ee4b07abf29f56811",
                      #                  document_name = "ToxValDBQA Wignall EHP 2014.pdf"),
                      "source_test" = data.frame(clowder_id = "6390c185e4b04f6bb149889a; 6390c185e4b04f6bb1498899",
                                                 document_name = "TEST data.xlsx; test_chemicals_invitrodb.csv"),
                      #"source_atsdr_mrls" = data.frame(clowder_id="649c5e23e4b00be57315594c",
                      #                                 document_name="ATSDR MRLs - April 2023 - H.pdf"),
                      # "source_atsdr_mrls_2022" = data.frame(clowder_id="63b58958e4b04f6bb1507bf2",
                      #                          document_name="ATSDR MRLs - August 2022 - H.pdf"),
                      "source_rsl" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                              "clowder_v3/source_rsl_doc_map_2022-11-01.xlsx")),
                      "source_hess" = {
                        paste0(toxval.config()$datapath,"clowder_v3/toxval_document_map_icf.xlsx") %>%
                          readxl::read_xlsx() %>%
                          fix.non_ascii.v2(.,"map.icf")
                      },
                      # No source match, return empty
                      data.frame()
    )

    # Sources with a single document in a combined map
    if(source_table %in% c("source_heast", "source_mass_mmcl",
                           "source_cal_dph", "source_copper", "source_dod_ered",
                           "source_doe_lanl_ecorisk", "source_doe_pac")){
      map_file = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                          "clowder_v3/source_single_doc_map.xlsx")) %>%
        dplyr::rename(src_tbl = source_table) %>%
        filter(src_tbl %in% source_table) %>%
        select(clowder_id, document_name)
    }

    # IUCLID sources in a combined map
    if(grepl("iuclid", source_table)){
      map_file = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                          "clowder_v3/source_echa_iuclid_doc_map.xlsx"))
    }
  }

  # Ensure clowder_id column is provided and/or standardized
  if(is.null(map_clowder_id_field) || is.na(map_clowder_id_field)){
    map_clowder_id_field <- "clowder_id"
    if(!"clowder_id" %in% names(map_file)){
      stop("Must provide a 'map_clowder_id_field' present in the provided 'map_file'...")
    }
  } else {
    # Rename custom ID field to common name
    map_file <- map_file %>%
      dplyr::rename(clowder_id = !!map_clowder_id_field)
  }

  # Filter documents to only new documents
  pushed_docs <- runQuery(paste0("SELECT clowder_id FROM documents where clowder_id in ('",
                                 paste0(unique(map_file$clowder_id), collapse="', '"),
                                 "')"),
                          db)

  mat <- map_file %>%
    dplyr::filter(!clowder_id %in% pushed_docs$clowder_id,
                  !is.na(clowder_id)) %>%
    dplyr::select(clowder_id) %>%
    # Only push unique Clowder IDs
    unique()

  if(nrow(mat)){
    message("...pushing ", nrow(mat), " new document entries...")
    # Push new document records to documents table
    runInsertTable(mat=mat,
                   table="documents",
                   db=db)
  } else {
    message("...no new document entries to push...moving on...")
  }

  if(sync_clowder_metadata){
    if(is.null(clowder_url) || is.null(clowder_api_key) ||
       is.na(clowder_url) || is.na(clowder_api_key)){
      cat("\nCannot sync clowder metadata without clowder_url and clowder_api_key...")
    } else {
      # Implement function with this subset of new documents
      doc_lineage_sync_clowder_metadata(source_table=source_table,
                                        db=db,
                                        clowder_url=clowder_url,
                                        clowder_api_key=clowder_api_key)
    }
  }

  # Pull source table document records
  pushed_docs <- runQuery(paste0("SELECT id as fk_doc_id, clowder_id FROM documents where clowder_id in ('",
                                 paste0(unique(map_file$clowder_id), collapse="', '"),
                                 "')"),
                          db)
  # Match records to documents ID table records
  map_file <- map_file %>%
    dplyr::left_join(pushed_docs,
                     by="clowder_id")

  if(!"parent_clowder_id" %in% names(map_file)){
    message("'parent_document_id' field not in map, cannot map document association lineage...skipping...")
  } else {
    # Push document-to-document associations
    mat <- map_file %>%
      # Select linkage cols
      dplyr::select(clowder_id, parent_clowder_id, parent_flag, fk_doc_id) %>%
      # Only those with a parent
      dplyr::filter(!is.na(parent_clowder_id)) %>%
      #Separate rows for the parent_clowder_id with multiple document linkages
      tidyr::separate_rows("parent_clowder_id","parent_flag",sep = ",") %>%
      # Split parent_clowder_id list (provided as semicolon separated list)
      tidyr::separate_rows(parent_clowder_id, parent_flag, sep="; ", convert=TRUE) %>%
      # Fix split columns extra whitespace
      dplyr::mutate(across(where(is.character), ~stringr::str_squish(.))) %>%
      # Filter out NA split parent fields
      dplyr::filter(!is.na(parent_clowder_id)) %>%
      # Map document ID values from database by Clowder ID
      dplyr::left_join(pushed_docs %>%
                         dplyr::rename(fk_parent_doc_id=fk_doc_id),
                       by=c("parent_clowder_id"="clowder_id")) %>%
      dplyr::select(-clowder_id, -parent_clowder_id) %>%
      # Only those that matched a document ID
      dplyr::filter(!is.na(fk_parent_doc_id))

    # Insert linkages (don't have to check pushed associations before due to
    # 'unique' database key on fk_doc_id and fk_parent_doc_id fields)
    runInsertTable(mat=mat,
                   table = "documents_lineage",
                   get.id = FALSE,
                   db=db)
  }
################################################################################
### PUll source table data
################################################################################
  res <- runQuery(paste0("SELECT * FROM ", source_table), db=db)

  # Check if source table data has been pushed (could be empty table)
  if(!nrow(res)){
    message("...No source table data pulled...returning...")
    return()
  }
  # Clear old mappings (if present in source table)
  res$clowder_id = NULL
  # Custom mapping source_hash to document ID

  # Map documents to source_hash records like normal
  if(nrow(map_file) == 1){
    # Set the easy mappings (only 1 document)
    res$clowder_id = map_file$clowder_id
    res$document_name = map_file$document_name
    res$fk_doc_id = map_file$fk_doc_id
    cat("clowder_id and document_name set for ",source_table,"\n")
  } else {
    res <- switch(source_table,

                  "source_pprtv_cphea" = {
                    res <- res %>%
                      dplyr::select(source_hash, source_version_date, name) %>%
                      dplyr::left_join(map_file %>%
                                         dplyr::select(fk_doc_id, clowder_id, Chemical),
                                       by=c("name"="Chemical"))
                    # Return res
                    res
                  },

                  "source_hess" = {
                    res <- res %>%
                      left_join(map_file %>%
                                  select(clowder_id, document_name, fk_doc_id),
                                by="document_name")
                    # Return res
                    res
                  },

                  "source_rsl" = {
                    res <- res %>%
                      left_join(map_file,
                                by=c("raw_input_file"="document_name"))
                    # Return res
                    res
                  },
                  # "source_iris_2022-10-21" = {
                  #   # cut the map down to just the webpage PDF documents, not screenshots or supplements
                  #   map_file <- map_file[which(map_file$parentPath == "IRIS"),]
                  #   # Match by chemical name first
                  #   res = res %>%
                  #     left_join(map_file %>%
                  #                 select(chemical_name, clowder_id, fk_doc_id),
                  #               by=c("name" = "chemical_name"))
                  #   # Filter to those without a match
                  #   res2 = res %>%
                  #     filter(is.na(clowder_id))
                  #   res = res %>%
                  #     filter(!is.na(clowder_id))
                  #   # Match by casrn
                  #   res2 = res2 %>%
                  #     select(-clowder_id, -fk_doc_id) %>%
                  #     left_join(map_file %>%
                  #                 select(casrn, clowder_id, fk_doc_id),
                  #               by="casrn")
                  #   # Recombine all matches
                  #   res = rbind(res, res2)
                  #   # Return res
                  #   res
                  # },
                  "source_iris" = {
                    # associates each origin document to specific record
                    origin_docs <- map_file %>%
                      dplyr::filter(is.na(parent_flag))
                    # Perform a left join on chemical names to match chemical names
                    res1 <- res %>%
                      dplyr::select(name, iris_chemical_id, source_hash, source_version_date) %>%
                      dplyr::left_join(origin_docs %>%
                                         dplyr::select(iris_chemical_id = chem_id, clowder_id, fk_doc_id),
                                       by = "iris_chemical_id")

                    # associates each record to the extraction document
                    extraction_docs <- map_file %>%
                      dplyr::filter(!is.na(parent_flag))

                    # Perform a left join on chemical names to match chemical names
                    res2 <- res %>%
                      dplyr::select(name, iris_chemical_id, source_hash, source_version_date) %>%
                      merge(extraction_docs %>%
                              dplyr::select(clowder_id, fk_doc_id))

                    # Combine the two associated data frames back into res
                    res <- rbind(res1, res2) %>%
                      dplyr::arrange(source_hash)

                    #Return the mapped res with document names and clowder ids
                    res
                  },
                  # "source_pprtv_ornl" = {
                  #   # Filter to the "_webpage_" PDF Clowder document
                  #   map_file = map_file[grepl("_webpage_", map_file$document_name) &
                  #                         grepl(".pdf", map_file$document_name), ]
                  #   for (i in 1:nrow(res)){
                  #     #Will perform matching based on casrn and chemical name fields
                  #     res_cas_num = res[i,'casrn']
                  #     res_chem_name = res[i,'name']
                  #     #Get rid of the leading zeros added by excel
                  #     res_cas_num <- sub("^0+","",res_cas_num)
                  #
                  #     #Match first based on exact chemical name (most consistently populated in key and res)
                  #     row = match(res_chem_name,map_file$chemical_name)
                  #     clowder_id = map_file[row,'clowder_id']
                  #     doc_name = map_file[row,'document_name']
                  #     #Some chemicals have additional abbreviations in the document map. Use grep to look for
                  #     #the chemical name from res is contained in the chemical name row (different than exact matching)
                  #     if (is.na(clowder_id)){
                  #       rows = grep(res_chem_name,map_file$chemical_name)
                  #       clowder_id = map_file$clowder_id[rows[1]]
                  #       document_name = map_file$document_name[rows[1]]
                  #     }
                  #     #Final match criteria is the casrn number. Res has all casrns but document map does not
                  #     #PPRTV ORNL source listed some casrn numbers as "various" instead of specific numbers
                  #     if(is.na(clowder_id)){
                  #       #If didn't match from chemical name, try to match by casrn
                  #       row = match(res_cas_num,map_file$casrn)
                  #       clowder_id = map_file[row,'clowder_id']
                  #       doc_name = map_file[row,'document_name']
                  #     }
                  #     #Populate clowder id and document name fields with matched info from key
                  #     res[i,'clowder_id'] = clowder_id
                  #     res[i,'document_name'] = doc_name
                  #   }
                  #   # Match for fk_doc_id field
                  #   res <- res %>%
                  #     dplyr::left_join(map_file %>%
                  #                        dplyr::select(fk_doc_id, clowder_id),
                  #                      by="clowder_id")
                  #   # Return res
                  #   res
                  # },

                  "source_pprtv_ncea" = {
                    res$document_name <- NULL
                    # Match by chemical name
                    res0 = res %>%
                      left_join(map_file %>%
                                  select(Chemical, clowder_id, fk_doc_id),
                                by=c("name" = "Chemical")) %>%
                      filter(!is.na(clowder_id))
                    # Filter to non-matches
                    res = res %>%
                      filter(!name %in% res0$name)
                    # Match by cas
                    res1 = res %>%
                      left_join(map_file %>%
                                  select(CASRN, clowder_id, fk_doc_id),
                                by= c("casrn"="CASRN")) %>%
                      filter(!is.na(clowder_id))
                    # Filter to non-matches
                    res = res %>%
                      filter(!casrn %in% res1$casrn) %>%
                      mutate(clowder_id = NA)

                    # Hardcode matching of "thiocyanate" to "thiocyanates"
                    res$clowder_id[which(res$name == "Thiocyanate")] <- "639a2fe6e4b04f6bb14a2734"

                    # Match for fk_doc_id field
                    res <- res %>%
                      dplyr::left_join(map_file %>%
                                         dplyr::select(fk_doc_id, clowder_id),
                                       by="clowder_id")

                    res = rbind(res, res0, res1)
                    # Return res
                    res
                  },

                  "source_caloehha" = {
                    # cut the map down to just the webpage PDF documents, no screenshots
                    map_file <- map_file %>%
                      filter(subDir1 == "pdf")
                    # clear old names
                    res$document_name = NULL
                    # Match by chemical name first
                    res = res %>%
                      left_join(map_file %>%
                                  select(Chemical, clowder_id),
                                by=c("name" = "Chemical"))
                    # Filter to those without a match
                    res2 = res %>%
                      filter(is.na(clowder_id))
                    res = res %>%
                      filter(!is.na(clowder_id))
                    # Match by casrn
                    res2 = res2 %>%
                      select(-clowder_id) %>%
                      left_join(map_file %>%
                                  select(casrn=CASRN, clowder_id),
                                by="casrn")
                    # Recombine all matches
                    res = rbind(res, res2)

                    # Match for fk_doc_id field
                    res <- res %>%
                      dplyr::left_join(map_file %>%
                                         dplyr::select(fk_doc_id, clowder_id),
                                       by="clowder_id")
                    # Return res
                    res
                  },

                  # "source_efsa2" = {
                  #   # Update map_file so it only contains mapped clowder_id values with long_refs
                  #   map_file = map_file %>%
                  #     select(clowder_id, long_ref, fk_doc_id) %>%
                  #     distinct() %>%
                  #     filter(!is.na(clowder_id))
                  #   # clear old names
                  #   res$document_name <- NULL
                  #
                  #   # match by longref
                  #   res <- res %>%
                  #     left_join(select(map_file, long_ref, clowder_id, fk_doc_id),
                  #               by = "long_ref") %>%
                  #     distinct()
                  #
                  #   # Return res
                  #   res
                  # },

                  "source_pfas_150_sem_v2" = {
                    # Update map_file so it only contains mapped clowder_id values with long_refs
                    map_file$clowder_id[map_file$clowder_id == "-"] <- NA
                    map_file = map_file %>%
                      select(clowder_id, fk_doc_id, hero_id = `HERO ID`) %>%
                      mutate(hero_id = as.character(hero_id)) %>%
                      distinct() %>%
                      filter(!is.na(clowder_id))
                    # clear old names
                    res$document_name <- NULL

                    # match by longref
                    res <- res %>%
                      left_join(map_file,
                                by = "hero_id") %>%
                      distinct()

                    # Return res
                    res
                  },

                  "source_hpvis" = {
                    res = res %>%
                      left_join(map_file %>%
                                  select(clowder_id, document_name, fk_doc_id),
                                by = c("raw_input_file"="document_name"))
                    # Return res
                    res
                  },

                  "source_oppt" = {
                    res$document_name = NULL
                    # Associates origin documents to records based on filename
                    origin_docs <- map_file %>%
                      dplyr::filter(is.na(parent_flag))

                    res1 <- res %>%
                      select(source_hash, source_version_date, srcf) %>%
                      left_join(origin_docs %>%
                                  select(clowder_id, filename, fk_doc_id),
                                by = c("srcf"="filename"))

                    # Associates extraction document to all records
                    extraction_docs <- map_file %>%
                      dplyr::filter(!is.na(parent_flag))

                    res2 <- res %>%
                      dplyr::select(source_hash, source_version_date) %>%
                      merge(extraction_docs %>%
                              dplyr::select(clowder_id, fk_doc_id, "srcf" = filename))

                    # Combines both associations back into one data frame
                    res <- rbind(res1, res2) %>%
                      dplyr::arrange(source_hash)

                    # Return res
                    res
                  },

                  "source_efsa" = {
                    res$document_name = NULL

                    res <- res %>%
                      dplyr::select(source_hash, title, source_version_date) %>%
                      left_join(map_file %>%
                                  #filter(!is.na(clowder_id)) %>%
                                  select(clowder_id, long_ref, fk_doc_id) %>%
                                  distinct(),
                                by = c("title" = "long_ref"))

                    # Match to extraction doc
                    tmp = res %>%
                      dplyr::select(source_hash, source_version_date) %>%
                      merge(map_file %>%
                              dplyr::filter(is.na(name)) %>%
                              dplyr::select(clowder_id, "title" = long_ref, fk_doc_id))

                    # Combine origin and extraction document associations
                    res = rbind(res, tmp)

                    # Return res
                    res
                  },

                  "source_hawc" = {
                    # Focus only on the study id and clowder id fields for matching
                    res <- res %>%
                      left_join(map_file %>%
                                  filter(!is.na(clowder_id)) %>%
                                  select(clowder_id, fk_doc_id, animal_group.experiment.study.id) %>%
                                  distinct(),
                                by=c("study_id" = "animal_group.experiment.study.id"))
                    # Return res
                    res
                  },

                  "source_cosmos" = {
                    res <- res %>%
                      dplyr::left_join(map_file %>%
                                         dplyr::select(-source, -new_hash),
                                       by = c("name", "casrn", "study_type", "species", "study_reference", "year")
                      ) %>%
                      dplyr::select(source_hash, fk_doc_id, clowder_id, source_version_date) %>%
                      dplyr::distinct()

                    res
                  },
                  "source_opp" = {
                    #Perform a left join on chemical names to match clowder ids and document names
                    res <- res %>%
                      left_join(map_file %>%
                                  dplyr::select(name = Chemical, clowder_id, filename, fk_doc_id),
                                by = "name")
                    #Return the mapped res with document names and clowder ids
                    res
                  },
                  "source_who_jecfa" = {
                    #Perform a left join on chemical names to match the chemical ids (the last part of the url)
                    res <- res %>%
                      left_join(map_file %>%
                                  dplyr::select(name = Chemical, clowder_id, filename, fk_doc_id),
                                by = "basename(URL)")
                    #Return the mapped res with document names and clowder ids
                    res
                  },
                  "source_epa_ow_npdwr" = {
                    # All source_hash records associated to all documents (1 extraction, 1 origin)
                    res <- res %>%
                      dplyr::select(source_hash, source_version_date) %>%
                      merge(map_file %>%
                              dplyr::select(clowder_id, fk_doc_id))
                    #Return the mapped res with document names and clowder ids
                    res
                  },
                  "source_epa_ow_nrwqc_hhc" = {
                    # Perform a left join on chemical names to match chemical names
                    # Match to origin docs (some do not have origin docs)
                    res <- res %>%
                      dplyr::select(source_hash, source_version_date, name) %>%
                      dplyr::left_join(map_file %>%
                                         dplyr::select(name, clowder_id, fk_doc_id),
                                       by = "name")

                    # Match to extraction doc
                    tmp = res %>%
                      dplyr::select(source_hash, source_version_date, name) %>%
                      merge(map_file %>%
                              dplyr::filter(is.na(name)) %>%
                              dplyr::select(clowder_id, fk_doc_id))

                    res = rbind(res, tmp)
                    #Return the mapped res with document names and clowder ids
                    res
                  },
                  "source_epa_ow_nrwqc_alc" = {
                    # associates each origin document to specific record
                    origin_docs <- map_file %>%
                      dplyr::filter(is.na(parent_flag))
                    # Perform a left join on chemical names to match chemical names
                    res1 <- res %>%
                      dplyr::select(name, source_hash, source_version_date) %>%
                      dplyr::left_join(origin_docs %>%
                                         dplyr::select(name = chemical, clowder_id, fk_doc_id),
                                       by = "name") %>%
                      dplyr::select(-name)
                    # associates each record to the extraction document
                    extraction_docs <- map_file %>%
                      dplyr::filter(!is.na(parent_flag))

                    # Perform a left join on chemical names to match chemical names
                    res2 <- res %>%
                      dplyr::select(source_hash, source_version_date) %>%
                      merge(extraction_docs %>%
                              dplyr::select(clowder_id, fk_doc_id))

                    # Combine the two associated dataframes back into res
                    res <- rbind(res1, res2) %>%
                      dplyr::arrange(source_hash)
                    #Return the mapped res with document names and clowder ids
                    res
                  },
                  "source_envirotox" = {
                    # All source_hash records associated to all documents (1 extraction, 1 origin)
                    res <- res %>%
                      dplyr::select(source_hash, source_version_date) %>%
                      merge(map_file %>%
                              dplyr::select(clowder_id, fk_doc_id))
                    #Return the mapped res with document names and clowder ids
                    res

                  },
                  "source_epa_ow_opp_alb" = {
                    # associates each origin document to specific record
                    origin_docs <- map_file %>%
                      dplyr::filter(is.na(parent_flag)) %>%
                      fix.non_ascii.v2(source=source_table)
                    # Perform a left join on chemical names to match chemical names
                    res1 <- res %>%
                      dplyr::select(name, source_hash, source_version_date) %>%
                      dplyr::left_join(origin_docs %>%
                                         dplyr::select(name = chemical, clowder_id, fk_doc_id),
                                       by = "name") %>%
                      dplyr::select(-name)
                    # associates each record to the extraction document
                    extraction_docs <- map_file %>%
                      dplyr::filter(!is.na(parent_flag))

                    # Perform a left join on chemical names to match chemical names
                    res2 <- res %>%
                      dplyr::select(source_hash, source_version_date) %>%
                      merge(extraction_docs %>%
                              dplyr::select(clowder_id, fk_doc_id))

                    # Combine the two associated dataframes back into res
                    res <- rbind(res1, res2) %>%
                      dplyr::arrange(source_hash)
                    #Return the mapped res with document names and clowder ids
                    res
                  },
                  "source_atsdr_mrls" = {
                    # associates each origin document to specific record
                    origin_docs <- map_file %>%
                      dplyr::filter(is.na(parent_flag))
                    # Chemical names in res and origin docs were in two different cases, changing
                    # the case so that we can merge on chemical name
                    origin_docs$`Chemical Name` <- toupper(origin_docs$`Chemical Name`)
                    # One record in res wasn't fully upper case, so had to make it upper
                    res$name <- toupper(res$name)

                    # Separate chemical name lists
                    origin_docs = origin_docs %>%
                      tidyr::separate_rows(`Chemical Name`, sep=", ") %>%
                      tidyr::separate_rows(`Chemical Name`, sep=" & ") %>%
                      tidyr::separate_rows(`Chemical Name`, sep=" AND ")

                    # Separate groups into individual chemical names
                    # origin_docs <- origin_docs %>%
                    #   mutate('Chemical Name'=strsplit(origin_docs$`Chemical Name`, ", ")) %>%
                    #   unnest(cols = c('Chemical Name'))
                    # origin_docs <- origin_docs %>%
                    #   mutate('Chemical Name'=strsplit(origin_docs$`Chemical Name`, " & ")) %>%
                    #   unnest(cols = c('Chemical Name'))
                    # origin_docs <- origin_docs %>%
                    #   mutate('Chemical Name'=strsplit(origin_docs$`Chemical Name`, " AND ")) %>%
                    #   unnest(cols = c('Chemical Name'))

                    # Perform a left join on chemical names to match chemical names
                    res1 <- res %>%
                      dplyr::select(name, source_hash, source_version_date) %>%
                      dplyr::left_join(origin_docs %>%
                                         dplyr::select(name = "Chemical Name", clowder_id, fk_doc_id),
                                       by = "name")

                    # Hard code matches with grep for chemical name
                    unmatched_names = c("BARIUM", "2-BUTOXYETHANOL", "CHROMIUM", "CYANIDE",
                                        "DDD", "DDT", "DDE", "DICHLOROBENZENE", "1,2-DICHLOROETHENE",
                                        "DICHLOROPROPENE", "DINITROTOLUENE", "FLUORIDE",
                                        "HEXACHLOROCYCLOHEXANE", "PBDES", "CHLOROPHENOL", "TIN",
                                        "URANIUM", "HYDRAZINE", "CHLORODIBENZOFURAN", "PHOSPHATE",
                                        "XYLENES", "PHOSPHORUS", "IONIZING RADIATION", "MANGANESE",
                                        "METHYLENEDIPHENYL DIISOCYANATE")

                    # Find matches for those missing matches with grep name to chemical name
                    for(u_name in unmatched_names){
                      origin_replace = unique(origin_docs$clowder_id[grep(paste0("^", u_name), origin_docs$`Chemical Name`)])
                      # if(length(origin_replace) > 1) stop("origin_replace too long")
                      # if(length(origin_replace) == 0) stop("No replacement")
                      res1$clowder_id[grep(u_name, res1$name)] = origin_replace
                      res1$fk_doc_id[grep(u_name, res1$name)] = unique(origin_docs$fk_doc_id[origin_docs$clowder_id %in% origin_replace])
                    }

                    # associates each record to the extraction document
                    extraction_docs <- map_file %>%
                      dplyr::filter(!is.na(parent_flag))

                    # Perform a left join on chemical names to match chemical names
                    res2 <- res %>%
                      dplyr::select(source_hash, source_version_date) %>%
                      merge(extraction_docs %>%
                              dplyr::select(clowder_id, fk_doc_id, name = "Chemical Name"))

                    # Combine the two associated dataframes back into res
                    res <- rbind(res1, res2) %>%
                      dplyr::arrange(source_hash)

                    #Return the mapped res with document names and clowder ids
                    res
                  },
                  "source_epa_aegl" = {
                    res <- res %>%
                      dplyr::select(name, casrn, source_hash, source_version_date) %>%
                      dplyr::left_join(map_file %>%
                                  #filter(!is.na(clowder_id)) %>%
                                    dplyr::select("casrn" = casn, clowder_id, fk_doc_id) %>%
                                    dplyr::distinct(),
                                by = "casrn")

                    # Match to extraction doc
                    tmp = res %>%
                      dplyr::select(name, casrn, source_hash, source_version_date) %>%
                      merge(map_file %>%
                              dplyr::filter(is.na(name)) %>%
                              dplyr::select(clowder_id, fk_doc_id))

                    # Combine origin and extraction document associations
                    res = rbind(res, tmp)

                    # Return res
                    res
                  },
                  "source_ntp_pfas" = {
                    # Associate records based off of ntp_study_identifier
                    res = res %>%
                      dplyr::select(ntp_study_identifier, source_hash, source_version_date) %>%
                      dplyr::left_join(map_file %>%
                                         # Create map_file column with NTP Study Identifier from file name
                                         # Based on what's in the NTP PFAS source data
                                         dplyr::mutate(ntp_study_identifier = stringr::str_extract(subDir3,
                                                                                                   paste0(unique(res$ntp_study_identifier),
                                                                                                          collapse="|"))) %>%
                                         dplyr::select(fk_doc_id, clowder_id, ntp_study_identifier),
                                       by = "ntp_study_identifier") %>%
                      dplyr::select(-ntp_study_identifier)

                    # Return res
                    res
                  },
                  # Default case, return without mapping
                  res
    )

    # Handle HAWC PFAS 150/430
    if(source_table %in% c("source_hawc_pfas_150","source_hawc_pfas_430")) {
      map = map_file %>%
        select(-fk_doc_id)
      title2 = res$title
      title2 = gsub("Registration dossier: |RRegistration dossier: ","", title2)
      title2 = gsub('\\.$','',title2)
      res$title2 = title2
      res$title2 = tolower(str_trim(title2))
      res$title3 = gsub("\\s*\\([^\\)]+\\)", "", res$title2)
      for(i in 1:nrow(map)) {
        cid = map[i,"clowder_id"]

        docname = map[i,"document_name"]
        title = map[i,"study_name"]
        title2 = str_trim(tolower(title)) %>%
          gsub('\\.$','',.)
        # Remove trailing . information
        title2b = sub('\\..*', '', title2)
        # Remove parenthetical information and trailing . information
        title3 = gsub("\\s*\\([^\\)]+\\)", "", title2) %>%
          gsub('\\.$','',.)
        # https://stackoverflow.com/questions/64656479/identical-strings-from-different-data-files-wont-match-in-r
        title4 = iconv(title2, from = 'UTF-8', to = 'ASCII//TRANSLIT')

        # Attempt various matching schemes
        # Direct title match
        if(any(is.element(res$title,title))){
          res[is.element(res$title,title),"clowder_id"] = cid
          res[is.element(res$title,title),"document_name"] = docname
        } else if(any(is.element(res$title2,title2))){
          res[is.element(res$title2,title2),"clowder_id"] = cid
          res[is.element(res$title2,title2),"document_name"] = docname
          # Removal of CASRN or other parenthetic information
        } else if(any(is.element(res$title3,title2))){
          res[is.element(res$title3,title2),"clowder_id"] = cid
          res[is.element(res$title3,title2),"document_name"] = docname
          # Title match after removing trailing "."
        } else if(any(is.element(res$title2,title2b))){
          res[is.element(res$title2,title2b),"clowder_id"] = cid
          res[is.element(res$title2,title2b),"document_name"] = docname
          # Case of incompatible encoding (UTF-8 vs. unknown)
        } else if(any(is.element(res$title2,title4))){
          res[is.element(res$title2,title4),"clowder_id"] = cid
          res[is.element(res$title2,title4),"document_name"] = docname
        }
      }
      # Hard coding a match due to HAWC extraction typo for a title (002 insted of 007)
      if(source_table == "source_hawc_pfas_150"){
        res$clowder_id[res$title == "Registration dossier: Trifluoroacetic acid (CASRN: 76-05-1): Developmental toxicity/teratogenicity: 002 Supporting | Experimental result"] = "637e5e2be4b04f6bb1425fc0"
        res$clowder_id[res$title == "Registration dossier: Trifluoroacetic acid (CASRN: 76-05-1): Developmental toxicity/teratogenicity: 007 Supporting | Experimental result"] = "637e5e2be4b04f6bb1425fc0"
      }

      # Remove title2 and 3
      res$title2 = NULL
      res$title3 = NULL
      # Match for fk_doc_id field
      res <- res %>%
        dplyr::left_join(map_file %>%
                           dplyr::select(fk_doc_id, clowder_id),
                         by="clowder_id")
    }

    # Handle IUCLID case
    if(grepl("iuclid", source_table)){
      res <- res %>%
        dplyr::left_join(map_file %>%
                           dplyr::select(fk_doc_id, clowder_id, source_table),
                         by = c("source"="source_table"))
    }
  }

  # Check for missing fk_doc_id matches
  # res %>% filter(is.na(fk_doc_id)) %>% View()

  #Checking and outing cat statement
  res2 <- res %>%
    dplyr::filter(is.na(clowder_id))
  records_missing <- length(unique(res2$source_hash))
  total_records <- length(unique(res$source_hash))
  cat("Mapped records: ", total_records-records_missing, "| Missing: ", records_missing, "(", round(records_missing/total_records*100, 3),"%)\n")

  # Filter documents_records to only new documents_records
  pushed_doc_records <- runQuery(paste0("SELECT source_hash, fk_doc_id FROM documents_records where source_hash in ('",
                                        paste0(unique(res$source_hash), collapse="', '"),
                                        "')"),
                                 db) %>%
    tidyr::unite(col="pushed_docs", source_hash, fk_doc_id)

  mat <- res %>%
    tidyr::unite(col="pushed_docs", source_hash, fk_doc_id, remove=FALSE) %>%
    dplyr::filter(!pushed_docs %in% pushed_doc_records$pushed_docs,
                  !is.na(fk_doc_id)) %>%
    dplyr::select(source_hash, source_version_date, fk_doc_id)

  if(nrow(mat)){
    message("...pushing ", nrow(mat), " new documents_records entries...")
    # Push new document records to documents table
    runInsertTable(mat=mat %>%
                     dplyr::mutate(source_table = source_table),
                   table="documents_records",
                   db=db)
  } else {
    message("...no new documents_records entries to push...moving on...")
  }

  # Update document_type
  set_clowder_doc_type(source_table=source_table,
                       clowder_url=clowder_url,
                       clowder_api_key=clowder_api_key,
                       source.db=db,
                       ds_id = "5e31dc1e99323f93a9f5cec0",
                       clowder_id_list=res %>%
                         dplyr::select(clowder_id) %>%
                         dplyr::distinct())
}
