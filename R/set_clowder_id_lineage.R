#--------------------------------------------------------------------------------------
#' @description Create document records and associations in toxval_source based on input source table and document map
#' @param source_table The source table name (e.g. source_test)
#' @param map_file A dataframe of Clowder document mapping info. If NULL, will try to load a hardcoded map for the source
#' @param map_clowder_id_field Column name for the Clowder ID field of the map
#' @param clowder_url URL to Clowder
#' @param clowder_api_key API key to access Clowder resources
#' @param sync_clowder_metadata Boolean whether to sync Clowder metadata for new document records. Default is False.
#' @param source.db The source database name
#' @param toxval.db The database version to use
#' @return Returns an updated map with newly associated toxval_source table ID values
#' @title set_clowder_id_lineage
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
#' @importFrom readr read_csv cols
#' @importFrom tidyselect where
#' @importFrom stringr str_squish str_extract str_trim
#--------------------------------------------------------------------------------------
set_clowder_id_lineage <- function(source_table,
                                   map_clowder_id_field,
                                   map_file,
                                   clowder_url,
                                   clowder_api_key,
                                   sync_clowder_metadata=FALSE,
                                   source.db,
                                   toxval.db) {
  printCurrentFunction(source_table)

  if(is.null(map_file)){
    # Switch case to load specific source document map files
    map_file = switch(source_table,
                      "source_caloehha" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                   "clowder_v3/source_caloehha_document_map_20240528.xlsx")),
                      "source_cosmos" = { readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                   "clowder_v3/source_cosmos_document_map_20240227.xlsx"),
                                                            guess_max=21474836) %>%
                          dplyr::filter(!is.na(clowder_id))
                      },
                      "source_iris" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                               "clowder_v3/source_iris_2023_document_map_20240521.xlsx"), col_type = "text"),
                      # "source_pprtv_ornl" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                      #                                           "clowder_v3/pprtv_ornl_docment_map_08172022_mmille16.xlsx")),
                      "source_pprtv_ncea" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                     "clowder_v3/source_pprtv_ncea_document_map_20240522.xlsx")),
                      "source_hawc_pfas_150" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                        "clowder_v3/source_hawc_pfas_150_document_map_20231114.xlsx")),
                      "source_hawc_pfas_430" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                        "clowder_v3/source_hawc_pfas_430_document_map_20231114.xlsx")),
                      "source_pfas_150_sem_v2" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                          "clowder_v3/source_pfas_150_sem_v2_document_map_20240717.xlsx")),
                      "source_hpvis" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                "clowder_v3/source_hpvis_document_map_jwall01_20221129.xlsx")),
                      "source_oppt" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                               "clowder_v3/source_epa_oppt_document_map_20240227.xlsx")),
                      "source_efsa" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                               "clowder_v3/source_efsa_document_map_20240604.xlsx"), col_types = "text"),
                      "source_hawc" = {
                        file_name = paste0(toxval.config()$datapath,
                                           "clowder_v3/hawc_original_matched_07072022_mmille16.xlsx")
                        sheet_list = readxl::excel_sheets(file_name)
                        # Pull all docu map sheets
                        docs = lapply(sheet_list, function(s_list){
                          readxl::read_xlsx(file_name, s_list) %>%
                            dplyr::select(clowder_id, animal_group.experiment.study.id) %>%
                            dplyr::mutate(parent_flag = NA,
                                          parent_clowder_id = NA)
                        }) %>%
                          dplyr::bind_rows() %>%
                          rbind(data.frame(clowder_id = "652d4faae4b045b9ff7a30d3",
                                           animal_group.experiment.study.id = NA,
                                           parent_flag = "primary_source",
                                           parent_clowder_id = paste0(unique(.$clowder_id[!is.na(.$clowder_id)]),
                                                                      collapse = "; ")),
                                .)
                        # Return prepped map
                        docs
                        # # Hard code extraction document and doc lineage
                        # tmp = data.frame(clowder_id = "652d4faae4b045b9ff7a30d3",
                        #                  animal_group.experiment.study.id = NA,
                        #                  parent_flag = "primary_source",
                        #                  parent_clowder_id = paste0(unique(docs$clowder_id[!is.na(docs$clowder_id)]),
                        #                                             collapse = "; "))
                      },
                      "source_pprtv_cphea" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                      "clowder_v3/source_pprtv_cphea_doument_map_20240521_jnhope.xlsx")),
                      "source_who_jecfa_adi" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                        "clowder_v3/source_who_jecfa_adi_document_map_20240227.xlsx")),
                      "source_who_jecfa_tox_studies" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                                "clowder_v3/source_who_jecfa_tox_studies_document_map_20240227.xlsx")),
                      "source_epa_ow_npdwr" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                       "clowder_v3/source_epa_ow_npdwr_document_map.xlsx")),
                      "source_epa_ow_nrwqc_hhc" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                           "clowder_v3/source_epa_ow-nrwqc-hhc_document_map_20231108.xlsx")),
                      "source_epa_ow_nrwqc_alc" = readr::read_csv(paste0(toxval.config()$datapath,
                                                                         "clowder_v3/source_epa_ow_nrwqc_alc_document_map_20231004.csv"),
                                                                  col_types = readr::cols()),
                      "source_epa_ow_opp_alb" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                         "clowder_v3/source_epa_ow_opp_alb_document_map_20240529.xlsx")),
                      "source_atsdr_mrls" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                     "clowder_v3/source_atsdr_mrls_doc_map_20240521.xlsx")),
                      "source_ntp_pfas" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                   "clowder_v3/source_ntp_pfas_doc_map_20240221_jnhope.xlsx")),
                      "source_health_canada" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                        "clowder_v3/source_health_canada_document_map_20240604.xlsx")),
                      ### Hard coded document maps
                      "source_alaska_dec" = data.frame(clowder_id = "610038e1e4b01a90a3f9ae63",
                                                       document_name = "53dec438dd4a7efab7ca19ffd32e9e45-Alaska Department of Environmental Conservation-2008-Clean-up L.pdf"),
                      # "source_atsdr" = data.frame(clowder_id="610036c7e4b01a90a3f9879c",
                      #                          document_name="a6a427952aa24d5d9e1d1a229109ba7e-Agency for Toxic Substances and Disease Registry-2020.pdf"),
                      # "source_atsdr_pfas" = data.frame(clowder_id = "6238e943e4b0b18cb57ced5a",
                      #                     document_name = "tp200-c2.pdf"),
                      "source_atsdr_pfas_2021" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                          "clowder_v3/source_atsdr_pfas_2021_document_map_20240529.xlsx")),
                      "source_dod_meg" = data.frame(clowder_id = "651c7a8fe4b0d99f5a8c9983",
                                                    document_name = "TG230MilitaryExposureGuidelines.xls"),
                      "source_doe_benchmarks" = data.frame(clowder_id = "65de658de4b063812d6afc53",
                                                           document_name = "DOE Benchmarks_1996-06-01.zip"),
                      "source_envirotox" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                    "clowder_v3/source_envirotox_doc_map_20231010.xlsx")),
                      "source_epa_aegl" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                   "clowder_v3/source_epa_aegl_document_map_20240529.xlsx")),
                      "source_opp" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                              "clowder_v3/source_epa_opp_document_map_20240528.xlsx")),
                      "source_niosh" = data.frame(clowder_id = "61fabd3de4b04a563fdc9b99",
                                                  document_name = "ToxValQA33091630_NIOSH_2020_ImmediatelyDangerous-(IDLH)Values.pdf"),
                      "source_ow_dwsha" = data.frame(clowder_id = "610036ede4b01a90a3f98ae0",
                                                     document_name = "b5ffe2b7e16578b78213213141cfc3ad-United States Environmental Protection Agency (USEPA)-2018-2018 Drink.pdf"),
                      "source_penn_dep_mcls" = data.frame(clowder_id = "65de5f44e4b063812d6afbb2",
                                                          document_name = "Pen DEP MCLs_2021-11-20_extraction.zip"),
                      "source_penn_dep_toxvalues" = data.frame(clowder_id = "65de5e8ae4b063812d6afb91",
                                                               document_name = "PEN DEP ToxValues20211120.zip"),
                      "source_usgs_hbsl" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                    "clowder_v3/source_usgs_hbsl_document_map_20240221_jnhope.xlsx")),
                      "source_who_ipcs" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                   "clowder_v3/source_who_ipcs_document_map.xlsx")),
                      "source_osha_air_limits" = data.frame(clowder_id = "65de60e8e4b063812d6afbd7",
                                                            document_name = "OSHA Air_2017-03-21_extraction_doc.zip"),
                      # "source_wignall" = data.frame(clowder_id = "62b30a1ee4b07abf29f56811",
                      #                  document_name = "ToxValDBQA Wignall EHP 2014.pdf"),
                      "source_test" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                               "clowder_v3/source_test_document_map_20240109.xlsx")),
                      #"source_atsdr_mrls" = data.frame(clowder_id="649c5e23e4b00be57315594c",
                      #                                 document_name="ATSDR MRLs - April 2023 - H.pdf"),
                      # "source_atsdr_mrls_2022" = data.frame(clowder_id="63b58958e4b04f6bb1507bf2",
                      #                          document_name="ATSDR MRLs - August 2022 - H.pdf"),
                      "source_rsl" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                              "clowder_v3/source_rsl_document_map_20240227.xlsx")),
                      "source_hess" =  readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                "clowder_v3/source_hess_2021_doc_map_20240429.xlsx")),
                      "source_copper" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                 "clowder_v3/source_copper_document_map.xlsx")),

                      "source_gestis_dnel" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                      "clowder_v3/source_gestis_dnel_document_map_20240529.xlsx"),
                                                               col_types = "text", guess_max=21474836),

                      "source_heast" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                "clowder_v3/source_heast_document_map_20240604.xlsx"), col_types = "text"),

                      "source_doe_pac" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                  "clowder_v3/source_doe_pac_document_map_20240529.xlsx"), col_types = "text") %>%
                        tidyr::separate_rows(clowder_id, filename, sep="; ") %>%
                        dplyr::select(-contentType) %>%
                        dplyr::distinct(),
                      "source_epa_hhtv" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                "clowder_v3/source_epa_hhtv_document_map.xlsx"), col_types = "text"),

                      "ChemIDPlus" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                              "clowder_v3/source_chemidplus_document_map.xlsx")),

                      "Uterotrophic Hershberger DB" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                                               "clowder_v3/source_uterotrophic_hershberger_db_document_map.xlsx")),

                      "ToxRefDB" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                             "clowder_v3/source_toxrefdb_document_map_20240716.xlsx")),

                      "ECOTOX" = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                                          "clowder_v3/source_ecotox_document_map_20240716.xlsx")),

                      # No source match, return empty
                      data.frame()
    )

    # Sources with a single document in a combined map
    if(source_table %in% c("source_mass_mmcl", "source_dod_ered", "source_doe_lanl_ecorisk")){
      map_file = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                          "clowder_v3/source_single_doc_map.xlsx")) %>%
        dplyr::rename(src_tbl = source_table) %>%
        dplyr::filter(src_tbl %in% source_table) %>%
        dplyr::select(clowder_id, document_name)
    }

    # IUCLID sources in a combined map
    if(grepl("iuclid", source_table)){
      map_file = readxl::read_xlsx(paste0(toxval.config()$datapath,
                                          "clowder_v3/source_iuclid_doc_map_20240424.xlsx"))
    }
  }

  # Ensure clowder_id column is provided and/or standardized
  if(is.null(map_clowder_id_field) || is.na(map_clowder_id_field)){
    map_clowder_id_field <- "clowder_id"
    if(!"clowder_id" %in% names(map_file)){
      cat(names(map_file), "\n")
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
                          source.db)

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
                   db=source.db)
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
                                        db=source.db,
                                        clowder_url=clowder_url,
                                        clowder_api_key=clowder_api_key)
    }
  }

  # Pull source table document records
  pushed_docs <- runQuery(paste0("SELECT id as fk_doc_id, clowder_id FROM documents where clowder_id in ('",
                                 paste0(unique(map_file$clowder_id), collapse="', '"),
                                 "')"),
                          source.db)
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
      dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~stringr::str_squish(.))) %>%
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
                   db=source.db)
  }
  ################################################################################
  ### PUll source table data
  ################################################################################
  if(source_table %in% c("ChemIDPlus", "Uterotrophic Hershberger DB", "ToxRefDB", "ECOTOX")) {
    res <- runQuery(paste0("SELECT * FROM toxval WHERE source='", source_table, "'"), db=toxval.db)
  } else {
    res <- runQuery(paste0("SELECT * FROM ", source_table), db=source.db)
  }

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
    if("document_name" %in% names(map_file)) {
      res$document_name = map_file$document_name
    } else {
      res$document_name = map_file$filename
    }
    res$fk_doc_id = map_file$fk_doc_id
    cat("clowder_id and document_name set for ",source_table,"\n")
  } else {
    res <- switch(source_table,

                  "source_pprtv_cphea" = {
                    # Match to origin docs
                    origin_docs <- map_file %>%
                      dplyr::filter(is.na(parent_flag))
                    res1 <- res %>%
                      dplyr::select(source_hash, source_version_date, name) %>%
                      dplyr::left_join(origin_docs %>%
                                         dplyr::select(fk_doc_id, clowder_id, Chemical),
                                       by=c("name"="Chemical"))

                    # Match to extraction doc
                    extraction_doc <- map_file %>%
                      dplyr::filter(!is.na(parent_flag))
                    tmp = res %>%
                      dplyr::select(source_hash, source_version_date, name) %>%
                      merge(extraction_doc %>%
                              dplyr::select(fk_doc_id, clowder_id))

                    # Combine origin and extraction associations
                    res = rbind(res1, tmp)

                    # Return res
                    res
                  },

                  "source_hess" = {
                    # Match to origin docs based on document names
                    origin_docs <- map_file %>%
                      dplyr::filter(is.na(parent_flag))
                    res1 <- res %>%
                      dplyr::select(source_hash, src_document_name, source_version_date) %>%
                      left_join(origin_docs %>%
                                  select(clowder_id, filename, fk_doc_id) %>%
                                  distinct(),
                                by = c("src_document_name" = "filename"))

                    # Match to extraction doc
                    extraction_doc <- map_file %>%
                      dplyr::filter(!is.na(parent_flag))
                    tmp = res %>%
                      dplyr::select(source_hash, src_document_name, source_version_date) %>%
                      merge(extraction_doc %>%
                              dplyr::select(clowder_id, fk_doc_id))

                    # Combine origin and extraction document associations
                    res = rbind(res1, tmp)
                    # Return res
                    res
                  },

                  "source_rsl" = {
                    res <- res %>%
                      dplyr::left_join(map_file,
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
                      dplyr::filter(document_type == "origin")

                    # Perform a left join on chemical id to match chemical ids
                    res1 <- res %>%
                      dplyr::select(name, iris_chemical_id, source_hash, source_version_date) %>%
                      tidyr::separate_rows(iris_chemical_id, sep="\\|::\\|") %>%
                      dplyr::mutate(iris_chemical_id = iris_chemical_id %>%
                                      stringr::str_squish()) %>%
                      dplyr::left_join(origin_docs %>%
                                         dplyr::select(iris_chemical_id = chem_id, clowder_id, fk_doc_id),
                                       by = "iris_chemical_id")

                    # Perform a left join on chemicals names for those that weren't matched on chemical id
                    res2 <- res1 %>%
                      filter(is.na(clowder_id)) %>%
                      dplyr::select(name, iris_chemical_id, source_hash, source_version_date) %>%
                      dplyr::left_join(origin_docs %>%
                                         dplyr::select(name, clowder_id, fk_doc_id),
                                       by = "name")

                    # Remove the records with missing clowder ids from res1 since they are associated with res2
                    res1 <- res1 %>%
                      filter(!is.na(clowder_id))

                    # associates each record to the extraction document
                    extraction_docs <- map_file %>%
                      dplyr::filter(!(document_type == "origin"))

                    res3 <- res %>%
                      dplyr::select(name, iris_chemical_id, source_hash, source_version_date) %>%
                      merge(extraction_docs %>%
                              dplyr::select(clowder_id, fk_doc_id))
                      #dplyr::rowwise() %>%
                      # Handle case of collapsed document_type
                      #dplyr::mutate(document_type = document_type %>%
                      #                strsplit("|::|", fixed=TRUE) %>%
                      #                unlist() %>%
                      #               stringr::str_squish() %>%
                      #                unique()) %>%
                      #dplyr::ungroup() %>%
                      #dplyr::left_join(extraction_docs %>%
                      #                   dplyr::select(clowder_id, fk_doc_id, iris_document_type),
                      #                 by = c("document_type" = "iris_document_type"))


                    # Combine the two associated data frames back into res
                    res <- rbind(res1, res2, res3) %>%
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

                    # Associate origin docs
                    origin_docs <- map_file %>%
                      dplyr::filter(parent_flag == "primary_source")
                    # Perform a left join to association origin docs based on casn
                    res1 <- res %>%
                      dplyr::select(casrn, name, source_hash, source_version_date) %>%
                      dplyr::left_join(origin_docs %>%
                                         dplyr::select(CASRN, clowder_id, fk_doc_id),
                                       by = c("casrn"="CASRN"))


                    # Associate all records to extraction doc
                    extraction_docs <- map_file %>%
                      dplyr::filter(parent_flag == "has_parent")
                    res2 <- res %>%
                      dplyr::select(casrn, name, source_hash, source_version_date) %>%
                      merge(extraction_docs %>%
                              dplyr::select(clowder_id, fk_doc_id))

                    # Combine the two associated dataframes back into res
                    res <- rbind(res1, res2) %>%
                      dplyr::arrange(source_hash)

                    # Return res
                    res
                  },

                  "source_caloehha" = {
                    origin_docs <- map_file %>%
                      dplyr::filter(is.na(parent_flag))

                    # Separate chemical name lists
                    origin_docs = origin_docs %>%
                      tidyr::separate_rows(`name`, sep=";  ")

                    # Perform a left join on chemical names to match chemical names
                    res1 <- res %>%
                      dplyr::select(name, source_hash, source_version_date) %>%
                      dplyr::left_join(origin_docs %>%
                                         dplyr::select(name, clowder_id, fk_doc_id),
                                       by = "name")

                    # associates each record to the extraction document
                    extraction_docs <- map_file %>%
                      dplyr::filter(!is.na(parent_flag))

                    # Perform a left join on chemical names to match chemical names
                    res2 <- res %>%
                      dplyr::select(source_hash, source_version_date) %>%
                      merge(extraction_docs %>%
                              dplyr::select(clowder_id, fk_doc_id, name))

                    # Combine the two associated dataframes back into res
                    res <- rbind(res1, res2) %>%
                      dplyr::arrange(source_hash)

                    #Return the mapped res with document names and clowder ids
                    res
                  },

                  "source_pfas_150_sem_v2" = {
                    # Match origin docs
                    res <- res %>%
                      dplyr::left_join(map_file %>%
                                         dplyr::filter(document_type == "origin", !is.na(clowder_id)) %>%
                                         dplyr::select(clowder_id, fk_doc_id, hero_id = `HERO ID`) %>%
                                         dplyr::mutate(hero_id = as.character(hero_id)) %>%
                                         dplyr::distinct(),
                                       by="hero_id") %>%
                      dplyr::select(source_hash, source_version_date, clowder_id, fk_doc_id, name, hero_id)

                    # Match to extraction doc
                    tmp = res %>%
                      dplyr::select(source_hash, source_version_date) %>%
                      merge(map_file %>%
                              dplyr::filter(document_type == "extraction") %>%
                              dplyr::select(clowder_id, fk_doc_id))

                    # Combine origin and extraction document associations
                    res = dplyr::bind_rows(res, tmp)

                    # Return res
                    res
                  },

                  "source_hpvis" = {
                    res = res %>%
                      dplyr::left_join(map_file %>%
                                         dplyr::select(clowder_id, document_name, fk_doc_id),
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
                      dplyr::select(source_hash, source_version_date, srcf) %>%
                      dplyr::left_join(origin_docs %>%
                                         dplyr::select(clowder_id, filename, fk_doc_id),
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

                    # Match to origin docs
                    origin_docs <- map_file %>%
                      dplyr::filter(is.na(parent_flag))
                    res1 <- res %>%
                      tidyr::separate_rows(doi, sep = "\\|::\\|") %>%
                      dplyr::mutate(doi = stringr::str_squish(doi)) %>%
                      dplyr::select(source_hash, doi, source_version_date) %>%
                      dplyr::left_join(origin_docs %>%
                                         dplyr::select(clowder_id, doi, fk_doc_id) %>%
                                         dplyr::distinct(),
                                       by = "doi")

                    # Match to extraction doc
                    extraction_doc <- map_file %>%
                      dplyr::filter(!is.na(parent_flag))
                    tmp = res %>%
                      dplyr::select(source_hash, source_version_date) %>%
                      merge(extraction_doc %>%
                              dplyr::select(clowder_id, doi, fk_doc_id))

                    # Combine origin and extraction document associations
                    res = rbind(res1, tmp)

                    # Return res
                    res
                  },

                  "source_hawc" = {
                    # Match origin docs
                    # Focus only on the study id and clowder id fields for matching
                    res <- res %>%
                      dplyr::mutate(study_id = as.numeric(study_id)) %>%
                      dplyr::left_join(map_file %>%
                                         dplyr::filter(!is.na(clowder_id)) %>%
                                         dplyr::select(clowder_id, fk_doc_id, animal_group.experiment.study.id) %>%
                                         dplyr::distinct(),
                                       by=c("study_id" = "animal_group.experiment.study.id")) %>%
                      dplyr::select(source_hash, source_version_date, clowder_id, fk_doc_id)

                    # Match to extraction doc
                    tmp = res %>%
                      dplyr::select(source_hash, source_version_date) %>%
                      merge(map_file %>%
                              dplyr::filter(is.na(animal_group.experiment.study.id)) %>%
                              dplyr::select(clowder_id, fk_doc_id))

                    # Combine origin and extraction document associations
                    res = dplyr::bind_rows(res, tmp)

                    # Return res
                    res
                  },

                  "source_cosmos" = {
                    # Match origin docs
                    res <- res %>%
                      dplyr::left_join(map_file %>%
                                         dplyr::filter(document_type == "origin") %>%
                                         dplyr::select(clowder_id, fk_doc_id, document_name,
                                                       name, casrn, study_type, study_reference, species, year) %>%
                                         dplyr::distinct(),
                                       by=c("study_type", "study_reference", "year")) %>%
                      dplyr::select(source_hash, source_version_date, clowder_id, fk_doc_id)

                    # Match to extraction doc
                    tmp = res %>%
                      dplyr::select(source_hash, source_version_date) %>%
                      merge(map_file %>%
                              dplyr::filter(document_type == "extraction") %>%
                              dplyr::select(clowder_id, fk_doc_id))

                    # Combine origin and extraction document associations
                    res = dplyr::bind_rows(res, tmp) %>%
                      dplyr::distinct()

                    # Return res
                    res
                  },
                  "source_opp" = {
                    # Associate the origin docs based on chemical names
                    origin_docs <- map_file %>%
                      dplyr::filter(is.na(parent_flag))
                    #Perform a left join on chemical names to match clowder ids and document names
                    res <- res %>%
                      dplyr::select(name, source_hash, source_version_date) %>%
                      dplyr::left_join(origin_docs %>%
                                         dplyr::select(name = Chemical, clowder_id, fk_doc_id),
                                       by = "name")

                    # Match to extraction doc
                    extraction_doc <- map_file %>%
                      dplyr::filter(!is.na(parent_flag))
                    tmp = res %>%
                      dplyr::select(name, source_hash, source_version_date) %>%
                      merge(extraction_doc %>%
                              dplyr::select(clowder_id, fk_doc_id))

                    # Combine origin and extraction document associations
                    res = rbind(res, tmp)

                    #Return the mapped res with document names and clowder ids
                    res
                  },
                  "source_who_jecfa_adi" = {
                    # Associates origin documents to records based on filename
                    origin_docs <- map_file %>%
                      dplyr::filter(is.na(parent_flag))

                    # Separates the lists of chemical id
                    origin_docs = origin_docs %>%
                      tidyr::separate_rows(chemical_id, sep="; ") %>%
                      dplyr::mutate(chemical_id = as.numeric(chemical_id))

                    res1 <- res %>%
                      dplyr::select(source_hash, source_version_date, chemical_id = who_jecfa_chemical_id) %>%
                      dplyr::left_join(origin_docs %>%
                                         dplyr::select(clowder_id, filename, chemical_id, fk_doc_id),
                                       by = "chemical_id")

                    # Associates extraction document to all records
                    extraction_docs <- map_file %>%
                      dplyr::filter(!is.na(parent_flag))

                    res2 = res %>%
                      dplyr::select(source_hash, source_version_date) %>%
                      merge(extraction_docs %>%
                              dplyr::select(clowder_id, fk_doc_id, filename))

                    # Combines both associations back into one data frame
                    res <- dplyr::bind_rows(res1, res2) %>%
                      dplyr::arrange(source_hash)
                    #Return the mapped res with document names and clowder ids
                    res
                  },
                  "source_who_jecfa_tox_studies" = {
                    # Associates origin documents to records based on filename
                    origin_docs <- map_file %>%
                      dplyr::filter(is.na(parent_flag))

                    # Separates the lists of chemical id
                    origin_docs = origin_docs %>%
                      tidyr::separate_rows(chemical_id, sep="; ") %>%
                      dplyr::mutate(chemical_id = as.numeric(chemical_id))

                    res1 <- res %>%
                      dplyr::select(source_hash, source_version_date, chemical_id = who_jecfa_chemical_id) %>%
                      dplyr::left_join(origin_docs %>%
                                         dplyr::select(clowder_id, filename, chemical_id, fk_doc_id),
                                       by = "chemical_id")

                    # Associates extraction document to all records
                    extraction_docs <- map_file %>%
                      dplyr::filter(!is.na(parent_flag))

                    res2 = res %>%
                      dplyr::select(source_hash, source_version_date) %>%
                      merge(extraction_docs %>%
                              dplyr::select(clowder_id, fk_doc_id, filename))

                    # Combines both associations back into one data frame
                    res <- dplyr::bind_rows(res1, res2) %>%
                      dplyr::arrange(source_hash)
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
                      dplyr::filter(document_type == "origin")
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

                    # Perform a left join on chemical names to match chemical names
                    res1 <- res %>%
                      dplyr::select(name, source_hash, source_version_date) %>%
                      dplyr::distinct() %>%
                      dplyr::left_join(origin_docs %>%
                                         dplyr::select(name = "Chemical Name", clowder_id, fk_doc_id) %>%
                                         dplyr::distinct(),
                                       by = "name")

                    # Hard code matches with grep for chemical name
                    unmatched_names = c("BARIUM", "2-BUTOXYETHANOL", "CHROMIUM", "CYANIDE",
                                        "DDD", "DDT", "DDE", "DICHLOROBENZENE", "1,2-DICHLOROETHENE",
                                        "DICHLOROPROPENE", "DINITROTOLUENE", "FLUORIDE",
                                        "HEXACHLOROCYCLOHEXANE", "CHLOROPHENOL", "TIN",
                                        "URANIUM", "HYDRAZINE", "CHLORODIBENZOFURAN", "PHOSPHATE",
                                        "XYLENES", "PHOSPHORUS", "IONIZING RADIATION", "PBDES", "MANGANESE", "(2,4-D)",
                                        "BIS\\(CHLOROMETHYL\\)ETHER", "HMX", "METHYLENEDIPHENYL DIISOCYANATE",
                                        "2-METHYLNAPHTHALENE")

                    # Find matches for those missing matches with grep name to chemical name
                    for(u_name in unmatched_names){
                      origin_replace = unique(origin_docs$clowder_id[grep(paste0("^", u_name), origin_docs$`Chemical Name`)])
                      # if(length(origin_replace) > 1) stop("origin_replace too long")
                      # if(length(origin_replace) == 0) stop("No replacement")
                      res1$clowder_id[grep(u_name, res1$name)] = origin_replace %>%
                        toString()
                      res1$fk_doc_id[grep(u_name, res1$name)] = unique(origin_docs$fk_doc_id[origin_docs$clowder_id %in% origin_replace]) %>%
                        toString()
                    }

                    res1 = res1 %>%
                      tidyr::separate_rows(clowder_id, fk_doc_id, sep = ", ")

                    # associates each record to the extraction document
                    extraction_docs <- map_file %>%
                      dplyr::filter(document_type != "origin")

                    # Perform a left join on chemical names to match chemical names
                    res2 <- res %>%
                      dplyr::select(name, source_hash, source_version_date) %>%
                      dplyr::distinct() %>%
                      merge(extraction_docs %>%
                              dplyr::select(clowder_id, fk_doc_id) %>%
                              dplyr::distinct())
                    #dplyr::rowwise() %>%
                    # Handle case of collapsed document_type
                    #dplyr::mutate(document_type = document_type %>%
                    #                strsplit("|::|", fixed=TRUE) %>%
                    #                unlist() %>%
                    #                stringr::str_squish() %>%
                    #                unique()) %>%
                    #dplyr::ungroup() %>%
                    #dplyr::left_join(extraction_docs %>%
                    #                   dplyr::select(clowder_id, fk_doc_id, atsdr_document_type),
                    #                 by = c("document_type"="atsdr_document_type"))

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

                    # Match to extraction doc
                    tmp = res %>%
                      dplyr::select(source_hash, source_version_date) %>%
                      merge(map_file %>%
                              dplyr::filter(!is.na(parent_flag)) %>%
                              dplyr::select(clowder_id, fk_doc_id))

                    res = res %>%
                      dplyr::bind_rows(tmp)
                    # Return res
                    res
                  },
                  "source_atsdr_pfas_2021" = {
                    # Match to origin doc
                    res <- res %>%
                      dplyr::select(short_ref, source_hash, source_version_date) %>%
                      dplyr::left_join(map_file %>%
                                         dplyr::select(short_ref, clowder_id, fk_doc_id) %>%
                                         dplyr::distinct(),
                                       by = "short_ref")

                    # Match to extraction doc
                    tmp = res %>%
                      dplyr::select(short_ref, source_hash, source_version_date) %>%
                      merge(map_file %>%
                              dplyr::filter(!is.na(parent_flag)) %>%
                              dplyr::select(clowder_id, fk_doc_id))

                    # Combine origin and extraction document associations
                    res = rbind(res, tmp)

                    # Return res
                    res
                  },
                  "source_copper" = {
                    # Join on long_ref
                    res <- res %>%
                      dplyr::select(source_hash, source_version_date, long_ref) %>%
                      dplyr::left_join(map_file %>%
                                         dplyr::select(fk_doc_id, clowder_id, long_ref),
                                       by="long_ref") %>%
                      dplyr::select(source_hash, source_version_date, clowder_id, fk_doc_id)

                    # Match to extraction doc
                    tmp = res %>%
                      dplyr::select(source_hash, source_version_date) %>%
                      merge(map_file %>%
                              dplyr::filter(!is.na(parent_flag)) %>%
                              dplyr::select(clowder_id, fk_doc_id))

                    # Combine origin and extraction document associations
                    res = rbind(res, tmp)
                    # Return res
                    res
                  },
                  "source_gestis_dnel" = {
                    # Sync map_file chemical name cleaning
                    map_file = map_file %>%
                      dplyr::mutate(name = name %>%
                                      # Fix Unicode symbols
                                      fix.replace.unicode() %>%

                                      # Remove trademark symbols
                                      gsub("\u00ae|<U+00ae>", "", .) %>%

                                      # Fix whitespace
                                      gsub("[\r\n][\r\n]", " ", .) %>%
                                      gsub("\u00a0|<U+00A0>", " ", .) %>%

                                      # Fix quotations and apostrophes
                                      gsub("\u201c|<U+201C>|\u201d|<U+201D>", '"', .) %>%
                                      gsub("\u2018|<U+2018>|\u0092|<U+0092>|\u2019|<U+2019>", "'", .) %>%

                                      # Fix superscript/subscript
                                      gsub("\u00b3|<U+00B3>", "3", .) %>%
                                      gsub("\u00b9|<U+00B9>", "1", .) %>%
                                      gsub("\u2070|<U+2070>", "0", .) %>%
                                      gsub("\u00b2|<U+00B2>", "2", .) %>%
                                      gsub("\u2079|<U+2079>", "9", .) %>%
                                      gsub("\u2078|<U+2078>", "8", .) %>%
                                      gsub("\u2074|<U+2074>", "4", .) %>%
                                      gsub("\u2077|<U+2077>", "7", .) %>%
                                      gsub("\u2076|<U+2076>", "6", .) %>%

                                      # Fix general punctuation
                                      gsub("\u00b4|<U+00B4>", "'", .) %>%
                                      gsub("\u2013|<U+2013>", "-", .) %>%
                                      gsub("\u00bf|<U+00BF>", "?", .) %>%

                                      # Fix math symbols
                                      gsub("\u2265|<U+2265>", ">=", .) %>%
                                      gsub("\u00b1|<U+00B1>", "+/-", .) %>%
                                      gsub("\u00b0|<U+00B0>", "", .) %>%
                                      gsub("\u00b0|<U+00B0>", "", .) %>%
                                      gsub("\u2032|<U+2032>", "", .) %>%
                                      gsub("\u00b7|<U+00B7>", "*", .) %>%

                                      # Remove excess whitespace
                                      stringr::str_squish())
                    # Join on name
                    res <- res %>%
                      dplyr::select(source_hash, source_version_date, name) %>%
                      dplyr::left_join(map_file %>%
                                         dplyr::select(fk_doc_id, clowder_id, name),
                                       by="name") %>%
                      dplyr::select(source_hash, source_version_date, clowder_id, fk_doc_id)

                    # Match to extraction doc
                    tmp = res %>%
                      dplyr::select(source_hash, source_version_date) %>%
                      merge(map_file %>%
                              dplyr::filter(!is.na(parent_flag)) %>%
                              dplyr::select(clowder_id, fk_doc_id))

                    # Combine origin and extraction document associations
                    res = rbind(res, tmp)
                    # Return res
                    res
                  },

                  "source_health_canada" = {
                    # Match origin docs
                    # Match based on trv_source
                    res <- res %>%
                      dplyr::select(source_hash, source_version_date, trv_source, long_ref) %>%
                      dplyr::left_join(map_file %>%
                                         dplyr::filter(!is.na(clowder_id)) %>%
                                         dplyr::select(clowder_id, fk_doc_id, trv_source) %>%
                                         dplyr::distinct(), relationship = "many-to-many",
                                       by = "trv_source")


                    # Match to extraction doc
                    tmp = res %>%
                      dplyr::select(source_hash, source_version_date, trv_source) %>%
                      merge(map_file %>%
                              dplyr::filter(is.na(trv_source)) %>%
                              dplyr::select(clowder_id, fk_doc_id))

                    # Combine origin and extraction document associations
                    res = dplyr::bind_rows(res, tmp)

                    # Return res
                    res
                  },

                  "source_heast" = {
                    # associates each origin document to specific record
                    origin_docs <- map_file %>%
                      dplyr::filter(is.na(parent_flag))

                    # Separate rows based on chemical name
                    origin_docs = origin_docs %>%
                      tidyr::separate_rows(`Chemical`, sep="; ")

                    # Match origin docs
                    # Match based on chemical names
                    res1 <- res %>%
                      dplyr::select(source_hash, source_version_date, name) %>%
                      dplyr::left_join(origin_docs %>%
                                         dplyr::filter(!is.na(clowder_id)) %>%
                                         dplyr::select(clowder_id, fk_doc_id, name = "Chemical") %>%
                                         dplyr::distinct(), relationship = "many-to-many",
                                       by = "name")

                    # Hard code matches with grep for chemical name
                    unmatched_names = c("Boron Trifluoride", "Bromophos", "Chlorpyrifos Methyl", "Chlorthiophos",
                                        "Chromium III", "Copper", "Diazinon", "Dichloroethylene",
                                        "Dichlorophenoxy)", "Dimethoate", "Dimethylterephthalate",
                                        "Endothall", "EPTC", "Hexachlorophene",
                                        "Isopropalin", "Mancozeb", "Maneb", "Mephosfolan",
                                        "Octamethylpyrophosphoramide", "Pebulate", "Phorate", "Phthalic Acid, p-", "Profluralin", "Ronnel",
                                        "Silver", "Temephos", "Terbufos", "Thiocyanomethylthio)", "Thiofanox", "Tin",
                                        "Trichloro-2'-Hydroxydiphenylether, 2,2,4'-", "Zinc")

                    # Find matches for those missing matches with grep name to chemical name
                    for(u_name in unmatched_names){
                      origin_replace = unique(origin_docs$clowder_id[grep(paste0("^", u_name), origin_docs$Chemical)])
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
                              dplyr::select(clowder_id, fk_doc_id, name = "Chemical"))

                    # Combine the two associated dataframes back into res
                    res <- rbind(res1, res2) %>%
                      dplyr::arrange(source_hash)

                    # Return res
                    res
                  },
                  "source_doe_pac" = {
                    # Join on name
                    res <- res %>%
                      dplyr::select(source_hash, source_version_date, name) %>%
                      dplyr::left_join(map_file %>%
                                         dplyr::select(fk_doc_id, clowder_id, name),
                                       by="name") %>%
                      dplyr::select(source_hash, source_version_date, clowder_id, fk_doc_id)

                    # Match to extraction doc
                    tmp = res %>%
                      dplyr::select(source_hash, source_version_date) %>%
                      merge(map_file %>%
                              dplyr::filter(!is.na(parent_flag)) %>%
                              dplyr::select(clowder_id, fk_doc_id))

                    # Combine origin and extraction document associations
                    res = rbind(res, tmp) %>%
                      dplyr::distinct()
                    # Return res
                    res
                  },

                  "source_epa_hhtv" = {
                    # associates each origin document to specific record
                    origin_docs <- map_file %>%
                      dplyr::filter(is.na(parent_flag))
                    origin_docs$long_ref <- fix.replace.unicode(origin_docs$long_ref)
                    # Perform a left join to match long_ref
                    res1 <- res %>%
                      tidyr::separate_rows(long_ref, sep=" \\| ") %>%
                      dplyr::select(long_ref, source_hash, source_version_date) %>%
                      dplyr::left_join(origin_docs %>%
                                         dplyr::select(long_ref, clowder_id, fk_doc_id),
                                       by = "long_ref") %>%
                      dplyr::select(-long_ref)
                    # associates each record to the extraction document
                    extraction_docs <- map_file %>%
                      dplyr::filter(!is.na(parent_flag))
                    extraction_docs$document_name <- gsub('.pdf', '', extraction_docs$document_name)

                    # Perform a left join to match document_name
                    res2 <- res %>%
                      dplyr::select(document_name, source_hash, source_version_date) %>%
                      dplyr::left_join(extraction_docs %>%
                                         dplyr::select(document_name, clowder_id, fk_doc_id),
                                       by = "document_name") %>%
                      dplyr::select(-document_name)

                    # Combine the two associated dataframes back into res
                    res <- rbind(res1, res2) %>%
                      dplyr::arrange(source_hash)
                    #Return the mapped res with document names and clowder ids
                    res
                  },

                  "source_who_ipcs" = {
                    # Associate records based off of ntp_study_identifier
                    res = res %>%
                      dplyr::select(name, source_hash, source_version_date) %>%
                      dplyr::left_join(map_file %>%
                                         tidyr::separate_rows(`name`, sep="; ") %>%
                                         dplyr::select(fk_doc_id, clowder_id, name),
                                       by = "name")

                    # Match to extraction doc
                    tmp = res %>%
                      dplyr::select(name, source_hash, source_version_date) %>%
                      merge(map_file %>%
                              dplyr::filter(!is.na(parent_flag)) %>%
                              dplyr::select(clowder_id, fk_doc_id))
                    # Return res
                    res
                  },

                  # Default case, return without mapping
                  res
    )

    # Handle HAWC PFAS 150/430
    if(source_table %in% c("source_hawc_pfas_150","source_hawc_pfas_430")) {
      map = map_file %>%
        dplyr::select(-fk_doc_id)
      if(!"study_name" %in% names(map)){
        map = map %>% dplyr::rename(study_name = `Study Name`)
      }
      title2 = res$title
      title2 = gsub("Registration dossier: |RRegistration dossier: ","", title2)
      title2 = gsub('\\.$','',title2)
      res$title2 = title2
      res$title2 = tolower(stringr::str_trim(title2))
      res$title3 = gsub("\\s*\\([^\\)]+\\)", "", res$title2)
      for(i in 1:nrow(map)) {
        cid = map[i,"clowder_id"]

        docname = map[i,"document_name"]
        title = map[i,"study_name"]
        title2 = stringr::str_trim(tolower(title)) %>%
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
                         by="clowder_id") %>%
        dplyr::select(source_hash, source_version_date, clowder_id, fk_doc_id)

      # Match to extraction doc
      tmp = res %>%
        dplyr::select(source_hash, source_version_date) %>%
        merge(map_file %>%
                dplyr::filter(!is.na(parent_flag)) %>%
                dplyr::select(clowder_id, fk_doc_id))

      # Combine origin and extraction document associations
      res = dplyr::bind_rows(res, tmp)
    }

    # Handle IUCLID case
    if(grepl("iuclid", source_table)){
      res <- res %>%
        dplyr::mutate(source = tolower(source)) %>%
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

  # Clear out associations for source_table source_hash entries not in the current document map
  message("...Clearing out old associations not in current map...")
  if(source_table %in% c("ChemIDPlus", "Uterotrophic Hershberger DB", "ToxRefDB", "ECOTOX")) {
    delete_query = paste0("DELETE FROM documents_records WHERE ",
                          "source_hash IN (SELECT source_hash FROM ", toxval.db, ".toxval WHERE source = '", source_table,"') ",
                          "AND fk_doc_id NOT IN ",
                          "(", toString(unique(map_file$fk_doc_id[!is.na(map_file$fk_doc_id)])), ")")

  } else {
    delete_query = paste0("DELETE FROM documents_records WHERE ",
                          "source_hash IN (SELECT source_hash FROM ", source_table, ") ",
                          "AND fk_doc_id NOT IN ",
                          "(", toString(unique(map_file$fk_doc_id[!is.na(map_file$fk_doc_id)])), ")")
  }
  runQuery(delete_query, source.db)

  # Filter documents_records to only new documents_records
  pushed_doc_records <- runQuery(paste0("SELECT source_hash, fk_doc_id FROM documents_records where source_hash in ('",
                                        paste0(unique(res$source_hash), collapse="', '"),
                                        "')"),
                                 source.db) %>%
    tidyr::unite(col="pushed_docs", source_hash, fk_doc_id)

  if(!("source_version_date" %in% names(res))) res$source_version_date = as.character(NA)
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
                   db=source.db)
  } else {
    message("...no new documents_records entries to push...moving on...")
  }

  # Update document_type
  set_clowder_doc_type(source_table=source_table,
                       clowder_url=clowder_url,
                       clowder_api_key=clowder_api_key,
                       source.db=source.db,
                       ds_id = "5e31dc1e99323f93a9f5cec0",
                       clowder_id_list=res %>%
                         dplyr::select(clowder_id) %>%
                         dplyr::distinct())
}
