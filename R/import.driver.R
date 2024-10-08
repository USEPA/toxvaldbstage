#--------------------------------------------------------------------------------------
#' @description Function to run all import scripts to fill toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.chek.halt If TRUE and there are bad chemical names or casrn, #' stop to look at the results in indir/chemcheck.xlsx
#' @param do.clean If TRUE, delte data from all tables before reloading
#' @param chem.check.halt PARAM_DESCRIPTION, Default: FALSE
#' @title import.driver
#' @return None
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname import.driver
#' @export
#--------------------------------------------------------------------------------------
import.driver <- function(db,
                          chem.check.halt=FALSE,
                          do.clean=FALSE) {
  printCurrentFunction(db)

  # Clear out all tables in toxval_source if specified
  if(do.clean) {
    tlist = runQuery("show tables",db)[,1] #%>%
      # Only delete source tables
      #.[grepl("source_", .)]
    for(table in tlist) {
      if(substr(table,1,7)=="source_") {
        query = paste0("delete from ",table)
        cat(query,"\n")
        runQuery(query,db)
      }
    }
  }
  drop = FALSE
  # If specified, drop all source tables in toxval_source
  if(drop){
    tlist = runQuery("show tables",db)[,1] %>%
    # Only drop source tables to be rebuilt entirely
    .[grepl("source_", .)] %>%
      # Do not drop source_chemical, source_audit, or source_chemical_index tables
    .[!grepl("chemical|audit", .)]
    for(table in tlist) {
      if(substr(table,1,7)=="source_") {
        query = paste0("drop table ",table)
        cat(query,"\n")
        runQuery(query,db)
      }
    }
  }

  #import_envirotox_source(db,chem.check.halt=chem.check.halt)# EnviroTox_v2
  #import_heast_source(db,chem.check.halt=chem.check.halt) # HEAST
  #import_rsl_source(db,chem.check.halt=chem.check.halt) # RSL
  #import_dod_source(db,chem.check.halt=chem.check.halt) # DOD [v1]
  #import_doe_source(db,chem.check.halt=chem.check.halt) # DOE Protective Action Criteria [v1]
  #import_oppt_source(db,chem.check.halt=chem.check.halt) # EPA OPPT
  #import_pfas_150_sem_source(db,chem.check.halt=chem.check.halt) # PFAS 150 SEM
  import_atsdr_pfas_2021_source(db,chem.check.halt=chem.check.halt) # ATSDR PFAS 2021 [v1]
  # import_source_atsdr_mrls(db,chem.check.halt=chem.check.halt) # ATSDR MRLs 2023 (TBD)
  import_caloehha_source(db,chem.check.halt=chem.check.halt) # Cal OEHHA [v1]
  import_copper_source(db,chem.check.halt=chem.check.halt) # Copper Manufacturers [v1]
  import_doe_benchmarks_source(db,chem.check.halt=chem.check.halt) # DOE Wildlife Benchmarks [v1]
  # import_efsa2_source(db,chem.check.halt=chem.check.halt)  # EFSA2
  import_efsa_source(db,chem.check.halt=chem.check.halt) # EFSA
  import_hawc_pfas_source(db, hawc_num=150, chem.check.halt=chem.check.halt) # HAWC PFAS 150
  import_hawc_pfas_source(db, hawc_num=430, chem.check.halt=chem.check.halt) # HAWC PFAS 430
  import_hawc_source(db,chem.check.halt=chem.check.halt) # HAWC
  import_health_canada_source(db,chem.check.halt=chem.check.halt) # Health Canada
  import_hess_source(db,chem.check.halt=chem.check.halt) # HESS
  import_hpvis_source(db,chem.check.halt=chem.check.halt) # HPVIS
  import_iris_source(db,chem.check.halt=chem.check.halt) # IRIS
  import_niosh_source(db,chem.check.halt=chem.check.halt) # NIOSH
  import_opp_source(db,chem.check.halt=chem.check.halt) # EPA OPP
  import_source_penn_dep_toxvalues(db,chem.check.halt=chem.check.halt) # Pennsylvania DEP ToxValues
  import_source_penn_dep_mscs(db,chem.check.halt=chem.check.halt) # Pennsylvania DEP MSCs
  # import_pfas_summary_pods_source(db,chem.check.halt=chem.check.halt) # PFAS Summary PODs
  import_pprtv_ncea_source(db,chem.check.halt=chem.check.halt) # PPRTV (NCEA)
  import_pprtv_ornl_source(db,chem.check.halt=chem.check.halt) # PPRTV (ORNL)
  import_test_source(db,chem.check.halt=chem.check.halt) # TEST
  # import_wignall_source(db,chem.check.halt=chem.check.halt) # Wignall
  # Trouble loading
  # import_flex_source(db,chem.check.halt=chem.check.halt) # ACTOR old sources (10)

  import_dod_meg_source(db,chem.check.halt=chem.check.halt) # DOD MEG
  import_doe_pac_source(db,chem.check.halt=chem.check.halt) # DOE PAC
  # import_pfas_150_sem_v2_source(db,chem.check.halt=chem.check.halt) # PFAS 150 SEM v2
  import_heast_source(db,chem.check.halt=chem.check.halt) # HEAST
  import_oppt_source(db,chem.check.halt=chem.check.halt) # OPPT
  import_rsl_source(db,chem.check.halt=chem.check.halt) # RSL
  # Moved to end since it takes the longest
  import_echa_echemportal_api_source(db,chem.check.halt=chem.check.halt) # ECHA echemportal API
  # Alaska DEC [v1]
  # EPA AEGL [v1]
  # Mass. Drinking Water Standards [v1]
  # OSHA Air Contaminants [v1]
  # OW Drinking Water Standards [v1]
  # USGS HBSL [v1]
  # WHO IPCS [v1]
  # import_flex_source(db,chem.check.halt=chem.check.halt)

  ###import_echa3_source(db,chem.check.halt=chem.check.halt) # ECHA
  ###import_echa_echemportal_source(db,chem.check.halt=chem.check.halt) # ECHA eChemPortal 2020
  ###import_echa_iuclid_source(db,chem.check.halt=chem.check.halt) # ECHA IUCLID
  ###import_echa_source(db,chem.check.halt=chem.check.halt) # ECHA POC

}
