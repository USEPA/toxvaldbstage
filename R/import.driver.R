#--------------------------------------------------------------------------------------
#' Function to run all import scripts to fill toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.chek.halt If TRUE and there are bad chemical names or casrn,
#' stop to look at the results in indir/chemcheck.xlsx
#' @param do.clean If TRUE, delte data from all tables before reloading
#--------------------------------------------------------------------------------------
import.driver <- function(db="res_toxval_source_v5",
                          chem.check.halt=FALSE,
                          do.clean=FALSE) {
  printCurrentFunction(db)

  if(do.clean) {
    tlist = runQuery("show tables",sdb)[,1]
    for(table in tlist) {
      if(substr(table,1,7)=="source_") {
        query = paste0("delete from ",table)
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
  import_atsdr_pfas_source(db,chem.check.halt=chem.check.halt) # ATSDR PFAS [v1]
  import_atsdr_source(db,chem.check.halt=chem.check.halt) # ATSDR MRLs 2020 [v1]
  import_caloehha_source(db,chem.check.halt=chem.check.halt) # Cal OEHHA [v1]
  import_chiu_source(db,chem.check.halt=chem.check.halt) # Chiu [v1]
  import_copper_source(db,chem.check.halt=chem.check.halt) # Copper Manufacturers [v1]
  import_cosmos_source(db,chem.check.halt=chem.check.halt) # COSMOS [v1]
  import_dod_ered_source(db,chem.check.halt=chem.check.halt) # DOD ERED [v1]
  import_doe_benchmarks_source(db,chem.check.halt=chem.check.halt) # DOE Wildlife Benchmarks [v1]
  import_echa_echemportal_api_source(db,chem.check.halt=chem.check.halt) # ECHA echemportal API
  import_efsa2_source(db,chem.check.halt=chem.check.halt)  # EFSA2
  import_efsa_source(db,chem.check.halt=chem.check.halt) # EFSA
  import_hawc_pfas_150_source(db,chem.check.halt=chem.check.halt) # HAWC PFAS 150
  import_hawc_pfas_430_source(db,chem.check.halt=chem.check.halt) # HAWC PFAS 430
  import_hawc_source(db,chem.check.halt=chem.check.halt) # HAWC
  import_health_canada_source(db,chem.check.halt=chem.check.halt) # Health Canada
  import_hess_source(db,chem.check.halt=chem.check.halt) # HESS
  import_hpvis_source(db,chem.check.halt=chem.check.halt) # HPVIS
  import_iris_source(db,chem.check.halt=chem.check.halt) # IRIS
  import_lanl_source(db,chem.check.halt=chem.check.halt) # DOE
  import_niosh_source(db,chem.check.halt=chem.check.halt) # NIOSH
  import_opp_source(db,chem.check.halt=chem.check.halt) # EPA OPP
  import_penn_source(db,chem.check.halt=chem.check.halt) # Pennsylvania DEP ToxValues
  import_pfas_summary_pods_source(db,chem.check.halt=chem.check.halt) # PFAS Summary PODs
  import_pprtv_ncea_source(db,chem.check.halt=chem.check.halt) # PPRTV (NCEA)
  import_pprtv_ornl_source(db,chem.check.halt=chem.check.halt) # PPRTV (ORNL)
  import_test_source(db,chem.check.halt=chem.check.halt) # TEST
  import_wignall_source(db,chem.check.halt=chem.check.halt) # Wignall
  # Alaska DEC [v1]
  # Cal DPH [v1]
  # EPA AEGL [v1]
  # FDA CEDI [v1]
  # Mass. Drinking Water Standards [v1]
  # OSHA Air Contaminants [v1]
  # OW Drinking Water Standards [v1]
  # Pennsylvania DEP MCLs [v1]
  # USGS HBSL [v1]
  # WHO IPCS [v1]
  import_flex_source(db,chem.check.halt=chem.check.halt)

  ###import_echa3_source(db,chem.check.halt=chem.check.halt) # ECHA
  ###import_echa_echemportal_source(db,chem.check.halt=chem.check.halt) # ECHA eChemPortal 2020
  ###import_echa_iuclid_source(db,chem.check.halt=chem.check.halt) # ECHA IUCLID
  ###import_echa_source(db,chem.check.halt=chem.check.halt) # ECHA POC

}
