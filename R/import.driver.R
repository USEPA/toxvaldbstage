#--------------------------------------------------------------------------------------
#' Fnction to run all import scripts
#'
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param indir The directory where the output file will be placed
#' @param infile The input file ./chiu/chiu_files/Full_RfD_databaseQAed-FINAL.xlsx
#' @param chem.chek.halt If TRUE and there are bad chemical names or casrn,
#' stop to look at the results in indir/chemcheck.xlsx
#--------------------------------------------------------------------------------------
import.driver <- function(db="dev_toxval_source_v4") {
  printCurrentFunction(db)

  # ATSDR PFAS 2021 [v1]
  import_atsdr_pfas_2021_source(db)

  # ATSDR PFAS [v1]
  import_atsdr_pfas_source(db)

  # ATSDR MRLs 2020 [v1]
  import_atsdr_source(db)

  # Cal OEHHA [v1]
  import_caloehha_source(db)

  # Chiu [v1]
  import_chiu_source(db)

  # Copper Manufacturers [v1]
  import_copper_source(db)

  # COSMOS [v1]
  import_cosmos_source(db)

  # DOD ERED [v1]
  import_dod_ered_source(db)

  # DOD [v1]
  import_dod_source(db)

  # DOE Wildlife Benchmarks [v1]
  import_doe_benchmarks_source(db)

  # DOE Protective Action Criteria [v1]
  import_doe_source(db)

  # ECHA
  import_echa3_source(db)

  # ECHA echemportal API
  import_echa_echemportal_api_source(db)

  # ECHA eChemPortal 2020
  import_echa_echemportal_source(db)

  # ECHA IUCLID
  import_echa_iuclid_source(db)

  # ECHA POC
  import_echa_source(db)

  # EFSA2
  import_efsa2_source(db)

  # EFSA
  import_efsa_source(db)

  # EnviroTox_v2
  import.envirotox.source(db)

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
  import_flex_source(db)

  # HAWC PFAS 150
  import_hawc_pfas_150_source(db)

  # HAWC PFAS 430
  import_hawc_pfas_430_source(db)

  # HAWC
  import_hawc_source(db)

  # Health Canada
  import_health_canada_source(db)

  # HEAST
  import_heast_source(db)

  # HESS
  import_hess_source(db)

  # HPVIS
  import_hpvis_source(db)

  # IRIS
  import_iris_source(db)

  import_lanl_source(db)

  # NIOSH
  import_niosh_source(db)

  # EPA OPP
  import_opp_source(db)

  # EPA OPPT
  import_oppt_source(db)

  # Pennsylvania DEP ToxValues
  import_penn_source(db)

  # PFAS 150 SEM
  import_pfas_150_sem_source(db)

  # PFAS Summary PODs
  import_pfas_summary_pods_source(db)

  # PPRTV (NCEA)
  import_pprtv_ncea_source(db)

  # PPRTV (ORNL)
  import_pprtv_ornl_source(db)

  # RSL
  import_rsl_source_info(db)

  # TEST
  import_test_source(db)

  # Wignall
  import_wignall_source(db)

}
