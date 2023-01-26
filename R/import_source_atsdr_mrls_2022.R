#--------------------------------------------------------------------------------------
#' Import of ATSDR MRLs 2022 source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#--------------------------------------------------------------------------------------
import_generic_source <- function(db,chem.check.halt=F) {
  printCurrentFunction(db)
  source = "ATSDR MRLs 2022"
  source_table = "source_atsdr_mrls_2022"
  dir = paste0(toxval.config()$datapath,"atsdr_mrls_2022/atsdr_mrls_2022_files/")
  file = paste0(dir,"atsdr_mrls_2022_raw.xlsx")
  res0 = read.xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  #
  # the final file should have column names that include "name" and "casrn"
  # additionally, the names in res need to match names in the source
  # database table. You do not need to add any of the generic columns
  # described in the SOP - they will get added in source_prep_and_load
  #
  
  # Standardize the names
  names(res0) <- names(res0) %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    stringr::str_squish() %>%
    tolower()
  
  res <- res0 %>%
    # Renaming columns
    dplyr::rename(study_type=duration_category,
                  exposure_route=route,
                  study_duration_value=duration_value,
                  casrn=cas_number,
                  study_duration_units=duration_units,
                  toxval_numeric=mrl_value,
                  critical_effect=endpoint,
                  toxval_units=mrl_unit) %>%
    mutate(toxval_type="MRL")

  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source=source,table=source_table,res=res,F,T,T)
}




