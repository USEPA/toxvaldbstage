#--------------------------------------------------------------------------------------
#' @description Import of ATSDR MRLs 2022 source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @title FUNCTION_TITLE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_replace}}
#'  \code{\link[dplyr]{rename}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{bind}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{case_when}}
#'  \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{reexports}}
#'  \code{\link[tibble]{add_column}}
#' @rdname import_rsl_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_squish str_replace_all
#' @importFrom dplyr rename filter mutate bind_rows across case_when
#' @importFrom tidyr pivot_longer separate matches
#' @importFrom tibble add_column
#--------------------------------------------------------------------------------------
import_source_atsdr_mrls <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE){
  printCurrentFunction(db)
  source = "ATSDR MRLs"
  source_table = "source_atsdr_mrls"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2023-04-01")
  dir = paste0(toxval.config()$datapath,"atsdr_mrls/atsdr_mrls_files/")
  file = paste0(dir,"atsdr_mrls_2023-04-01_raw.xlsx")
  res0 = readxl::read_xlsx(file)
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
    gsub("[*]", "", .) %>%
    stringr::str_squish() %>%
    tolower()

  res <- res0 %>%
    # Renaming columns
    dplyr::rename(study_type=duration,
                  exposure_route=route,
                  casrn=cas_number,
                  toxval_numeric = mrl,
                  toxval_units = units,
                  critical_effect=endpoint,
                  doc_status = status,
                  doc_cover_date = cover_date) %>%
    # Recoding/fixing entries in critical_effect
    dplyr::mutate(critical_effect=dplyr::recode(critical_effect,
                                                "Body Wt." = "body weight",
                                                "Develop." = "developmental",
                                                "Endocr." = "endocrine",
                                                "Gastro." = "gastrointestinal",
                                                "Hemato." = "hematological",
                                                "Hepatic,Endocr." = "hepatic, endocrine",
                                                "Immuno." = "immunological",
                                                "Lymphor." = "lymphatic",
                                                "Metab." = "metabolic",
                                                "Musculo." = "musculoskeletal",
                                                "Neurol." = "neurological",
                                                "Neurol.,Develop." = "neurological, developmental",
                                                "Neurol.,Hepatic" = "neurological, hepatic",
                                                "Neurol.,Reprod." = "neurological, reproductive",
                                                "Reprod." = "reproductive",
                                                "Resp." = "respiratory") %>%
                    tolower()) %>%
    # Set toxval_type as MRL and convert values to numeric type
    dplyr::mutate(toxval_numeric=as.numeric(toxval_numeric),
                  toxval_type = "MRL",
                  source_url = "https://wwwn.cdc.gov/TSP/MRLS/mrlsListing.aspx",
                  # Recode study_type from abbreviations
                  study_type = dplyr::recode(study_type,
                                             "Int." = "Intermediate",
                                             "Chr."="Chronic"),
                  # Recode exposure_route from abbreviations
                  exposure_route = dplyr::recode(exposure_route,
                                        'Rad.'='External Radiation',
                                        'Inh.'='Inhalation')) %>%
    # Fix greek symbols
    dplyr::mutate(dplyr::across(c("name", "toxval_units"), ~fix.greek.symbols(.))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(study_duration_value = ifelse(study_type == "Acute", "1-14 days",
                                          ifelse(study_type == "Chronic", ">1 year",
                                                 "15-364 days"))) %>%
    tidyr::separate(study_duration_value, c("study_duration_value", "study_duration_units"), sep=" ") %>%
    #Reorder columns
    select(name, casrn, exposure_route, study_type, study_duration_value, study_duration_units, toxval_type, everything())

  # Add version date. Can be converted to a mutate statement as needed
  res$source_version_date <- src_version_date
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db=db,
                       source=source,
                       table=source_table,
                       res=res,
                       do.reset=do.reset,
                       do.insert=do.insert,
                       chem.check.halt=chem.check.halt)
}




