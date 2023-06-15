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
#'  \code{\link[openxlsx]{read.xlsx}}
#'  \code{\link[stringr]{str_trim}}
#'  \code{\link[dplyr]{rename}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{recode}}
#'  \code{\link[tidyr]{pivot_longer}}
#' @rdname import_source_atsdr_mrls_2022
#' @importFrom openxlsx read.xlsx
#' @importFrom stringr str_squish
#' @importFrom dplyr rename mutate recode
#' @importFrom tidyr pivot_longer
#--------------------------------------------------------------------------------------
import_generic_source <- function(db,chem.check.halt=F) {
  printCurrentFunction(db)
  source = "ATSDR MRLs 2022"
  source_table = "source_atsdr_mrls_2022"
  dir = paste0(toxval.config()$datapath,"atsdr_mrls_2022/atsdr_mrls_2022_files/")
  file = paste0(dir,"atsdr_mrls_2022_raw.xlsx")
  res0 = openxlsx::read.xlsx(file)
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
    dplyr::rename(study_type=duration_category,
                  exposure_route=route,
                  study_duration_value=duration_value,
                  casrn=cas_number,
                  MRL=mrl_value,
                  study_duration_units=duration_units,
                  critical_effect=endpoint,
                  toxval_units=mrl_unit) %>%
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
    # calculating NOAEL
    dplyr::mutate(MRL=as.numeric(MRL),
           NOAEL=MRL*total_factors) %>%
    # expanding toxval_type to include MRL and NOAEL columns
    tidyr::pivot_longer(
      cols= c("MRL", "NOAEL"),
      names_to= "toxval_type",
      values_to= "toxval_numeric"
    )

  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source=source,table=source_table,res=res,F,T,T)
}




