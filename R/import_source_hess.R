#--------------------------------------------------------------------------------------
#' @description Import of HESS data source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_source_hess
#' @return None. Data is processed into the database
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[stringr]{str_trim}}
#' @rdname import_generic_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_squish
#--------------------------------------------------------------------------------------
import_source_hess <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "HESS"
  source_table = "source_hess"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2023-05-17")
  dir = paste0(toxval.config()$datapath,"hess/hess_files/")
  file = paste0(dir,"hess_20231109_fixed.xlsx")
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
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  #res = source.specific.transformations(res0)

  res <- res0 %>%
    # Renaming columns
    dplyr::rename(casrn="cas_number",
                  name="chemical_name(s)",
                  ec_number="additional_ids",
                  study_type="endpointpath",
                  critical_effect="effect",
                  toxval_type="endpoint",
                  tissue="organ(tissue)",
                  strain="strain",
                  species="test_organisms_(species)",
                  toxval_numeric="value_meanvalue",
                  toxval_numeric_qualifier="value_qualifier",
                  toxval_units="value_unit",
                  ) %>%
    dplyr::mutate(
      # Removing "EC Number:" from ec_number field
      ec_number = gsub("EC Number:", "", ec_number),
      # Removing "Human Health Hazards#" from study_type field
      # Lowercasing the study_type field values
      study_type = gsub("Human Health Hazards#", "", study_type) %>%
        tolower(),
      # Removing parenthesis around values in route_of_administration field
      route_of_administration = gsub("[()]", "", route_of_administration),
      toxval_subtype = "Repeated Dose Toxicity HESS"
    ) %>%
    # Splitting route_of_administration field into exposure_route and exposure_method
    tidyr::separate(route_of_administration, c("exposure_route", "exposure_method"),
                    sep=" ", fill="right", extra = "merge", remove=FALSE) %>%
    tidyr::separate(name, into=c("name", "name_extra"), sep=";", extra='merge', fill='right') %>%
    dplyr::select(-name_extra) %>%
    dplyr::distinct()

  # Check route_of_administration splitting
  # View(res %>% select(route_of_administration, exposure_route, exposure_method) %>% distinct())

  # Group combine effect for study groups
  crit_groups = res %>%
    tidyr::unite(critical_effect, critical_effect, tissue, sep=" - ", remove=FALSE) %>%
    dplyr::select(name, toxval_type, toxval_numeric, critical_effect) %>%
    dplyr::group_by(name, toxval_type, toxval_numeric) %>%
    dplyr::mutate(critical_effect = paste0(unique(critical_effect), collapse = " | ")) %>%
    dplyr::distinct()

  # Join back in combined critical_effect groups
  res = res %>%
    dplyr::select(-critical_effect) %>%
    dplyr::left_join(crit_groups,
                     by=c("name", "toxval_type", "toxval_numeric")) %>%
    distinct()

  # Omit blank rows
  res <- res[rowSums(is.na(res)) != ncol(res), ]

  # Add version date. Can be converted to a mutate statement as needed
  res$source_version_date <- src_version_date

  # Make records distinct
  res <- res %>%
    dplyr::distinct()

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db=db,
                       source=source,
                       table=source_table,
                       res=res,
                       do.reset=do.reset,
                       do.insert=do.insert,
                       chem.check.halt=chem.check.halt,
                       hashing_cols=toxval.config()$hashing_cols)
}




