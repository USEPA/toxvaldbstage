#--------------------------------------------------------------------------------------
#' @description Import of EFSA OpenFoodTox 2022 source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_efsa_source
#' @return None; data is pushed to toxval_source
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
#'  \code{\link[dplyr]{rename}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{recode}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{distinct}}
#'  \code{\link[tidyr]{separate}}, \code{\link[tidyr]{reexports}}
#' @rdname import_efsa_source_2022
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_squish
#' @importFrom dplyr rename mutate recode across select distinct
#' @importFrom tidyr separate matches
#' @importFrom tidyselect where
#--------------------------------------------------------------------------------------
import_efsa_source <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "EFSA"
  source_table = "source_efsa"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2022-06-16")
  dir = paste0(toxval.config()$datapath,"efsa/efsa_files/")
  file = paste0(dir,"OpenFoodToxTX22784_2022.xlsx")

  # Read in separate data frames for relevant sheets, pulling only specified fields
  comp_res0 = readxl::read_xlsx(file, sheet="COMPONENT") %>%
    dplyr::select(tidyselect::all_of(c("SUB_NAME", "SUB_COM_ID", "QUALIFIER", "SUB_CASNUMBER"))) %>%
    dplyr::filter(QUALIFIER == "as such") %>%
    dplyr::select(-QUALIFIER) %>%
    dplyr::distinct()

  study_res0 = readxl::read_xlsx(file, sheet="STUDY") %>%
    dplyr::select(tidyselect::all_of(c("SUB_COM_ID", "TOX_ID", "OP_ID"))) %>%
    dplyr::distinct()

  endpoint_res0 = readxl::read_xlsx(file, sheet="ENDPOINTSTUDY_KJ") %>%
    dplyr::select(tidyselect::all_of(
      c("ENDPOINTSTUDY_ID", "TESTSUBSTANCE", "STUDY_CATEGORY", "TESTTYPE", "LIMITTEST", "GUIDELINE_QUALIFIER",
        "GUIDELINEFULLTXT", "DEVIATION", "GLP_COMPL", "SPECIES", "STRAIN", "SEX", "ROUTE", "EXP_DURATION",
        "DURATIONUNIT", "NUMBER_INDIVIDUALS", "CONTROL", "ENDPOINT", "QUALIFIER", "VALUE", "DOSEUNIT", "BASIS",
        "TOXICITY", "TARGETTISSUE", "EFFECT_DESC", "REMARKS", "TOX_ID")
    )) %>%
    dplyr::distinct()

  opinion_res0 = readxl::read_xlsx(file, sheet="OPINION") %>%
    dplyr::select(tidyselect::all_of(c("PUBLICATIONYEAR", "DOI", "URL", "OP_ID"))) %>%
    dplyr::distinct()

  # Join sheets together into res0
  res0 = comp_res0 %>%
    dplyr::left_join(study_res0, by=c("SUB_COM_ID")) %>%
    dplyr::left_join(endpoint_res0, by=c("TOX_ID")) %>%
    dplyr::left_join(opinion_res0, by=c("OP_ID")) %>%
    dplyr::select(-c("SUB_COM_ID", "TOX_ID", "OP_ID")) %>%
    tidyr::drop_na("VALUE") %>%
    dplyr::distinct()

  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  # Standardize the names
  names(res0) <- names(res0) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    gsub("___", "_", .) %>%
    tolower()

  res = res0 %>%
    dplyr::mutate(
      # Renaming columns
      record_url = url,
      toxval_type = endpoint,
      toxval_numeric = value,
      toxval_numeric_qualifier = qualifier,
      toxval_units = doseunit,
      critical_effect = basis,
      study_type = testtype,
      study_duration_value = exp_duration,
      study_duration_units = durationunit,
      human_eco = study_category,
      year = publicationyear,
      casrn = sub_casnumber,
      name = sub_name,
      chemical = testsubstance,

      # Recoding/fixing entries in study_type, human_eco, and study_duration_units
      study_type = study_type %>%
        dplyr::recode(
          "acute toxicity" = "acute",
          "chronic/long term toxicity" = "chronic/long-term",
          "reproduction toxicity" = "reproductive",
          "short-term toxicity" = "short-term",
          "study with volunteers" = "human"
        ),
      human_eco = human_eco %>%
        dplyr::recode(
          "Animal (non-target species) health" = "human health",
          "Animal (target species) health" = "human health",
          "Ecotox (soil compartment)" = "eco",
          "Ecotox (water compartment)" = "eco",
          "Human health" = "human health"
        ),
      study_duration_units = study_duration_units %>%
        dplyr::recode(
          "h" = "hours",
          "D" = "days",
          "week" = "weeks",
          "month" = "months",
          "year" = "years"
        ),
      toxval_numeric_qualifier = toxval_numeric_qualifier %>%
        dplyr::recode(
          "ca." = "~"
        ),

      # Fill in general information
      source_url = "https://zenodo.org/record/5076033#.Y9fEoXbMI2z",
      subsource_url = source_url,
      source_download = "OpenFoodToxTX22784_2022.xlsx",
    ) %>%
    # splitting ROUTE into exposure_route and exposure_method columns
    tidyr::separate(., route, c("exposure_route","exposure_method"), sep=": ", fill="right", remove=FALSE) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), fix.replace.unicode)) %>%
    dplyr::distinct()

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"

  # Perform deduping
  hashing_cols = toxval.config()$hashing_cols
  res = toxval.source.import.dedup(res, hashing_cols = hashing_cols)

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
                       chem.check.halt=chem.check.halt,
                       hashing_cols=toxval.config()$hashing_cols)
}
