#--------------------------------------------------------------------------------------
#' @description Load chiu Source into toxval_source, only HDMI values
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile The input file ./chiu/chiu_files/RfD_HDMI_mc_results.csv
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn, #' stop to look at the results in indir/chemcheck.xlsx
#' @title import_chiu_source
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
#'  \code{\link[dplyr]{select}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{distinct}}
#'  \code{\link[tidyr]{drop_na}}
#'  \code{\link[stringr]{str_remove}}, \code{\link[stringr]{modifiers}}, \code{\link[stringr]{str_trim}}
#' @rdname import_chiu_source
#' @export
#' @importFrom readr read_csv
#' @importFrom dplyr select mutate distinct
#' @importFrom tidyr drop_na
#' @importFrom stringr str_remove fixed str_squish str_trim str_extract str_detect
#--------------------------------------------------------------------------------------
import_chiu_source <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "Chiu"
  source_table = "source_chiu"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2018-06-28")
  dir = paste0(toxval.config()$datapath,"chiu/chiu_files/")
  file = paste0(dir,"RfD_HDMI_mc_results.csv")
  res0 = readr::read_csv(file, col_types = readr::cols())
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  # Rename list
  rename_list = c("casrn"="strCAS",
                  "name"="strName",
                  "subsource"="Source",
                  "toxval_type"="Type",
                  "toxval_numeric"="PrRfD.approx",
                  "record_url"="strHyperlink",
                  "long_ref"="strReference",
                  "toxval_units"="strUnitsPOD",
                  "critical_effect_1"="Effect",
                  "critical_effect_2"="strCriticalEffect",
                  "year"="strDateAssessed",
                  "species"="Species",
                  "strain"="Strain",
                  "study_duration_value"="strDuration",
                  "study_duration_units"="Duration type",
                  "sex"="tblEffect_strSex",
                  "uf"="numUF",
                  "ufa"="numUFa",
                  "ufh"="numUFh",
                  "ufs"="numUFs",
                  "ufl"="numUFl",
                  "ufd"="numUFd",
                  "ufother"="numUFother",
                  "exposure_route"="Route")

  res1 <- res0 %>%
    dplyr::rename(tidyselect::any_of(rename_list)) %>%
    dplyr::select(tidyselect::any_of(names(rename_list))) %>%
    dplyr::distinct() %>%
    # Extract toxval_type and toxval_units
    tidyr::separate(toxval_type, into=c("toxval_type", "toxval_units"), sep="\\(") %>%
    dplyr::mutate(toxval_units = toxval_units %>%
                    gsub("\\)", "", .))

  # Mutate columns in combined data as needed
  res <- res1 %>%
    dplyr::mutate(year = ifelse(stringr::str_detect(year, "/"), stringr::str_extract(year, "\\d{4}"), year)) %>%
    dplyr::mutate(year = ifelse(stringr::str_detect(year, " "), stringr::str_trim(stringr::str_extract(year, "(?<=\\s)/S+")), year)) %>%
    tidyr::unite(critical_effect_1, critical_effect_2, col="critical_effect", sep = ": ", na.rm=TRUE) %>%
    dplyr::mutate(
      # Remove extraneous whitespace
      dplyr::across(tidyselect::where(is.character), stringr::str_squish),
      casrn = as.character(casrn),
      year = as.numeric(year),
      # Moved from deprecated load script
      exposure_method = "-",
      risk_assessment_class = "chronic",
      toxval_type = "HDMI",
      study_type = "chronic",
      toxval_units = "mg/kg-day",
      exposure_method = ifelse(grepl("other", tolower(exposure_route)), "other",
                               ifelse(grepl("diet", exposure_route), "diet",
                                      ifelse(grepl("drinking water", exposure_route), "drinking water",
                                             ifelse(grepl("gavage", exposure_route), "gavage", "-")))),
      exposure_route = ifelse(grepl("Oral", exposure_route), "oral",
                              ifelse(exposure_route == "Other", "other", "-")),
      critical_effect = gsub("\\[", "(", critical_effect),
      critical_effect = gsub("\\]", ")", critical_effect),
      species = tolower(species),
      sex = sex %>%
        gsub("Both", "M/F", .) %>%
        toupper()
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(casrn = casrn %>%
                    fix.casrn()) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    tidyr::drop_na(toxval_numeric)

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"

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
                       chem.check.halt=chem.check.halt,
                       hashing_cols=toxval.config()$hashing_cols)
}

