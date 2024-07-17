#--------------------------------------------------------------------------------------
#' @description Load EPA OPP data to toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_opp_source
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
#'  \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{separate}}
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{row_number}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{bind_rows}}
#'  \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_extract}}
#' @rdname import_opp_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom tidyr pivot_longer separate
#' @importFrom dplyr filter mutate row_number case_when select bind_rows
#' @importFrom stringr str_squish str_extract
#--------------------------------------------------------------------------------------
import_opp_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "EPA OPP"
  source_table = "source_opp"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2023-01-31")
  dir = paste0(toxval.config()$datapath,"opp/opp_files/")
  file = paste0(dir,"source_opp_raw_20230131.xlsx")
  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  res = res0 %>%
    dplyr::mutate(
      # Add ID values before pivot
      group_id = dplyr::row_number()
    ) %>%
    # Pivot on columns with toxval_type/numeric/units information
    tidyr::pivot_longer(
      cols=c("Acute or One Day PAD (RfD) (mg/kg/day)",
             "Acute or One Day HHBPs (ppb)",
             "Chronic or Lifetime PAD (rfD) (mg/kg/day)",
             "Chronic or Lifetime HHBPs (ppb)",
             "Cancer Quantification (Q1) Values (CSF) (mg/kg/per day) -1",
             "Carcinogenic HHBP (E-6 to E-4 ) (ppb)"),
      names_to = "toxval_type_units",
      values_to = "toxval_numeric",
      values_drop_na = TRUE
    ) %>%

    # Remove entries with missing toxval_numeric
    dplyr::filter(!(toxval_numeric %in% c("-", "--", NA, as.character(NA)))) %>%

    # Split toxval_numeric into lower and upper range values
    tidyr::separate(
      toxval_numeric,
      into=c("toxval_numeric", "toxval_numeric_upper"),
      sep="-",
      fill="right"
    ) %>%

    dplyr::mutate(
      # Add fields and conduct basic cleaning operations
      name = `Common Name and Reference Document` %>%
        gsub("&amp;", "&", .) %>%
        fix.replace.unicode() %>%
        stringr::str_squish(),
      casrn = `CAS Number` %>%
        gsub("\\s+", "; ", .) %>%
        stringr::str_squish(),
      subsource_url = "https://www.epa.gov/sdwa/2021-human-health-benchmarks-pesticides",
      source_url = subsource_url,

      # Fix character and whitespace issues with name/casrn
      name = gsub("&amp;", "&", name),
      casrn = stringr::str_squish(casrn),

      # Set relationship ID to handle ranged toxval_numeric entries
      range_relationship_id = dplyr::row_number(),

      # Set toxval information based on toxval_type_units
      toxval_type = dplyr::case_when(
        grepl("RfD", toxval_type_units, ignore.case=TRUE) ~ "PAD (RfD)",
        grepl("Carcinogenic HHBP", toxval_type_units) ~ "Carcinogenic HHBP",
        grepl("HHBP", toxval_type_units) ~ "HHBP",
        grepl("CSF", toxval_type_units) ~ "cancer slope factor",
        TRUE ~ as.character(NA)
      ),
      toxval_units = dplyr::case_when(
        grepl("mg\\/kg\\/day", toxval_type_units) ~ "mg/kg/day",
        grepl("\\(mg\\/kg\\/per day\\) \\-1", toxval_type_units) ~ "(mg/kg/day)-1",
        grepl("ppb", toxval_type_units) ~ "ppb",
        TRUE ~ as.character(NA)
      ),
      risk_assessment_class = dplyr::case_when(
        grepl("Acute", toxval_type_units) ~ "acute",
        grepl("Chronic", toxval_type_units) ~ "chronic",
        grepl("Cancer|Carcinogenic", toxval_type_units) ~ "cancer",
        TRUE ~ as.character(NA)
      ),
      study_type = risk_assessment_class,
      toxval_subtype = study_type,

      # Set sensitive_lifestage field based on risk_assessment_class
      sensitive_lifestage = dplyr::case_when(
        risk_assessment_class == "acute" ~ `Acute HHBP Sensitive Lifestage/ Population`,
        risk_assessment_class == "chronic" ~ `Chronic HHBP Sensitive Lifestage/Population`,
        TRUE ~ as.character(NA)
      ),

      # Extract sex, lifestage, and population from sensitive_lifestage when possible
      sex = stringr::str_extract(sensitive_lifestage, "(Female|\\bMale)", group=1) %>%
        tolower(),
      lifestage = stringr::str_extract(sensitive_lifestage, "(Children|[0-9]+\\-[0-9]+ yrs)", group=1),
      population = sensitive_lifestage,

      # Get specialized ID values
      cancer_id = dplyr::case_when(
        study_type == "cancer" ~ group_id,
        TRUE ~ NA
      ),
      hhbp_rfd_id = dplyr::case_when(
        grepl("HHBP|RfD", toxval_type) & is.na(cancer_id) ~ group_id,
        TRUE ~ NA
      )
    )

  # Handle entries with upper ranges
  lower_range_res = res %>%
    dplyr::filter(!is.na(toxval_numeric_upper)) %>%
    dplyr::select(-toxval_numeric_upper) %>%
    dplyr::mutate(
      toxval_subtype = stringr::str_c(toxval_subtype, " lower range (TR = 1E-6)"),
      toxval_numeric_qualifier = ">="
    )
  upper_range_res = res %>%
    dplyr::filter(!is.na(toxval_numeric_upper)) %>%
    dplyr::mutate(
      toxval_numeric = toxval_numeric_upper,
      toxval_subtype = stringr::str_c(toxval_subtype, " upper range (TR = 1E-4)"),
      toxval_numeric_qualifier = "<="
    ) %>%
    dplyr::select(-toxval_numeric_upper)
  # Recombine ranged entries with original data
  res = res %>%
    dplyr::filter(is.na(toxval_numeric_upper)) %>%
    dplyr::select(-toxval_numeric_upper) %>%
    dplyr::bind_rows(lower_range_res, upper_range_res) %>%
    dplyr::mutate(toxval_numeric = as.numeric(toxval_numeric))

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"

  # Perform deduping
  res = toxval.source.import.dedup(res)

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




