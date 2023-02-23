#--------------------------------------------------------------------------------------
#' Import of PPRTV CPHEA source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#--------------------------------------------------------------------------------------
import_source_pprtv_cphea <- function(db,chem.check.halt=F) {
  printCurrentFunction(db)
  source = "PPRTV CPHEA"
  source_table = "source_pprtv_cphea"
  dir = paste0(toxval.config()$datapath,"pprtv_cphea/pprtv_cphea_files/")
  tmp = readxl::read_xlsx(paste0(dir, "pprtv_cphea_full.xlsx"))
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  #
  # the final file should have column names that include "name" and "casrn"
  # additionally, the names in res need to match names in the source
  # database table. You do not need to add any of the generic columns
  # described in the SOP - they will get added in source_prep_and_load

  # Define helper function for cleaning up scientific notation
  parse_scientific <- function(s) {
    if (grepl("^[0-9]+\\.?[0-9]* ?[Xx] ?10\\^.*$", s)) {
      mantissa <- as.numeric(gsub(" ?[Xx].*", "", s))
      exponent <- as.numeric(gsub(".*\\^", "", s))
      return(mantissa * 10^exponent)
    }
    return(suppressWarnings(as.numeric(s)))
  }

  # Handle specific toxval_type fixes
  # Empty dataframe to collect handled cases
  res_parsed = data.frame()
  # Add temp ID to help filter out handled cases
  res = tmp %>%
    dplyr::mutate(temp_id = 1:n())

  # Collect all the records that lack a toxval
  res_parsed = res %>%
    filter(is.na(`RfD (mg/kg-day)`) & (is.na(PoD) | PoD == ":") & is.na(`RfC (mg/m^3))`)
           & is.na(`Oral Slope Factor`) & is.na(`Unit Risk Factor`)) %>%
    select(-c(`RfD (mg/kg-day)`, PoD, `RfC (mg/m^3))`, `Oral Slope Factor`, `Unit Risk Factor`, `RfC (mg/kg-day)`)) %>%
    mutate(toxval_type = NA, toxval_numeric = NA, toxval_units = NA) %>%
    rbind(res_parsed, .)
  res = res %>% filter(!temp_id %in% res_parsed$temp_id)
  # Resetting before the pivoting so we don't lose fields in the pivot
  res_parsed$temp_id = NA

  # Get list of fields to pivot
  toxval_type_list = c("RfD (mg/kg-day)", "PoD", "RfC (mg/m^3))",
                       "Oral Slope Factor", "Unit Risk Factor", "RfC (mg/kg-day)")
  # Apply general pivot longer fixes for all toxval_type fields
  res = res %>%
    tidyr::pivot_longer(cols = toxval_type_list,
                        names_to="toxval_type",
                        values_to="toxval_numeric",
                        values_transform = list(toxval_numeric=as.character)) %>%
    # Split out units for RfD and RfC variants
    tidyr::separate(toxval_type, c("toxval_type", "toxval_units"), sep="\\(",
                    extra="merge", fill="right", remove=FALSE) %>%
    # Remove extra parentheses from units
    dplyr::mutate(toxval_units = gsub("\\(|\\)", "", toxval_units),
                  temp_id = 1:n()) %>%
    dplyr::mutate(across(c("toxval_type", "toxval_units", "toxval_numeric"),
                         ~stringr::str_squish(.)))

  # Per splitting
  res_parsed = res %>%
    filter(grepl(" per ", toxval_numeric)) %>%
    tidyr::separate(toxval_numeric, c("toxval_numeric", "toxval_units"), sep=" per ") %>%
    rbind(res_parsed, .)
  res = res %>% filter(!temp_id %in% res_parsed$temp_id)
  # PoD splitting
  res_parsed = res %>%
    filter(grepl(" : ", toxval_numeric)) %>%
    mutate(toxval_numeric = stringr::str_squish(toxval_numeric)) %>%
    tidyr::separate(toxval_numeric, c("toxval_type", "toxval_numeric"), sep=" : ",
                    fill="right") %>%
    tidyr::separate(toxval_numeric, c("toxval_numeric", "toxval_units"), sep=" ") %>%
    rbind(res_parsed, .)
  res = res %>% filter(!temp_id %in% res_parsed$temp_id)
########################################################################
  # Straggler PoD without a PoD listed (just ":")
  res_parsed = res %>%
    filter(grepl("^:", toxval_numeric)) %>%
    mutate(toxval_numeric = gsub(":", "", toxval_numeric) %>%
             stringr::str_squish()) %>%
    tidyr::separate(toxval_numeric, c("toxval_numeric", "toxval_units"), sep=" ",
                    extra="merge", fill="left") %>%
    rbind(res_parsed, .)
  res = res %>% filter(!temp_id %in% res_parsed$temp_id)
  # Recombine
  res = res %>%
    rbind(res_parsed) %>%
    select(-temp_id) %>%
    # Filter out unnecssarily created toxval_numeric-types during pivoting
    filter(!(is.na(toxval_numeric) & !is.na(toxval_type)))

  # View(res %>% select(toxval_numeric, toxval_numeric_new) %>% distinct())
  # Check if parsing/filtering was successful
  # if(nrow(res) != nrow(tmp)){
  #   stop("Issue parsing, extra rows added")
  # }

  # TODO Handle cases where fields are "known" empty because they weren't in the original table
  # Case: https://cfpub.epa.gov/ncea/pprtv/chemicalLanding.cfm?pprtv_sub_id=1550

  # TODO Handle case like https://cfpub.epa.gov/ncea/pprtv/chemicalLanding.cfm?pprtv_sub_id=1815
  # toxval_numeric remained as the units, toxval_numeric is in the notes section

  # Fix scientific notation issue
  res = res %>%
    # Add rowwise so mutate can vectorize the parse_scientific function
    dplyr::rowwise() %>%
    dplyr::mutate(toxval_numeric = parse_scientific(toxval_numeric))

  # Standardize the names
  res0 <- res %>%
    dplyr::rename(name = chemical, endpoint = System, critical_effect = Basis, uncertainty_factor = UF,
                  species = `Species Studied`, study_reference = `Principal Study`,
                  tumor_site = `Tumor site(s)`, cancer_type = Cancer) %>%
    # Clean up punctuation/spacing for endpoint column
    mutate(endpoint = gsub(" ,", ", ", endpoint)) %>%
    dplyr::rowwise() %>%
    # Determine study_type
    dplyr::mutate(study_type = ifelse(grepl("Cancer|Carcinogenic", table_title),
                                      "Cancer", ifelse(grepl("Subchronic", table_title),
                                                       "Subchronic", "Chronic")))
  # Fix names
  names(res0) <- names(res0) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()


  # TODO: Split duration column into study_duration_value and _units


  # Combine two separate notes columns from the extraction into one
  # We don't need to worry about having values in both fields for one record, as they're exclusionary by construction
  res0$notes <- paste0(tidyr::replace_na(res0$note, ""), tidyr::replace_na(res0$note_in_body, ""))
  res0 <- res0 %>% select(-c(note, note_in_body))

  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source=source,table=source_table,res=res0,F,T,T)
}
