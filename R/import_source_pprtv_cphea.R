#--------------------------------------------------------------------------------------
#' Import of PPRTV CPHEA source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#--------------------------------------------------------------------------------------
import_generic_source <- function(db,chem.check.halt=F) {
  printCurrentFunction(db)
  source = "PPRTV CPHEA"
  source_table = "source_pprtv_cphea"
  dir = paste0(toxval.config()$datapath,"pprtv_cphea/pprtv_cphea_files/")
  # TODO files = ls(dir)
  # TODO tmp = readxl::read_excel()
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
    if (grepl("[0-9]\\.?[0-9]* ?[Xx] ?10\\^.*", s)) {
      mantissa <- as.double(gsub(" ?[Xx].*", "", s))
      exponent <- as.double(gsub(".*\\^", "", s))
      return(mantissa * 10^exponent)
    }
    else {
      return(s)
    }
  }

  # Pivot RfD column to toxval_type, _numeric, _units
  rfd <- tmp %>%
    filter(!is.na(`RfD (mg/kg-day)`)) %>%
    pivot_longer(`RfD (mg/kg-day)`,
                 names_to = c("toxval_type", "toxval_units"), names_sep = " ",
                 values_to = "toxval_numeric") %>%
    # Standardize columns
    select(-c(PoD, `RfC (mg/m^3))`, `Oral Slope Factor`, `Unit Risk Factor`, `RfC (mg/kg-day)`))
  # Clean up toxval_units column
  rfd$toxval_units <- sapply(rfd$toxval_units, function(x) {gsub("\\(|\\)", "", x)})
  # Parse scientific notation in toxval_numeric column
  rfd$toxval_numeric <- sapply(rfd$toxval_numeric, parse_scientific)

  # Pivot PoD
  pod <- tmp %>% filter(!is.na(PoD) & PoD != ":")
  # Extract toxval_type, _numeric, and _units
  pod$toxval_type <- sapply(pod$PoD, function(x){stringr::str_trim(gsub(":.*", "", x))})
  # regmatches() outputs a list, so coerce to character
  pod$toxval_numeric <- as.character(sapply(pod$PoD, function(x){regmatches(x, regexpr("[0-9]+\\.?[0-9]*", x))}))
  pod$toxval_units <- sapply(pod$PoD, function(x){stringr::str_trim(gsub(".*[0-9] *", "", x))})
  # Standardize columns
  pod <- select(pod, -c(`RfD (mg/kg-day)`, PoD, `RfC (mg/m^3))`, `Oral Slope Factor`, `Unit Risk Factor`, `RfC (mg/kg-day)`))

  # Pivot RfC (mg/m^3) column
  rfc_m3 <- tmp %>%
    filter(!is.na(`RfC (mg/m^3))`)) %>%
    pivot_longer(`RfC (mg/m^3))`,
                 names_to = c("toxval_type", "toxval_units"), names_sep = " ",
                 values_to = "toxval_numeric") %>%
    select(-c(`RfD (mg/kg-day)`, PoD, `Oral Slope Factor`, `Unit Risk Factor`, `RfC (mg/kg-day)`))
  # Clean up toxval_units column
  rfc_m3$toxval_units <- sapply(rfc_m3$toxval_units, function(x) {gsub("\\(|\\)", "", x)})
  # Parse scientific notation in toxval_numeric column
  rfc_m3$toxval_numeric <- sapply(rfc_m3$toxval_numeric, parse_scientific)

  # Pivot Oral Slope Factor
  osf <- tmp %>%
    filter(!is.na(`Oral Slope Factor`))
  # Extract toxval_type, _numeric, and _units
  osf$toxval_type <- "Oral Slope Factor"
  osf$toxval_numeric <- sapply(osf$`Oral Slope Factor`, function(x){parse_scientific(stringr::str_trim(gsub("per.*", "", x)))})
  osf$toxval_units <- sapply(osf$`Oral Slope Factor`, function(x){stringr::str_trim(gsub(".*per", "", x))})
  # Standardize columns
  osf <- select(osf, -c(`RfD (mg/kg-day)`, PoD, `RfC (mg/m^3))`, `Oral Slope Factor`, `Unit Risk Factor`, `RfC (mg/kg-day)`))

  # Pivot Unit Risk Factor
  urf <- tmp %>%
    filter(!is.na(`Unit Risk Factor`))
  # Extract toxval_type, _numeric, and _units
  urf$toxval_type <- "Unit Risk Factor"
  urf$toxval_numeric <- sapply(urf$`Unit Risk Factor`, function(x){parse_scientific(stringr::str_trim(gsub("per.*", "", x)))})
  urf$toxval_units <- sapply(urf$`Unit Risk Factor`, function(x){stringr::str_trim(gsub(".*per", "", x))})
  # Standardize columns
  urf <- select(urf, -c(`RfD (mg/kg-day)`, PoD, `RfC (mg/m^3))`, `Oral Slope Factor`, `Unit Risk Factor`, `RfC (mg/kg-day)`))

  # If there are future versions of this source, they may have data in the RfC (mg/kg-day) column, but this version doesn't.

  # Collect all the records that lack a toxval
  remainder <- tmp %>%
    filter(is.na(`RfD (mg/kg-day)`) & (is.na(PoD) | PoD == ":") & is.na(`RfC (mg/m^3))`)
           & is.na(`Oral Slope Factor`) & is.na(`Unit Risk Factor`)) %>%
    select(-c(`RfD (mg/kg-day)`, PoD, `RfC (mg/m^3))`, `Oral Slope Factor`, `Unit Risk Factor`, `RfC (mg/kg-day)`)) %>%
    mutate(toxval_type = NA, toxval_numeric = NA, toxval_units = NA)

  # Bind together all the lengthened dataframes and the remainders
  res0 <- rbind(rfd, pod, rfc_m3, osf, urf, remainder)

  # Standardize the names
  res0 <- res0 %>%
    rename(name = chemical, endpoint = System, critical_effect = Basis, uncertainty_factor = UF,
                  species = `Species Studied`, study_reference = `Principal Study`,
                  tumor_site = `Tumor site(s)`, cancer_type = Cancer)
  names(res0) <- names(res0) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  # Clean up punctuation/spacing for endpoint column
  res0$endpoint <- sapply(res0$endpoint, function(x){gsub(" ,", ", ", x)})

  # Create study_type column
  res0$study_type <- NA
  for (i in 1:nrow(res0)) {
    if (grepl("Cancer|Carcinogenic", res0$table_title[i])) {
      res0$study_type[i] <- "Cancer"
    } else if (grepl("Subchronic", res0$table_title[i])) {
      res0$study_type[i] <- "Subchronic"
    } else {
      res0$study_type[i] <- "Chronic"
    }
  }

  # TODO: Split duration column into study_duration_value and _units


  # Combine two separate notes columns from the extraction into one
  # We don't need to worry about having values in both fields for one record, as they're exclusionary by construction
  res0$notes <- paste0(tidyr::replace_na(res0$note, ""), tidyr::replace_na(res0$note_in_body, ""))
  res0 <- res0 %>% select(-c(note, note_in_body))

  res = source.specific.transformations(res0)


  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source=source,table=source_table,res=res,F,T,T)
}
