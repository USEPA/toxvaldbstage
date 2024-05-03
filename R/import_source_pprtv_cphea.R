#--------------------------------------------------------------------------------------
#' @title import_source_pprtv_cphea
#' @description Import PPRTV (CPHEA) source data into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @param do.summary_data If TRUE, add PPRTV CPHEA Summary data to table before insertion
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
#'  \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{reexports}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{replace_na}}, \code{\link[tidyr]{drop_na}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{distinct}}
#'  \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_extract}}
#'  \code{\link[tidyselect]{all_of}}
#' @rdname import_source_pprtv_cphea
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom tidyr pivot_longer all_of separate replace_na drop_na
#' @importFrom dplyr mutate across case_when select where distinct
#' @importFrom stringr str_squish str_extract
#' @importFrom tidyselect any_of
#--------------------------------------------------------------------------------------
import_source_pprtv_cphea <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE, do.summary_data=FALSE) {
  printCurrentFunction(db)
  source = "PPRTV (CPHEA)"
  source_table = "source_pprtv_cphea"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2023-01-23")
  dir = paste0(toxval.config()$datapath,"pprtv_cphea/pprtv_cphea_files/")
  file = paste0(dir,"pprtv_cphea_full.xlsx")
  res0 = readxl::read_xlsx(file, guess_max=21474836, col_types="text")
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  # Define helper function for cleaning up scientific notation
  parse_scientific <- function(s) {
    # Handle scientific notation conversion (either 10's or e notation)
    if(grepl("[Xx]", s) && grepl("^[0-9]+\\.?[0-9]* ?[Xx] ?10\\^.*$", s)){
      mantissa <- as.numeric(gsub(" ?[Xx].*", "", s))
      exponent <- as.numeric(gsub(".*\\^", "", s))
      return(mantissa * 10^exponent)
    } else if(grepl("[eE]", s) && grepl("^(-?[0-9]*)\\.?[0-9]+[eE]?[-\\+]?[0-9]+$", s)){
      mantissa <- as.numeric(gsub(" ?[eE].*", "", s))
      exponent <- as.numeric(gsub(".*?[eE]", "", s))
      return(mantissa * 10^exponent)
    }
    return(suppressWarnings(as.numeric(s)))
  }

  # Get list of fields to pivot
  toxval_type_list = c("RfD (mg/kg-day)", "PoD", "RfC (mg/m^3)",
                       "Oral Slope Factor", "Unit Risk Factor", "RfC (mg/kg-day)")

  # Apply general pivot longer fixes for all toxval_type fields
  res = res0 %>%
    tidyr::pivot_longer(cols = tidyr::all_of(toxval_type_list),
                        names_to="toxval_type",
                        values_to="toxval_numeric",
                        values_transform = list(toxval_numeric=as.character)) %>%
    # Split out units for RfD and RfC variants
    tidyr::separate(toxval_type, c("toxval_type", "toxval_units"), sep="\\(",
                    extra="merge", fill="right", remove=FALSE) %>%
    # Remove extra parentheses from units
    dplyr::mutate(toxval_units = gsub("\\(|\\)", "", toxval_units)) %>%
    dplyr::mutate(dplyr::across(c("toxval_type", "toxval_units", "toxval_numeric"),
                                ~stringr::str_squish(.))) %>%

    dplyr::rename(duration_original = Duration) %>%

    dplyr::mutate(
      # Add new column names
      name = chemical,
      endpoint = System %>%
        gsub(" ,", ", ", .),
      uncertainty_factor = UF,
      species = `Species Studied` %>%
        tolower(),
      study_reference = `Principal Study`,
      tumor_site = `Tumor site(s)`,
      cancer_type = Cancer,
    )

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"

  # Add summary data to df before prep and load
  res$document_type = "PPRTV Webpage"
  if(do.summary_data){
    # Import manually curated PPRTV CPHEA Summary information
    summary_file = "source_pprtv_cphea_summary_curation.xlsx"
    res1 = readxl::read_xlsx(paste0(dir, summary_file), col_types="text") %>%
      dplyr::mutate(document_type = 'PPRTV Summary') %>%
      .[ , (names(.) %in% names(res))]
    res = res %>%
      dplyr::bind_rows(res1) %>%
      dplyr::distinct()
  } else {
    res = res %>%
      dplyr::distinct()
  }

  res = res %>%
    dplyr::mutate(
      # Add extra columns
      subsource = "EPA ORD CPHEA",
      source_url = "https://www.epa.gov/pprtv/basic-information-about-provisional-peer-reviewed-toxicity-values-pprtvs",
      long_ref = study_reference %>%
        gsub(",$", "", .),
      guideline = confidence,

      # Set appropriate study_type
      study_type = dplyr::case_when(
        document_type == "PPRTV Summary" ~ study_type,
        grepl("Subchronic", table_title) ~ "subchronic",
        grepl("Cancer|Carcinogenic", table_title) ~ "cancer",
        TRUE ~ "chronic"
      ) %>% tolower(),

      # Clean toxval_units
      toxval_units = dplyr::case_when(
        # Extract values in toxval_numeric
        grepl("mg", toxval_numeric) ~ stringr::str_extract(toxval_numeric, "mg.+"),
        grepl("per", toxval_numeric) ~ gsub(".+per", "", toxval_numeric),
        TRUE ~ toxval_units
      ) %>%
        fix.replace.unicode() %>%
        gsub("\\^", "", .) %>%
        stringr::str_squish(),

      # Clean toxval_type
      toxval_type = dplyr::case_when(
        # Extract values in toxval_numeric
        grepl(":", toxval_numeric) ~ gsub(":.+", "", toxval_numeric),
        toxval_type == "Oral Slope Factor" ~ "cancer slope factor",
        toxval_type == "Unit Risk Factor" ~ "cancer unit risk",
        TRUE ~ toxval_type
      ),

      # Uncomment if splitting toxval_type into toxval_subtype
      # # Case of BMCL(1SD) to BMCL1SD for subtype extraction consistency with BMDL1SD(HED)
      # toxval_type = toxval_type %>%
      #   gsub("(1SD)", "1SD", ., fixed = TRUE),

      toxval_numeric = toxval_numeric %>%
        gsub("per.+", "", .) %>%
        gsub("mg.+", "", .) %>%
        gsub(".+:", "", .) %>%
        sapply(FUN=parse_scientific) %>%
        stringr::str_squish(),

      # Correctly format factor units
      toxval_units = dplyr::case_when(
        is.na(toxval_units) ~ as.character(NA),
        grepl("\\)\\-1", toxval_units) ~ toxval_units,
        toxval_type %in% c("cancer slope factor", "cancer unit risk") ~ paste0("(", toxval_units, ")-1"),
        TRUE ~ toxval_units
      ),

      note = note %>%
        dplyr::na_if(""),
      note_in_body = note_in_body %>%
        dplyr::na_if("")
    ) %>%
    # Combine two separate notes columns from the extraction into one
    tidyr::unite(col = "notes", note, note_in_body, na.rm=TRUE) %>%
    dplyr::mutate(
      # Handle cases like https://cfpub.epa.gov/ncea/pprtv/chemicalLanding.cfm?pprtv_sub_id=1815
      # toxval_numeric remained as the units, toxval_numeric is in the notes section
      toxval_with_units = stringr::str_extract(notes, "[0-9]+\\.?[0-9]* [^ ]* "),
      toxval_numeric = dplyr::case_when(
        is.na(toxval_numeric) & !is.na(toxval_type) ~  gsub(" .*$", "", toxval_with_units),
        TRUE ~ toxval_numeric
      ),
      toxval_units = dplyr::case_when(
        is.na(toxval_numeric) & !is.na(toxval_type) ~  gsub("^[^ ]* ", "", toxval_with_units),
        TRUE ~ toxval_units
      ),

      # Uncomment if splitting toxval_type into toxval_subtype
      # # Extract toxval_subtype from toxval_type where available
      # toxval_subtype = stringr::str_extract(toxval_type, "\\((.+)\\)", group=1),
      # toxval_type = toxval_type %>%
      #   gsub("\\(.+\\)", "", .) %>%
      #   stringr::str_squish(),

      # Extract sex from species when possible
      sex = dplyr::case_when(
        grepl("male; female", sex) ~ "male/female",
        grepl("male and female|both|m\\/f|m and f", species) ~ "male/female",
        grepl("\\/f", species) ~ "female",
        grepl("\\/m\\b|male|\\bM\\b", species) ~ "male",
        TRUE ~ sex
      ),

      # Extract strain from species when possible
      strain = dplyr::case_when(
        grepl("cd|CD", species) ~ "CD Sprague Dawley",
        grepl("s\\-d|S\\-D", species) ~ "Sprague Dawley",
        TRUE ~ strain
      ),

      # Hardcode species for several records
      species = dplyr::case_when(
        toxval_type %in% c("RfD", "RfC", "cancer slope factor", "cancer unit risk") ~ "human",
        study_reference == "Biodynamics 1988" & name == "Butyltin Compounds, mono-" ~ "rat",
        study_reference == "Kawakami et al., 2015" & name == "2-Nitropropane" ~ "rat",
        study_reference == "Lewis et al., (1979), Ulrich et al. (1977)" & name == "2-Nitropropane" ~ "rat",
        study_reference == "Lewis et al. (1979) and Ulrich et al. (1977)" & name == "2-Nitropropane" ~ "rat",
        TRUE ~ species
      ) %>%
        # Clean species
        tolower() %>%
        gsub("monley", "monkey", .) %>%
        gsub("rats", "rat", .) %>%
        gsub("rat\\/mouse|rat; mouse", "rat, mouse", .) %>%
        gsub("mice", "mouse", .) %>%
        gsub("m\\/f", "", .) %>%
        gsub("\\/.+|cd|s\\-d", "", .) %>%
        gsub(" and ", ", ", .) %>%
        gsub("\\/", ", ", .) %>%
        gsub(",,", ",", .) %>%
        stringr::str_squish(),

      # Reset human strain (Replaced rat species with human for RfC, etc.)
      strain = dplyr::case_when(
        species == "human" ~ NA,
        TRUE ~ strain
      ),

      # Clean initial Duration value
      duration_clean = gsub("^([0-9]+\\s?hr?/d,?\\s)?([0-9]+\\s?d(ay)?/wk?)?,?\\s?(for\\s|on\\s)?",
                      "", duration_original) %>%
        gsub(", postweaning|gavage study", "", .) %>%
        gsub("weejs", "weeks", .) %>%
        gsub("\\bOne\\b", "1", .) %>%
        gsub("F0-F3", "3", .) %>%
        gsub("\\-day", " day", .) %>%
        gsub("Gestation days", "GD", .) %>%
        gsub("6 weeks, then 12", "18", .) %>%
        gsub("1 generation \\(12 months\\)", "12 months", .) %>%
        gsub("28d\\)", "28 days)", .) %>%
        gsub("16wk", "16 weeks", .) %>%
        gsub("13wk", "13 weeks", .) %>%
        gsub("14 days prior to mating through day 3 of lactation", "17 days", .),

      # Extract study_duration_value and study_duration_units from duration_clean field
      study_duration_value = dplyr::case_when(
        grepl("[0-9\\.]+\\s*\\-?\\s*[0-9\\.]*\\s*(?:day|hour|month|week|year|generation)", duration_clean) ~ stringr::str_extract(duration_clean,
                                                                                                                      "([0-9\\.]+\\s*\\-?\\s*[0-9]*)\\s*(?:day|hour|month|week|year|generation)",
                                                                                                                      group=1),
        grepl("GD|PND", duration_clean) ~ duration_clean %>%
          gsub("\\s?(?:to|\\-)\\s?PND|\\s?to\\s?PND", "-", .) %>%
          fix.replace.unicode() %>%
          stringr::str_extract("([0-9\\.]+\\s*\\-?\\s*[0-9\\.]*)", group=1),
        grepl("lifetime", duration_clean) ~ "1",
        TRUE ~ as.character(NA)
      ) %>% fix.replace.unicode() %>% gsub("\\s?\\-\\s?", "-", .) %>% gsub("\\s.+", "", .),

      # Follow same patterns as above for study_duration_units
      study_duration_units = dplyr::case_when(
        grepl("[0-9\\.]+\\s*\\-?\\s*[0-9\\.]*\\s*(?:day|hour|month|week|year|generation)", duration_clean) ~ stringr::str_extract(duration_clean,
                                                                                                                      "[0-9\\.]+\\s*\\-?\\s*[0-9\\.]*\\s*(day|hour|month|week|year|generation)",
                                                                                                                      group=1),
        # Old GD handling (calculate range as single "day" value)
        # grepl("GD", duration_clean) ~ "GD",

        grepl("GD", duration_clean) ~ "gestational days",
        grepl("PND", duration_clean) ~ "postnatal days",
        grepl("lifetime", duration_clean) ~ "lifetime",
        TRUE ~ as.character(NA)
      ),

      # Set experimental_record and human_ra
      experimental_record = dplyr::case_when(
        toxval_type %in% c("RfD", "RfC", "cancer slope factor", "cancer unit risk") ~ "No",
        TRUE ~ "Yes"
      ),
      human_ra = dplyr::case_when(
        toxval_type %in% c("RfD", "RfC", "cancer slope factor", "cancer unit risk") ~ "Yes",
        TRUE ~ "No"
      ),

      # Set values to NA to prepare for tidyr::unite
      tumor_site = tumor_site %>%
        dplyr::na_if("-"),
      cancer_type = cancer_type %>%
        dplyr::na_if("-"),
    ) %>%
    # Build critical_effect depending on assessment_type (NA removed)
    tidyr::unite(col="critical_effect_noncancer", endpoint, basis, sep = ": ", na.rm = TRUE, remove=FALSE) %>%
    tidyr::unite(col="critical_effect_cancer", tumor_site, cancer_type, sep = ": ", na.rm = TRUE, remove=FALSE) %>%
    dplyr::mutate(
      critical_effect = dplyr::case_when(
        assessment_type == "Cancer Assessment" ~ critical_effect_cancer,
        TRUE ~ critical_effect_noncancer
      )
    ) %>%
    dplyr::select(-tidyselect::any_of(c("critical_effect_noncancer", "critical_effect_cancer")))

    # Old GD handling (calculate range as single "day" value)
    # # Fix GD study_duration values (subtract top range from low range)
    # tidyr::separate(
    #   col="study_duration_value",
    #   into=c("study_duration_low", "study_duration_high"),
    #   sep="-",
    #   remove=FALSE,
    #   fill="right"
    # ) %>%
    # dplyr::mutate(
    #   critical_effect = critical_effect %>%
    #     dplyr::na_if(""),
    #   study_duration_low = as.numeric(study_duration_low),
    #   study_duration_high = as.numeric(study_duration_high),
    #
    #   study_duration_value = dplyr::case_when(
    #     study_duration_units == "GD" ~ as.character(study_duration_high - study_duration_low + 1),
    #     TRUE ~ study_duration_value
    #   ) %>% stringr::str_squish(),
    #
    #   study_duration_units = gsub("GD", "day", study_duration_units)
    # ) %>%
    # dplyr::select(!tidyselect::any_of(c("study_duration_low", "study_duration_high")))

  # Add missing toxval_units (imputed from RfC/RfD) to NOAEL cases
  # Narrow down the search first
  noael_fix = res %>%
    dplyr::filter(toxval_type == "NOAEL", is.na(toxval_units), document_type=="PPRTV Webpage") %>%
    dplyr::select(name, study_type, table_title) %>%
    dplyr::distinct()

  # Fix each individual case
  for(i in seq_len(nrow(noael_fix))){
    # Pull case of chemical from same table and same type
    n_fix = res %>%
      dplyr::filter(name == noael_fix$name[i], study_type == noael_fix$study_type[i],
                    table_title == noael_fix$table_title[i],
                    !is.na(toxval_units), document_type=="PPRTV Webpage") %>%
      # Select available units
      dplyr::select(toxval_units)

    # Skip if multiple units
    if(nrow(n_fix) > 1){
      message("Issue with NOAEL chemical: ", noael_fix$name[i])
      next
    } else {
      # Make replacement of NA
      res$toxval_units[is.na(res$toxval_units) &
                          res$name == noael_fix$name[i] &
                          res$study_type == noael_fix$study_type[i] &
                          res$table_title == noael_fix$table_title[i]] = n_fix$toxval_units
    }
  }

  res = res %>%
    # Filter out entries without valid toxval columns
    tidyr::drop_na(toxval_type, toxval_numeric, toxval_units) %>%
    # Fix unicode symbols in character fields and ensure numeric fields are of numeric type
    dplyr::mutate(dplyr::across(dplyr::where(is.character),
                                ~fix.replace.unicode(.) %>%
                                  stringr::str_squish() %>%
                                  # Set empty strings to NA
                                  dplyr::na_if("") %>%
                                  tidyr::replace_na("-")),
                  toxval_numeric = as.numeric(toxval_numeric))%>%
    # Drop temp column
    dplyr::select(-toxval_with_units) %>%
    # Drop duplicates
    dplyr::distinct()

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
