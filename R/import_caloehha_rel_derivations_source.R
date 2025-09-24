#--------------------------------------------------------------------------------------
#' @description Import of Cal OEHHA REL Derivations source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_caloehha_rel_derivations_source
#' @return None; data is pushed to toxval_source
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[stringr]{str_trim}}
#' @rdname import_generic_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_squish
#' @importFrom dplyr mutate across where
#' @importFrom tidyr replace_na
#--------------------------------------------------------------------------------------
import_caloehha_rel_derivations_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "Cal OEHHA REL derivations"
  source_table = "source_caloehha_rel_derivations"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2025-03-04")
  dir = paste0(toxval.config()$datapath,"caloehha_rel_derivations/caloehha_rel_derivations_files/")
  file = paste0(dir, "CalOEHHA Inhalation noncancer RELs_QC_final.xlsx")
  # Skip first few rows that were manually curated as metadata for the file
  res0 = readxl::read_xlsx(file)

  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  # Define helper function for cleaning up scientific notation strings
  parse_scientific <- function(s) {
    # Handle scientific notation conversion (either 10's or e notation)
    if(grepl("[Xx]", s) && grepl("^[0-9]+\\.?[0-9]* ?[Xx] ?10 ?-?[0-9]+", s)){
      mantissa <- as.numeric(gsub(" ?[Xx].*", "", s))
      exponent <- as.numeric(gsub('.*?[Xx] 10', "", s))
      return(as.character(mantissa * 10^exponent))
    } else if(grepl("[eE]", s) && grepl("^[0-9]+\\.?[0-9]* ?[Ee] ?10 ?-?[0-9]+", s)){
      mantissa <- as.numeric(gsub(" ?[eE].*", "", s))
      exponent <- as.numeric(gsub(".*?[eE]", "", s))
      return(as.character(mantissa * 10^exponent))
    }
    return(s)
  }

  # Add source specific transformations
  res = res0 %>%
    dplyr::select(-`QC row count`, -`percent QC`) %>%
    dplyr::mutate(
      qc_status = dplyr::case_when(
        !is.na(`QC result`) ~ "pass",
        TRUE ~ "undetermined"
      ),
      year = summary_doc_year,
      casrn = dplyr::case_when(
        grepl("No CASRN|group|NOCAS|DSSTox", casrn, ignore.case = TRUE) ~ NA,
        TRUE ~ casrn
      ) %>%
        # Remove parentheses
        gsub("\\s*\\([^)]+\\)", "", .),
      # Fix exposure_method and form
      exposure_form = dplyr::case_when(
        exposure_method == "Food and drinking wate" ~ paste0(exposure_method, "r"),
        TRUE ~ exposure_form
      ) %>% tolower(),
      exposure_method = dplyr::case_when(
        exposure_form %in% c("gavage", "drinking water") ~ exposure_form,
        exposure_form %in% c("occupational") ~ "occupational",
        exposure_method %in% c("Food and drinking wate",
                               "Continuous dietary exposure starting at seven weeks of age for 2 years") ~ "diet",
        TRUE ~ exposure_method
      ) %>% tolower(),
      exposure_form = exposure_form %>%
        gsub("^drinking water$|^gavage$|^occupational$", "-", .) %>%
        gsub("[dust]", "dust", ., fixed = TRUE),
      # Fix toxval_numeric and units
      toxval_numeric = dplyr::case_when(
        toxval_numeric == "none" ~ NA,
        TRUE ~ toxval_numeric
      )
    ) %>%
    # Remove empty rows that only have NA values
    .[rowSums(is.na(.)) < ncol(.), ] %>%
    tidyr::separate_longer_delim(casrn, delim = ", ") %>%
    tidyr::separate_longer_delim(casrn, delim = "; ")

  res = res %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      toxval_numeric = toxval_numeric %>%
        parse_scientific(),
      range_flag = dplyr::case_when(
        !is.na(as.numeric(toxval_numeric)) ~ 0,
        grepl("[0-9]-[0-9]", toxval_numeric) ~ 1,
        TRUE ~ 0
      )) %>%
    ungroup()

  # Handle ranged toxval_numeric values
  ranged_res = res %>%
    dplyr::filter(range_flag == 1)

  if(nrow(ranged_res)) {
    ranged_res = ranged_res %>%
      dplyr::mutate(
        numeric_relationship_id = dplyr::row_number()
      ) %>%
      tidyr::separate(
        col = toxval_numeric,
        into = c("Lower Range", "Upper Range"),
        sep = "-",
        remove = TRUE
      ) %>%
      tidyr::pivot_longer(
        cols = c("Lower Range", "Upper Range"),
        values_to = "toxval_numeric",
        names_to = "numeric_relationship_description"
      )
  } else {
    # Empty dataframe with res cols to bind_rows()
    ranged_res = res[0,]
  }

  # Add ranged data to res
  res = res %>%
    dplyr::filter(range_flag == 0) %>%
    dplyr::bind_rows(ranged_res) %>%
    dplyr::select(-range_flag) %>%
    dplyr::mutate(toxval_numeric = as.numeric(toxval_numeric)) %>%
    tidyr::drop_na(toxval_type, toxval_numeric, toxval_units)

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  res = res %>%
    # Generic cleanup of strings before dedup check
    dplyr::mutate(
      dplyr::across(dplyr::where(is.character), ~tidyr::replace_na(., "-") %>%
                      fix.replace.unicode() %>%
                      stringr::str_squish()),
      dplyr::across(dplyr::where(is.character), ~gsub("\\r|\\n|\\\\r|\\\\n", "", .)),
      dplyr::across(dplyr::where(is.character), ~gsub("\\\\'", "'", .)),
      dplyr::across(dplyr::where(is.character), ~gsub('\\\\\\"', '"', .))
    )

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"

  # Perform deduping
  res = toxval.source.import.dedup(res, hashing_cols=toxval.config()$hashing_cols)

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
