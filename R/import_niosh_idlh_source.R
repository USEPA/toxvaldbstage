#--------------------------------------------------------------------------------------
#' @description Import of NIOSH IDLH source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_niosh_idlh_source
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
import_niosh_idlh_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "NIOSH IDLH"
  source_table = "source_niosh_idlh"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2025-02-04")
  dir = paste0(toxval.config()$datapath,"niosh_idlh/niosh_idlh_files/")
  file = paste0(dir, "NIOSH IDLH Derivation 2025_QC_final.xlsx")
  # Skip first few rows that were manually curated as metadata for the file
  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  # Add source specific transformations
  res = res0 %>%
    dplyr::mutate(
      qc_status = dplyr::case_when(
        is.na(qc_status) ~ "undetermined",
        TRUE ~ qc_status
      ),
      year = summary_doc_year,
      casrn_fix = dplyr::case_when(
        grepl("n\\/a|NOCAS|unclear|unreliable|forms|substances|cadmium|available|no known",
              casrn, ignore.case = TRUE) ~ NA,
        casrn == "-" ~ NA,
        TRUE ~ casrn
      ) %>%
        # Remove parentheses
        gsub("\\s*\\([^)]+\\)", "", .),
      name = dplyr::case_when(
        grepl("-|unclear|form not|form unknown|not specified|unspecified|from DSSTox", name) ~ NA,
        TRUE ~ name
      ),
      exposure_method = dplyr::case_when(
        # exposure_method %in% c("exposure", "controlled", "intentional exposure) ~ NA,
        TRUE ~ exposure_method
      ),
      exposure_route = dplyr::case_when(
        exposure_route == "i.v." ~ "iv",
        grepl("exposure", exposure_route) ~ NA,
        grepl("skin", exposure_route) ~ "dermal",
        TRUE ~ exposure_route
      ),
      sex = dplyr::case_when(
        sex == "M + F" ~ "M/F",
        TRUE ~ sex
      ),
      species = tolower(species),
      study_duration_qualifier = dplyr::case_when(
        study_duration_qualifier %in% c("NA") ~ NA,
        TRUE ~ study_duration_qualifier
      ),
      toxval_numeric = dplyr::case_when(
        grepl("times|of the|constituents", toxval_numeric) ~ NA,
        grepl(" to | - ", toxval_numeric) ~ gsub(" to ", "-", toxval_numeric),
        TRUE ~ toxval_numeric
      ) %>%
        # Remove comma
        gsub(",", "", .)
    ) %>%
    # Remove empty rows that only have NA values
    .[rowSums(is.na(.)) < ncol(.), ] %>%
    # Filter out records that do not have a name and casrn
    dplyr::filter(!(is.na(name) & is.na(casrn))) %>%
    tidyr::drop_na(toxval_type, toxval_numeric)

  res = res %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      range_flag = dplyr::case_when(
        is.na(toxval_numeric) ~ 0,
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
    dplyr::mutate(toxval_numeric = as.numeric(stringr::str_squish(toxval_numeric))) %>%
    tidyr::drop_na(toxval_type, toxval_numeric, toxval_units)

  # View(res %>% dplyr::select(name, toxval_numeric, toxval_numeric_fix, toxval_units) %>% dplyr::distinct() %>% dplyr::mutate(compare = toxval_numeric != toxval_numeric_fix))

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
