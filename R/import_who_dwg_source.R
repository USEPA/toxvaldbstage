#--------------------------------------------------------------------------------------
#' @description Import of WHO DWG source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_who_dwg_source
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
import_who_dwg_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "WHO DWG"
  source_table = "source_who_dwg"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2024-06-11")
  dir = paste0(toxval.config()$datapath,"who_dwg/who_dwg_files/")
  file = paste0(dir, "WHO_DWG_July2024_All_formatted.xlsx")
  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  # Add source specific transformations
  res = res0 %>%
    dplyr::rename(source_url = subsource_url) %>%
    # Handle exposure fields
    dplyr::mutate(
      exposure_form = dplyr::case_when(
        exposure_route %in% c("feed") ~ exposure_route,
        TRUE ~ exposure_form
      ),
      exposure_method = dplyr::case_when(
        grepl("drinking", exposure_route, fixed=TRUE) ~ "drinking water",
        exposure_route %in% c("diet", "ingestion", "drinking water") ~ exposure_route,
        exposure_route == "feed" ~ "diet",
        grepl("gavage", exposure_route) ~ "gavage",
        TRUE ~ exposure_method
      ),
      exposure_route = dplyr::case_when(
        grepl("drinking", exposure_route, fixed=TRUE) ~ "oral",
        # Remove critical_effect
        exposure_route == "increased absolute and relative spleen weight" ~ "-",
        grepl("gavage", exposure_route) ~ "oral",
        exposure_route %in% c("diet", "feed", "ingestion", "drinking water") ~ "oral",
        TRUE ~ exposure_route
      ),
      sex = dplyr::case_when(
        sex == "M + F" ~ "male/female",
        sex == "F" ~ "female",
        sex == "M" ~ "male",
        TRUE ~ sex
      ),
      species = species %>%
        gsub("volunteers", "", .) %>%
        stringr::str_squish(),
      # Fix numeric, qualifier, and units
      toxval_numeric = toxval_numeric %>%
        fix.replace.unicode(),
      toxval_units = dplyr::case_when(
        is.na(toxval_units) & grepl("mg/kg body weight", toxval_numeric) ~ "mg/kg bw",
        TRUE ~ toxval_units
      ) %>%
        gsub("body weight", "bw", .),
      # Set 0- range values as <= and report max
      toxval_numeric_qualifier = dplyr::case_when(
        grepl("^0-", toxval_numeric) ~ "<=",
        grepl("approximately", toxval_numeric) ~ "~",
        TRUE ~ toxval_numeric_qualifier
      ),
      toxval_numeric = dplyr::case_when(
        # Set 0- range values as <= and report max
        grepl("^0-", toxval_numeric) ~ toxval_numeric %>%
          gsub("^0-", "", .),
        grepl("NR", toxval_numeric) ~ NA,
        TRUE ~ toxval_numeric
      ) %>%
        gsub("mg/kg body weight", "", .) %>%
        gsub("approximately", "", .) %>%
        stringr::str_squish(),
      # Fix url
      source_url = source_url %>%
        gsub("\\.$", "", .) %>%
        gsub("/ teams", "/teams", ., fixed = TRUE) %>%
        gsub("/ water", "/water", ., fixed = TRUE) %>%
        gsub("andhealth", "and-health", .)
    ) %>%
    # Remove empty rows that only have NA values
    .[rowSums(is.na(.)) < ncol(.), ] %>%
    dplyr::mutate(year = summary_doc_year) %>%
    # Filter out records that do not have a name and casrn
    dplyr::filter(!(is.na(name) & is.na(casrn)),
                  !name %in% c("Hardness", "pH")) %>%
    tidyr::drop_na(toxval_type, toxval_numeric)

  # View(res %>% select(toxval_numeric_qualifier, toxval_numeric, toxval_units) %>% mutate(fix = as.numeric(toxval_numeric)) %>% distinct())

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
