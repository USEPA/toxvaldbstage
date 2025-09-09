#--------------------------------------------------------------------------------------
#' @description Import of NJ DEP source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_nj_dep_source
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
import_nj_dep_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "NJ DEP"
  source_table = "source_nj_dep"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2024-06-11")
  dir = paste0(toxval.config()$datapath,"nj_dep/nj_dep_files/")

  file_list = list.files(dir, pattern = "xlsx")
  # Load all files
  res0 = lapply(file_list, function(f){
    tmp = readxl::read_xlsx(paste0(dir, f)) %>%
      dplyr::mutate(data_filename = f)

    if("study_year" %in% names(tmp)){
      tmp = tmp %>%
        dplyr::mutate(study_year = as.character(study_year))
    }

    if("summary_doc_year" %in% names(tmp)){
      tmp = tmp %>%
        dplyr::mutate(summary_doc_year = as.character(summary_doc_year))
    }

    # Standardize the names
    names(tmp) <- names(tmp) %>%
      stringr::str_squish() %>%
      # Replace whitespace and periods with underscore
      gsub("[[:space:]]|[.]", "_", .) %>%
      tolower()
    return(tmp)
  }) %>%
    dplyr::bind_rows()

  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  # Define helper function for cleaning up scientific notation
  parse_scientific <- function(s) {
    # Handle scientific notation conversion (either 10's or e notation)
    # 7 x 10-6
    if(grepl("[Xx]", s) && grepl("^[0-9]+\\.?[0-9]* ?[Xx] ?10 ?-?[0-9]+", s)){
      mantissa <- as.numeric(gsub(" ?[Xx].*", "", s))
      exponent <- as.numeric(gsub('.*?[Xx] 10', "", s))
      return(as.character(mantissa * 10^exponent))
    # 7x10^6
    } else if(grepl("[Xx]", s) && grepl("^[0-9]+\\.?[0-9]*?[Xx]?10\\^?-?[0-9]+", s)){
      mantissa <- as.numeric(gsub(" ?[Xx].*", "", s))
      exponent <- as.numeric(gsub('.*?[Xx]10\\^', "", s))
      return(as.character(mantissa * 10^exponent))
    # 7 e 10-6
    } else if(grepl("[eE]", s) && grepl("^[0-9]+\\.?[0-9]* ?[Ee] ?10 ?-?[0-9]+", s)){
      mantissa <- as.numeric(gsub(" ?[eE].*", "", s))
      exponent <- as.numeric(gsub(".*?[eE]", "", s))
      return(as.character(mantissa * 10^exponent))
    }
    return(s)
  }

  # Add source specific transformations
  res = res0 %>%
    tidyr::separate_longer_delim(casrn, delim = " & ") %>%
    dplyr::mutate(
      year = summary_doc_year,
      casrn = dplyr::case_when(
        grepl("No CASRN|group|NOCAS|DSSTox|DTXSID|See|used|N/A", casrn, ignore.case = TRUE) ~ NA,
        TRUE ~ casrn
      ) %>%
        # Remove parentheses
        gsub("\\s*\\([^)]+\\)", "", .) %>%
        gsub("CASRN: |)$", "", .),
      exposure_form = dplyr::case_when(
        exposure_route %in% c("feed") ~ exposure_route,
        TRUE ~ exposure_form
      ) %>%
        # Replace with "-"
        gsub("^6 hours per day$", "-", .),
      exposure_method = dplyr::case_when(
        grepl("gavage", exposure_route) ~ "gavage",
        exposure_route %in% c("drinking water") ~ exposure_route,
        grepl("feed|dietary", exposure_route) ~ "diet",
        TRUE ~ exposure_method
      ),
      exposure_route = dplyr::case_when(
        grepl("gavage|dietary|feed|drinking water", exposure_route) ~ "oral",
        TRUE ~ exposure_route
      ),
      # generation from lifestage
      generation = dplyr::case_when(
        is.na(generation) & grepl("F1", lifestage) ~ "F1",
        TRUE ~ generation
      ),
      # Fix sex
      sex = dplyr::case_when(
        sex %in% c("[F]", "F") ~ "female",
        sex == "M + F" ~ "male/female",
        TRUE ~ sex
      ) %>%
        gsub("(predominantly or entirely)", "", ., fixed = TRUE) %>%
        stringr::str_squish() %>%
        gsub("^M$", "male", .),
      # Fix study_duration_units
      study_duration_units = dplyr::case_when(
        study_duration_units == "y" ~ "years",
        study_duration_units == "w" ~ "weeks",
        study_duration_units == "d" ~ "days",
        study_duration_units == "m" ~ "months",
        TRUE ~ study_duration_units
      ),
      # Fix toxval_numeric
      toxval_numeric = dplyr::case_when(
        toxval_numeric %in% c("None Noticeable", "unable to find data", "NA", "NR") ~ NA,
        toxval_numeric == "10^6" ~ as.character(10^6),
        TRUE ~ toxval_numeric
      ) %>%
        as.numeric()
      ) %>%
    # Remove empty rows that only have NA values
    .[rowSums(is.na(.)) < ncol(.), ]

  res = res %>%
    dplyr::rowwise() %>%
    dplyr::mutate(toxval_numeric = parse_scientific(toxval_numeric)) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na(toxval_type, toxval_numeric) %>%
    # Remove non-chemical names
    dplyr::filter(!grepl("bacteria|\\bodor\\b|pH|Color|Dissolved Solids|Taste", name, ignore.case = TRUE)) %>%
    # Filter out records that do not have a name and casrn
    dplyr::filter(!(is.na(name) & is.na(casrn)))

  # View(res %>% select(toxval_numeric) %>% mutate(fix = as.numeric(toxval_numeric)) %>% distinct())

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
