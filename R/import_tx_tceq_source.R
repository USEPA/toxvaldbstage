#--------------------------------------------------------------------------------------
#' @description Import of TX TCEQ source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_tx_tceq_source
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
import_tx_tceq_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "TX TCEQ"
  source_table = "source_tx_tceq"
  # Date provided by the source or the date the data was extracted
  # src_version_date = as.Date("YYYY-MM-DD")
  dir = paste0(toxval.config()$datapath,"tx_tceq/tx_tceq_files/")
  file_list = list.files(dir, pattern = "xlsx", full.names = TRUE)

  res0 = lapply(file_list, function(f){
    tmp = readxl::read_xlsx(f, sheet="Final") %>%
      dplyr::mutate(data_filename = basename(f),
                    # Add version date by file
                    source_version_date = dplyr::case_when(
                      grepl("amcv", data_filename, ignore.case = TRUE) ~ as.Date("2024-06-11"),
                      grepl("esl", data_filename, ignore.case = TRUE) ~ as.Date("2025-01-20"),
                      TRUE ~ NA
                    ))
  }) %>%
    dplyr::bind_rows() %>%
    # Combine footnote fields
    tidyr::unite(
      col = "footnotes",
      footnotes, Footnotes,
      sep = "; ",
      na.rm = TRUE)

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
      # 7e-6
    } else if(grepl("[eE]", s) && grepl("^[0-9]+\\.?[0-9]* ?[Ee] ?10 ?-?[0-9]+", s)){
      mantissa <- as.numeric(gsub(" ?[eE].*", "", s))
      exponent <- as.numeric(gsub(".*?[eE]", "", s))
      return(as.character(mantissa * 10^exponent))
    }
    return(s)
  }

  # Add source specific transformations
  res = res0 %>%
    dplyr::mutate(
      qc_status = dplyr::case_when(
        !is.na(`QC result`) ~ "pass",
        TRUE ~ "undetermined"
      ),
      year = summary_doc_year,
      casrn = dplyr::case_when(
        grepl("problem|NOCAS", casrn, ignore.case = TRUE) ~ NA,
        TRUE ~ casrn
      ) %>%
        # Remove parentheses
        gsub("\\s*\\([^)]+\\)", "", .),
      name = dplyr::case_when(
        grepl("unspecified", name, ignore.case = TRUE) ~ NA,
        # Remove starting and ending brackets (e.g., [Hexamethylenediamine])
        grepl("^\\[.*\\]$", name) ~ name %>%
          gsub("^\\[|\\]$", "", .),
        TRUE ~ name
      ) %>%
        gsub(":$", "", .),
      exposure_form = dplyr::case_when(
        exposure_method %in% c("feed") ~ exposure_method,
        TRUE ~ exposure_form
      ),
      exposure_method = dplyr::case_when(
        grepl("gavage", exposure_route) ~ "gavage",
        exposure_method %in% c("feed") ~ "diet",
        exposure_route %in% c("breathing zone") ~ exposure_route,
        TRUE ~ exposure_method
      ) %>%
        tolower(),
      exposure_route = dplyr::case_when(
        grepl("gavage", exposure_route) ~ "oral",
        exposure_route %in% c("breathing zone") ~ "inhalation",
        exposure_route %in% c("?") ~ NA,
        TRUE ~ exposure_route
      ) %>%
        tolower(),
      sex = dplyr::case_when(
        sex == "M + F" ~ "male/female",
        sex == "M" ~ "male",
        sex == "F" ~ "female",
        sex %in% c("NA") ~ NA,
        TRUE ~ sex
      ),
      # TODO fix toxval_numeric
      toxval_numeric = dplyr::case_when(
        grepl("Insufficient|Not available|used because", toxval_numeric) ~ NA,
        toxval_numeric %in% c("NR") ~ NA,
        # Remove trailing a and b
        grepl("a$|b$", toxval_numeric) ~ gsub("a$|b$", "", toxval_numeric),
        TRUE ~ toxval_numeric
      ) %>%
        # Remove parenthetic information
        gsub("\\s*\\([^)]+\\)", "", .) %>%
        stringr::str_squish() %>%
        fix.replace.unicode()
      ) %>%
    # Remove empty rows that only have NA values
    .[rowSums(is.na(.)) < ncol(.), ] %>%
    # Remove columns where all values are NA
    .[, colSums(is.na(.)) != nrow(.)] %>%
    # Filter out records that do not have a name and casrn
    dplyr::filter(!(is.na(name) & is.na(casrn))) %>%
    tidyr::drop_na(toxval_type, toxval_numeric)

  res = res %>%
    dplyr::mutate(
      # Flag cases to ignore when removing parenthetic entries
      toxval_units_paren_fix = dplyr::case_when(
        # Ignore cases like (ug Co/m3)-1
        grepl("^\\([^\\)]+\\)-1$", toxval_units) ~ 0,
        # Ignore cases like (mg Co/m3)
        grepl("^\\([^\\)]+\\)$", toxval_units) ~ 0,
        TRUE ~ 1
      ),
      toxval_units = dplyr::case_when(
        is.na(toxval_units) & grepl("ug/m3", toxval_numeric) ~ "ug/m3",
        toxval_units_paren_fix == 1 ~ toxval_units %>%
          # Remove parenthetic entries for flagged cases
          gsub("\\s*\\([^\\)]+\\)", "", .),
        TRUE ~ toxval_units
      ) %>%
        # Fix case of extraneous 7 at start of units
        gsub("^7ug\\/m3", "ug/m3", .) %>%
        # Remove "or" cases
        gsub(' or .*', '', .) %>%
        # Remove trailing letters/footnotes
        gsub("a, c$|a, b$| a$|, c$| a$| b$| \\*|ComettoMuniz et al. 1999$", "", .) %>%
        gsub(", the mean of 1,2,4-TMB and 1,2,3-TMB", "", .) %>%
        gsub("/m3b", "/m3", ., fixed = TRUE) %>%
        gsub("/m3a, b", "/m3", ., fixed = TRUE)
    ) %>%
    dplyr::select(-toxval_units_paren_fix) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(toxval_numeric = toxval_numeric %>%
                    gsub(",", "", .) %>%
                    gsub("ug/m3", "", ., fixed = TRUE) %>%
                    # Replace exponential notation to fix next
                    gsub(" \\* 10", "e", .) %>%
                    gsub(" E", "e", .) %>%
                    parse_scientific()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(toxval_numeric = as.numeric(toxval_numeric)) %>%
    tidyr::drop_na(toxval_type, toxval_numeric)

  # View(res %>% select(toxval_numeric, toxval_units, data_filename) %>% mutate(fix = as.numeric(toxval_numeric)) %>% distinct())
  # View(res %>% select(toxval_units, toxval_units_fix) %>% mutate(diff = toxval_units != toxval_units_fix) %>% filter(diff == TRUE) %>% distinct())

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
