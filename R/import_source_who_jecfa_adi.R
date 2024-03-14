#--------------------------------------------------------------------------------------
#' @description Import of WHO JECFA ADI data
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title FUNCTION_TITLE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[stringr]{str_trim}}
#' @rdname import_generic_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_squish
#--------------------------------------------------------------------------------------
import_source_who_jecfa_adi <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "WHO JECFA ADI"
  source_table = "source_who_jecfa_adi"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2022-11-01")
  dir = paste0(toxval.config()$datapath,"who_jecfa_adi/who_jecfa_adi_files/")
  file = paste0(dir,"source_who_jecfa_raw_data_20231101.xlsx")
  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  #
  # the final file should have column names that include "name" and "casrn"
  # additionally, the names in res need to match names in the source
  # database table. You do not need to add any of the generic columns
  # described in the SOP - they will get added in source_prep_and_load
  #

  res = res0 %>%
    # Copy toxval fields from originals
    dplyr::mutate(name = `Webpage Name` %>%
                    fix.replace.unicode() %>%
                    gsub("\\bASC\\b", "Acidifed Sodium Chlorate", .),
                  casrn = `CAS number` %>%
                    # Remove parenthetics
                    gsub("\\s*\\([^\\)]+\\)","", .) %>%
                    # Replace unicode
                    fix.replace.unicode() %>%
                    # Replace separators with |::|
                    gsub(";|,|and|\\/", " |::| ", .) %>%
                    stringr::str_squish(),
                  year = `Evaluation year`) %>%
    # # Separate casrn lists
    # tidyr::separate_rows(casrn, sep=";") %>%
    # tidyr::separate_rows(casrn, sep=",") %>%
    # tidyr::separate_rows(casrn, sep=" and ") %>%
    # tidyr::separate_rows(casrn, sep="/") %>%
    # dplyr::mutate(casrn = casrn %>%
    #                 stringr::str_squish()) %>%
    # Rename toxval identifier field
    dplyr::rename(who_jecfa_chemical_id = `Chemical ID`)

  # name unicode check
  # tmp = res %>% mutate(name_check = name %>% stringi::stri_escape_unicode())
  # tmp %>% filter(name != name_check) %>% select(name, name_check) %>% distinct() %>% View()
  # casrn unicode check
  # tmp = res %>% mutate(casrn_check = casrn %>% stringi::stri_escape_unicode())
  # tmp %>% filter(casrn != casrn_check) %>% select(casrn, casrn_check) %>% distinct() %>% View()
  # res %>% select(casrn) %>% unique() %>% View()

  # res$ADI[grepl(";", res$ADI)] %>% unique()
  # Case of ADI ending with ";", fix before separate
  # res$ADI[grepl(";$", res$ADI)]
  res$ADI[grepl(";$", res$ADI)] = res$ADI[grepl(";$", res$ADI)] %>%
    gsub(";", "", .)
  # Case of ; in reference parentheses - hardcode fix for the single case
  res$ADI[grepl("(1973; SORBIC ACID)", res$ADI)] = "(1973, SORBIC ACID)"

  res = res %>%
    # Derive toxval_numeric, toxval_units, toxval_numeric_qualifier from ADI column
    tidyr::separate_rows(ADI, sep = ';') %>%
    dplyr::mutate(
      cleaned_adi = ADI %>%
        gsub("\\(.*?\\)","",.) %>%
        gsub("–", "-", .) %>%
        gsub("\\s*-\\s*", "-", .),
      toxval_numeric = cleaned_adi %>%
        stringr::str_extract(., "\\d+\\.?\\d*-?\\d*\\.?\\d*"),
      toxval_units = cleaned_adi %>%
        stringr::str_replace(., paste0(".*", toxval_numeric),"") %>%
        stringr::str_squish() %>%
        fix.replace.unicode(),
      toxval_units_comments = Comments %>%
        # Fix unicode symbols
        fix.replace.unicode() %>%
        stringr::str_replace(., paste0(".*", toxval_numeric),"") %>%
        stringr::str_squish(),
      # toxval_units = if_else(is.na(toxval_numeric), NA_character_, toxval_units),
      toxval_numeric_qualifier = dplyr::case_when(
        grepl(">", cleaned_adi) ~ ">",
        grepl("<", cleaned_adi) ~ "<",
        grepl("=", cleaned_adi) ~ "=",
        grepl("~", cleaned_adi) ~ "~",
        TRUE ~ "-"
      ),
      toxval_type = "ADI",
      species = "human",
      exposure_route = "oral",
      exposure_method = "diet"
    ) %>%
    # Remove NA toxval_numeric values (we don't use them)
    dplyr::filter(!is.na(toxval_numeric))

  # Set units as NA if numeric is NA
  res$toxval_units[is.na(res$toxval_numeric)] = NA

  # Handle case of blank toxval_units in Comment column
  res$toxval_units[res$toxval_units == "" & !is.na(res$toxval_units)] = res$toxval_units_comments[res$toxval_units == "" & !is.na(res$toxval_units)]

  # Remove cleaning columns
  res = res %>%
    dplyr::select(-cleaned_adi, toxval_units_comments)

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




