#--------------------------------------------------------------------------------------
#' @title import_source_who_jecfa_tox_studies
#' @description Import of WHO JECFA Tox Studies data
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALS
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
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{filter}}
#'  \code{\link[tidyr]{separate_rows}}
#'  \code{\link[stringr]{str_trim}} \code{\link[stringr]{str_extract}}
#' @rdname import_source_who_jecfa_tox_studies
#' @export 
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate case_when filter
#' @importFrom tidyr separate_rows
#' @importFrom stringr str_squish str_extract
#' @importFrom tidyselect where
#--------------------------------------------------------------------------------------
import_source_who_jecfa_tox_studies <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "WHO JECFA Tox Studies"
  source_table = "source_who_jecfa_tox_studies"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2022-11-01")
  dir = paste0(toxval.config()$datapath,"who_jecfa_tox_studies/who_jecfa_tox_studies_files/")
  file = paste0(dir,"who_jecfa_toxicological_data_manual.xlsx")
  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  res = res0 %>%
    dplyr::rename(who_jecfa_chemical_id = `Chemical ID`) %>%
    # Handle casrn lists
    tidyr::separate_rows(casrn, sep=";") %>%
    # Copy toxval fields from originals
    dplyr::mutate(
      source_url = chemical_url,
      name = fix.replace.unicode(name) %>% toupper(),
      species = tolower(species),
      casrn = gsub("\\s*\\([^\\)]+\\)","", casrn),
      toxval_units = fix.replace.unicode(toxval_units) %>%
        # Fix unit cases and remove excess whitespace
        gsub("bw/ d", "bw/d", ., fixed=TRUE) %>%
        gsub("mg/kgbw per day", "mg/kg bw per day", ., fixed=TRUE) %>%
        gsub("ug/kg bw /d", "ug/kg bw/d", ., fixed=TRUE) %>%
        gsub(" per ", "/", .) %>%
        gsub("\\/d\\b", "/day", .) %>%
        stringr::str_squish(),

      # Replace non-number toxval_numeric values with blank entry (not NA to preserve char col)
      toxval_numeric = dplyr::case_when(
        grepl("[a-zA-Z]", toxval_numeric) ~ "-",
        TRUE ~ toxval_numeric
      ),

      # Uncomment if splitting toxval_type into toxval_subtype
      # # Extract toxval_subtype from toxval_type
      # toxval_subtype = dplyr::case_when(
      #   # grepl("modal", toxval_type) ~ "modal",
      #   TRUE ~ stringr::str_extract(toxval_type, "\\((.+)\\)", group=1),
      # ),
      # # Remove subtype information from toxval_type
      # toxval_type = toxval_type %>%
      #   gsub("\\(.+", "", .) %>%
      #   # gsub("modal", "", .) %>%
      #   stringr::str_squish(),

      # Handle special NOEL EHMI case for toxval_type
      toxval_type = dplyr::case_when(
        toxval_type == "NOEL (EHMI)" ~ "EHMI (NOEL)",
        # Uncomment if LOEL EHMI swap is desired
        # toxval_type == "LOEL (EHMI)" ~ "EHMI (LOEL)",
        TRUE ~ toxval_type
      ),

      # Translate sex into M/F format
      sex = dplyr::case_when(
        sex == "male" ~ "M",
        sex == "female" ~ "F",
        sex == "male; female" ~ "M/F",
        TRUE ~ sex
      ),

      # Set "none"-type critical_effect to NA
      critical_effect = stringr::str_to_sentence(critical_effect),
      # Remove excess whitespace
      dplyr::across(tidyselect::where(is.character), stringr::str_squish)
    ) %>%

                  # Drop rows without toxval_numeric
    dplyr::filter(toxval_numeric != "-",
                  # Remove entries with blank toxval_type
                  toxval_type != "-") %>%
    dplyr::distinct()

  # Separate toxval_numeric ranges, range_relationship_id created for Load toxval_relationship purposes
  ranged <- res %>%
    dplyr::filter(grepl("-(?![eE])", toxval_numeric, perl=TRUE))
  # Check if any available
  if(nrow(ranged)){
    ranged = ranged %>%
      dplyr::mutate(range_relationship_id = 1:dplyr::n()) %>%
      tidyr::separate_rows(toxval_numeric, sep="-") %>%
      dplyr::group_by(range_relationship_id) %>%
      dplyr::mutate(
        toxval_numeric = as.numeric(toxval_numeric),
        relationship = ifelse(toxval_numeric == min(toxval_numeric), "Lower Range", "Upper Range")
      ) %>%
      dplyr::ungroup()
  } else {
    # Empty dataframe with res cols to bind_rows()
    ranged = res[0,]
  }

  # Join back the range split rows
  res <- res %>%
    dplyr::filter(!grepl("-(?![eE])", toxval_numeric, perl=TRUE)) %>%
    dplyr::mutate(toxval_numeric = as.numeric(toxval_numeric)) %>%
    dplyr::bind_rows(., ranged)

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
