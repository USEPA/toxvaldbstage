#--------------------------------------------------------------------------------------
#' @description Import of EPA HWIR source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_epa_hwir_source
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
#'  \code{\link[stringr]{str_trim}}
#' @rdname import_generic_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_squish
#' @importFrom dplyr mutate across where
#' @importFrom tidyr replace_na
#--------------------------------------------------------------------------------------
import_epa_hwir_source <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "EPA HWIR"
  source_table = "source_epa_hwir"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2025-01-10")
  dir = paste0(toxval.config()$datapath, "epa_hwir/epa_hwir_files/")
  file = paste0(dir, "EPA_HWIR_Inhalation_ToxVal_QCcomplete.xlsx")
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

  # Add source specific transformations
  res = res0  %>%
    dplyr::rename(toxval_type = `toxval_type (called Critical Dose in this collection)`) %>%
    # Combine unnamed columns to notes
    tidyr::unite(col = `Footnotes and Notes`,
                 dplyr::contains("..."), `Footnotes and Notes`,
                 sep = "; ",
                 na.rm = TRUE) %>%
    # Split off units from toxval_numeric
    tidyr::separate_wider_delim(
      cols = toxval_numeric,
      names = c("toxval_numeric", "toxval_units_ext"),
      delim = " ",
      too_few = "align_start"
    ) %>%
    # Combine toxval_units columns
    tidyr::unite(
      col = "toxval_units",
      toxval_units, toxval_units_ext,
      sep = "; ",
      na.rm = TRUE
    ) %>%
    dplyr::mutate(
      `Footnotes and Notes` = `Footnotes and Notes` %>%
        dplyr::na_if(""),
      toxval_numeric = as.numeric(toxval_numeric),
      # Remove "volunteer" from lifestage
      population = dplyr::case_when(
        grepl("volunteer", lifestage) ~ lifestage,
        TRUE ~ NA
      ),
      lifestage = dplyr::case_when(
        lifestage == population ~ NA,
        TRUE ~ lifestage
      ),
      year = study_year,
      # All records completed QC and passed
      qc_status = "pass"
    )

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  res = res %>%
    # Remove records with NA in core columns
    tidyr::drop_na(toxval_numeric, toxval_units, toxval_type) %>%
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
