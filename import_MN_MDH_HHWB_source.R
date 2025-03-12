#--------------------------------------------------------------------------------------
#' @description A generic template for adding data to toxval_source for a new source
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
#' @importFrom dplyr mutate across where
#' @importFrom tidyr replace_na
#--------------------------------------------------------------------------------------
import_MN_MDH_HHWB_source <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "MN MDH HHBW"
  source_table = "source_MN_MDH_HHBW"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2024-12-17")
  dir = paste0(toxval.config()$datapath,"mn_mdh_hhbw/mn_mdh_hhbw_files/")
  file = paste0(dir,"MINN_MDH_HHBW_21June_formatted.xlsx")
  res0 = readxl::read_xlsx("Z:\ToxValDB9\Repo\mn_mdh_hhbw\mn_mdh_hhbw_files")
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
  res = res0

  # Convert 'NR' to '-' in study_year , remove trailing commas from 'toxval_type'.
  res <- res %>%
    dplyr::mutate(
      study_year = case_when(study_year == "NR" ~ "-",
                             TRUE ~ study_year),
      toxval_type = gsub("[[:punct:]]", "", toxval_type))

  # Split 'casrn' list into separate rows
  res <- res %>%
    dplyr::mutate(casrn = str_replace_all(casrn, " or |,|;", ";")) %>%
    separate_longer_delim(casrn, delim = ";")

  # Convert entries in 'species' column to lower-case.
  res <- res %>%
    mutate(!!sym(names(res)[which(grepl("species", names(res), ignore.case = TRUE))]) :=
             tolower(!!sym(names(res)[which(grepl("species", names(res), ignore.case = TRUE))]))
           )

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
#   source_prep_and_load(db=db,
#                        source=source,
#                        table=source_table,
#                        res=res,
#                        do.reset=do.reset,
#                        do.insert=do.insert,
#                        chem.check.halt=chem.check.halt,
#                        hashing_cols=toxval.config()$hashing_cols)
  }
