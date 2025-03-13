#--------------------------------------------------------------------------------------
#' @description Import MN MDH HHBW 2024-12-17 source into toxval_source.
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_mn_mdh_hhwb_source
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
#' @rdname import_mn_mdh_hhwb_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_squish
#' @importFrom dplyr mutate across where
#' @importFrom tidyr replace_na
#--------------------------------------------------------------------------------------
import_mn_mdh_hhwb_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "MN MDH HHBW"
  source_table = "source_mn_mdh_hhbw"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2024-12-17")
  dir = paste0(toxval.config()$datapath,"mn_mdh_hhbw/mn_mdh_hhbw_files/")
  file = paste0(dir,"MINN_MDH_HHBW_21June_formatted.xlsx")
  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  # Add source specific transformations

  res <- res0 %>%
    dplyr::mutate(
      # Substitute 'NR' to '-' in study_year
      study_year = dplyr::case_when(
        study_year %in% c("NR") ~ "-",
        TRUE ~ study_year),
      # Remove trailing commas from toxval_type
      toxval_type = gsub(",$", "", toxval_type),
      # Set species to lowercase
      species = tolower(species),
      # Set study_duration_class to lowercase
      study_duration_class = tolower(study_duration_class),
      # Remove trailing semi-colons from casrn
      casrn = casrn %>%
        gsub(";$", "", .),
      # Remove extraneous numeric
      study_duration_class = study_duration_class %>%
        gsub("neurotoxicity study1", "neurotoxicity study", .),
      strain = strain %>%
        gsub("SpragueDawley", "Sprague-Dawley", .),
      sex = dplyr::case_when(
        sex %in% c("F") ~ "female",
        sex %in% c("M") ~ "male",
        sex %in% c("M/F") ~ "male/female",
        TRUE ~ sex
      ),
      experimental_record = experimental_record %>%
        gsub("NR", "undetermined", .)
      ) %>%
    # Split 'casrn' list into separate rows
    tidyr::separate_longer_delim(casrn, delim = stringr::regex(" or |,|;"))

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
