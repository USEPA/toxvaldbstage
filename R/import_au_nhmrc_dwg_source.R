#--------------------------------------------------------------------------------------
#' @description Adding source AU DWG data to toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_au_nhmrc_dwg_source
#' @return None. SQL statements are run to load data to toxval_source
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
#' @rdname import_au_dwg_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_squish
#' @importFrom dplyr mutate across where
#' @importFrom tidyr replace_na
#--------------------------------------------------------------------------------------
import_au_nhmrc_dwg_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "AU DWG"
  source_table = "source_au_nhmrc_dwg"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2022-09-01")
  dir = paste0(toxval.config()$datapath,"au_nhmrc_dwg/au_nhmrc_dwg_files/")
  file = paste0(dir,"AU_NHMRC_DWG_edit_QC_final.xlsx")
  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  # Add source specific transformations
  res = res0 %>%
    dplyr::mutate(
      qc_status = dplyr::case_when(
        !is.na(`QC result`) ~ "pass",
        TRUE ~ "undetermined"
      )
    )

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  res = res %>%
    dplyr::mutate(
      # Trimming the 'toxval_numeric' column to reduce trailing figures.
      toxval_numeric = case_when(
        toxval_numeric == "c" ~ NA,
        toxval_numeric == "0.015/0.016" ~ "0.015-0.016",
        toxval_numeric == "0.015 -0.055" ~ "0.015-0.055",
        grepl("^[0-9.]+$", toxval_numeric) ~ format(signif(as.numeric(toxval_numeric), 5),
                                                    scientific = TRUE,
                                                    digits = 5,
                                                    trim = TRUE),
        TRUE ~ toxval_numeric
    ) %>%
      # Remove "f" character
      gsub("f", "", .) %>%
      stringr::str_squish(),
    # Remove trailing ")"
    casrn = casrn %>%
      gsub("\\)$", "", .),
    toxval_subtype = tolower(toxval_subtype),
    # Fix exposure route/method/form
    exposure_form = dplyr::case_when(
      exposure_route == "feed" ~ exposure_route,
      exposure_method == "feed" ~ exposure_method,
      TRUE ~ exposure_form
    ),
    exposure_method = dplyr::case_when(
      exposure_route %in% c("gavage", "drinking water", "diet") ~ exposure_route,
      exposure_route == "feed" ~ "diet",
      exposure_method == "feed" ~ "diet",
      exposure_method == "dietary" ~ "diet",
      TRUE ~ exposure_method
    ),
    exposure_route = dplyr::case_when(
      exposure_route %in% c("gavage", "drinking water", "diet", "feed", "occupational exposure", "dietary") ~ "oral",
      exposure_method %in% c("gavage", "drinking water", "diet", "feed", "occupational exposure", "dietary") ~ "oral",
      TRUE ~ exposure_route
    ),
    # Fix sex
    sex = dplyr::case_when(
      sex == "M + F" ~ "male/female",
      sex %in% c("F?", "F") ~ "female",
      sex == "M" ~ "male",
      TRUE ~ sex
    ),
    # Fix study_duration_units
    study_duration_units = dplyr::case_when(
      study_duration_units == "y" ~ "years",
      study_duration_units == "d" ~ "days",
      study_duration_units == "w" ~ "weeks",
      study_duration_units %in% c("m", "month") ~ "months",
      # Curator typo
      study_duration_units == "k" ~ "weeks",
      TRUE ~ study_duration_units
    ),
    # Fix toxval_units
    toxval_units = toxval_units %>%
      gsub("body weight per day|bodyweight per day", "bw/day", .)
    ) %>%
    # Filter out non-chemicals
    dplyr::filter(!name %in% c("pH")) %>%
    # Filter out records that do not have a name and casrn
    dplyr::filter(!(is.na(name) & is.na(casrn))) %>%
    dplyr::filter(!toxval_type %in% c(
      "Data are inadequate to set guideline values for chloroketones in drinking water.",
      "Unknown",
      "lead intake which, based on metabolic studies with infants, does not result in an increase in lead retention",
      "upper range of the estimated adult requiremen",
      "To minimise an undesirable scale build up on surfaces"
    )) %>%
    # Drop records with NA for type or numeric
    tidyr::drop_na(toxval_type, toxval_numeric)

  # Handle toxval_numeric range and relationship
  res = res %>%
    dplyr::mutate(range_flag = dplyr::case_when(
      grepl("[0-9]-[0-9]", toxval_numeric) ~ 1,
      TRUE ~ 0
    ))

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
    dplyr::mutate(toxval_numeric = as.numeric(toxval_numeric))

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
