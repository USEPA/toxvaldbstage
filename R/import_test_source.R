#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#' @title import_test_source
#' @description Load TEST Source data into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#' @param do.reset If TRUE, delete data from the database for this source before inserting new data
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @return None; data is loaded into toxval_source
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[utils]{read.table}}
#'  \code{\link[dplyr]{select}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{join_by}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{case_when}}
#'  \code{\link[stringr]{str_trim}}
#'  \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{drop_na}}
#' @rdname import_test_source
#' @export 
#' @importFrom readxl read_xlsx
#' @importFrom utils read.csv
#' @importFrom dplyr select left_join join_by mutate case_when
#' @importFrom stringr str_squish
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom readr read_csv cols
import_test_source <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "TEST"
  source_table = "source_test"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2019-11-26")
  dir = paste0(toxval.config()$datapath,"test/test_files/")
  study_info_file = paste0(dir, "TEST data.xlsx")
  chemical_info_file = paste0(dir, "test_chemicals_invitrodb.csv")

  res_study_info = readxl::read_xlsx(study_info_file)
  res_chemical_info = readr::read_csv(chemical_info_file, col_types = readr::cols())
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  # Remove unlabeled column in study_info DF
  res_study_info = res_study_info %>% dplyr::select(-...2)

  # Combine input files
  res0 = res_study_info %>%
    dplyr::left_join(res_chemical_info,
                     by = c("CAS" = "casn"))

  # Perform necessary transformations
  res = res0 %>%
    dplyr::mutate(
      # Rename/add basic columns
      name = fix.replace.unicode(chnm),
      casrn = CAS,
      toxval_units = LD50Units,
      study_type = "acute",
      species = "rat",
      source_url = "https://www.epa.gov/chemical-research/toxicity-estimation-software-tool-test",
      exposure_route = "oral",

      # Get reference_url, long_ref, and critical_effect, and remove "Unknown" values
      reference_url = dplyr::case_when(
        ReferenceURL == "Unknown" ~ as.character(NA),
        TRUE ~ ReferenceURL
      ),
      long_ref = dplyr::case_when(
        Reference == "Unknown" ~ as.character(NA),
        TRUE ~ Reference
      ),
      critical_effect = dplyr::case_when(
        Effect == "Unknown" ~ as.character(NA),
        TRUE ~ Effect
      ) %>%
        # Clean critical_effect
        gsub("<br><br>", "; ", .) %>%
        gsub("BEHAVIORAL: MUSCLE CONTRACTION OR SPASTICITY\\)",
             "BEHAVIORAL: MUSCLE CONTRACTION OR SPASTICITY", .) %>%
        stringr::str_squish()
    ) %>%

    # Extract toxval_numeric and toxval_type
    tidyr::pivot_longer(
      cols = c("LD50"),
      names_to = "toxval_type",
      values_to = "toxval_numeric"
    ) %>%

    dplyr::mutate(
      # Get toxval_numeric_qualifier from toxval_numeric
      toxval_numeric_qualifier = dplyr::case_when(
        grepl(">", toxval_numeric) ~ ">",
        TRUE ~ "="
      ),

      # Clean toxval_numeric
      toxval_numeric = toxval_numeric %>%
        gsub(">|un", "", .) %>%
        as.numeric()
    ) %>%

    # Drop entries without toxval_numeric
    tidyr::drop_na(toxval_numeric) %>%
    # Remove duplicates
    dplyr::distinct()

  # Hardcode fix for units
  res$toxval_units[res$toxval_units == "its/kg"] = "units/kg"

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"

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




