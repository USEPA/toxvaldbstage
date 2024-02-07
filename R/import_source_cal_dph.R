#--------------------------------------------------------------------------------------
#' @title import_source_cal_dph
#' @description Load California DPH into toxval_source
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
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{select}}
#'  \code{\link[purrr]{reexports}}
#'  \code{\link[stringr]{str_split}}, \code{\link[stringr]{str_match}}, \code{\link[stringr]{str_trim}}
#'  \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{drop_na}}
#' @rdname import_source_cal_dph
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate across case_when select any_of
#' @importFrom stringr str_match str_squish
#' @importFrom tidyr pivot_longer drop_na
#--------------------------------------------------------------------------------------
import_source_cal_dph <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "California DPH"
  source_table = "source_cal_dph"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2023-08-16")
  dir = paste0(toxval.config()$datapath,"cal_dph/cal_dph_files/")
  file = paste0(dir,"cal_dph_2023_update.xlsx")
  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  res = res0 %>%
    # Replace NAs with actual NA value
    dplyr::mutate(dplyr::across(where(is.character),
                                .fns = ~replace(., . %in% c("--", "n/a", "withdrawn Nov. 2001", "none"),
                                                NA))) %>%

    # Replace "zero" with actual 0
    dplyr::mutate(dplyr::across(where(is.character),
                                .fns = ~replace(., . == "zero", 0))) %>%

    # Replace scientific notation with numbers understandable to R
    dplyr::mutate(dplyr::across(where(is.character),
                                .fns = ~gsub("x10\\-", "e-", .))) %>%

    dplyr::mutate(
      # Add basic columns
      source = "California DPH",
      subsource = "California DPH",
      source_url = "https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/Chemicalcontaminants.html",
      risk_assessment_class = "chronic",
      study_type = "chronic",
      exposure_route = "oral",
      detection_limit = `State DLR` %>%
        # stringr::str_split_i(., " ", 1) %>%
        gsub('[",*]', "", .) %>%
        gsub('MFL', "", .) %>%
        as.numeric()
    ) %>%

    # Get name and full chemical class
    tidyr::pivot_longer(
      cols = c("State Regulated Inorganic Chemical Contaminant",
               "State Regulated Copper and Lead Contaminant",
               "State Regulated Radionuclides Contaminant",
               "State Regulated Volatile Organic Contaminants",
               "State Regulated Non-Volatile Synthetic Organic Contaminants",
               "State Regulated Disinfection Byproducts Contaminants"),
      names_to = "chemical_class_full",
      values_to = "name"
    ) %>%
    # Not all classes have chemicals to list, so remove
    tidyr::drop_na("name") %>%

    # Get toxval_type and toxval_numeric
    tidyr::pivot_longer(
      cols = c("State MCL", "State PHG", "Federal MCL"),
      names_to = "toxval_type",
      values_to = "toxval_numeric"
    ) %>%
    dplyr::mutate(
      # Clean up toxval_numeric
      toxval_numeric = toxval_numeric %>%
        fix.replace.unicode() %>%
        gsub('[",]', "", .) %>%
        gsub(" as NO3 \\(=10 as N\\)| as N", "", .),
      # Get year
      year = dplyr::case_when(
        toxval_type != "State PHG" ~ "2023",
        TRUE ~ `State Date of PHG`
      )
    ) %>%
    # Split units where needed
    tidyr::separate(
      toxval_numeric, into=c("toxval_numeric", "toxval_units"), sep = " ", fill="right"
    ) %>%

    # Separate year where needed to get revision years
    tidyr::separate(
      year, into = c("extra_year", "year"), sep = "\\(", fill="left"
    ) %>%

    dplyr::select(-extra_year) %>%

    # Conduct final transformations
    dplyr::mutate(
      # Remove notes from name column
      name = name %>%
        gsub('"', "", .) %>%
        gsub(" \\(MFL = million fibers per liter; for fibers >10 microns long\\)", "", .) %>%
        gsub(" \\- OEHHA withdrew the 0\\.0025\\-mg/L PHG", "", .) %>%
        gsub(" \\- 0\\.01\\- mg/L MCL \\& 0\\.001\\- mg/L DLR repealed September 2017", "", .) %>%
        gsub(" \\- OEHHA concluded in 2003 that a PHG was not practical", "", .),

      # Extract specific chemical class
      chemical_class = stringr::str_match(chemical_class_full, "State Regulated (.+) Contaminants?")[,2],

      # Recode toxval_type
      toxval_type = dplyr::case_when(
        toxval_type == "State MCL" ~ "California MCL",
        toxval_type == "State PHG" ~ "OEHHA PHG",
        toxval_type == "Federal MCL" ~ "MCL Federal",
        TRUE ~ toxval_type
      ),

      # Check for specified units, otherwise use mg/L
      toxval_units = dplyr::case_when(
        is.na(toxval_units) ~ "mg/L",
        chemical_class == "Radionuclides" & is.na(toxval_units) ~ "pCi/L",
        TRUE ~ toxval_units
      ),

      # Set toxval_numeric as numeric
      toxval_numeric = toxval_numeric %>%
        as.numeric(),
      # Finish formatting year
      year = year %>%
        gsub("rev|\\)|\\*", "", .) %>%
        as.numeric()
    ) %>%
    tidyr::drop_na("toxval_numeric") %>%

    # Remove Federal MCLG field
    dplyr::select(!dplyr::any_of("Federal MCLG"))

  # Hardcode special case for Asbestos
  res$toxval_units[res$name == "Asbestos"] = "million fibers/L"

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




