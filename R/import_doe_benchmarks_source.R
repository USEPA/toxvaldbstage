#--------------------------------------------------------------------------------------
#' @title import_doe_benchmarks_source
#' @description Load DOE Wildlife Benchmarks data into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
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
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{distinct}}
#'  \code{\link[tidyr]{reexports}}, \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{drop_na}}
#'  \code{\link[stringr]{str_trim}}
#' @rdname import_doe_benchmarks_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate across case_when distinct
#' @importFrom tidyr starts_with pivot_longer separate drop_na
#' @importFrom stringr str_squish
#--------------------------------------------------------------------------------------
import_doe_benchmarks_source <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "DOE Wildlife Benchmarks"
  source_table = "source_doe_benchmarks"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("1996-06-01")
  dir = paste0(toxval.config()$datapath,"doe_benchmarks/benchmarks_files/")
  file = paste0(dir,"DOE_Wildlife_Benchmarks_1996.xlsx")
  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  res = res0 %>%
    # Add basic columns as necessary
    dplyr::mutate(
      source_url = URL,
      species = `Test Species`,
      media = "food",
      exposure_route = "oral",
      source_url = "https://rais.ornl.gov/documents/tm86r3.pdf"
    ) %>%

    # Set appropriate columns to numeric
    dplyr::mutate(dplyr::across(tidyr::starts_with("Test"), ~suppressWarnings(as.numeric(.)))) %>%

    # Removing this line from the original to align with "keep all data" philosophy
    # Uncomment if column removal is still desired
    # dplyr::select(-tidyr::starts_with(c("Wildlife","NOAEL","LOAEL", "Endpoint"))) %>%

    # Extract toxval_type and toxval_numeric
    tidyr::pivot_longer(cols=tidyr::starts_with("Test"),
                        names_to = "toxval_type_units",
                        values_to = "toxval_numeric") %>%

    # Split type/units
    tidyr::separate(col="toxval_type_units", into=c("toxval_type", "toxval_units"), sep="\\(") %>%

    dplyr::mutate(
      # Clean toxval_units
      toxval_units = gsub("\\)", "", toxval_units) %>%
        gsub("mg/kg/d", "mg/kg-day", ., fixed=TRUE),

      # Clean toxval_type
      toxval_type = gsub("Test Species", "", toxval_type) %>%
        stringr::str_squish(),

      # Get toxval_subtype
      toxval_subtype = dplyr::case_when(toxval_type =="NOAEL" ~ "test_species_noael",
                                        toxval_type =="LOAEL" ~ "test_species_loael")) %>%

    # Drop rows w/o numeric value
    tidyr::drop_na("toxval_numeric") %>%

    # Remove duplicate rows
    dplyr::distinct()

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




