#--------------------------------------------------------------------------------------
#' @description Load doe_benchmarks Source into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile The input file ./doe_benchmarks/doe_benchmarks_files/DOE_Wildlife_Benchmarks_1996.xlsx
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
#'  \code{\link[openxlsx]{read.xlsx}}
#' @rdname import_doe_benchmarks_source
#' @export
#' @importFrom openxlsx read.xlsx
#--------------------------------------------------------------------------------------
import_doe_benchmarks_source <- function(db,
                                         infile="DOE_Wildlife_Benchmarks_1996.xlsx",
                                         chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE){
  printCurrentFunction(db)
  source = "DOE Wildlife Benchmarks"
  source_table = "source_doe_benchmarks"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("1996-06-01")
  infile = paste0(toxval.config()$datapath,"doe_benchmarks/doe_benchmarks_files/",infile)
  #####################################################################
  cat("Build original_doe_benchmarks_table \n")
  #####################################################################
  res0 = readxl::read_xlsx(infile)

  res = res0 %>%
    dplyr::rename(source_url = URL,
                  species_original = `Test Species`) %>%
    dplyr::mutate(across(starts_with("Test"), ~suppressWarnings(as.numeric(.)))) %>%
    dplyr::select(-tidyr::starts_with(c("Wildlife","NOAEL","LOAEL", "Endpoint"))) %>%
    tidyr::pivot_longer(cols=starts_with("Test"),
                        names_to = "toxval_type",
                        values_to = "toxval_numeric") %>%
    # Split type and units
    tidyr::separate(col="toxval_type", into=c("toxval_type", "toxval_units"), sep="\\(") %>%
    dplyr::mutate(toxval_units = gsub("\\)", "", toxval_units) %>%
                    gsub("mg/kg/d", "mg/kg-day", ., fixed=TRUE),
                  toxval_type = gsub("Test Species", "", toxval_type) %>%
                    stringr::str_squish(),
                  media = "food",
                  exposure_route = "oral",
                  toxval_subtype = dplyr::case_when(toxval_type =="NOAEL" ~ "test_species_noael",
                                                    toxval_type =="LOAEL" ~ "test_species_loael")) %>%
    distinct()

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

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
                       chem.check.halt=chem.check.halt)
}
