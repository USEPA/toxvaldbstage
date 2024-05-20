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
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{row_number}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{distinct}}
#'  \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_extract}}
#'  \code{\link[tidyr]{reexports}}, \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{drop_na}}, \code{\link[tidyr]{separate}}
#' @rdname import_doe_benchmarks_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate row_number select rename bind_rows across case_when distinct starts_with
#' @importFrom tidyselect matches contains
#' @importFrom stringr str_squish str_extract
#' @importFrom tidyr pivot_longer drop_na separate
#--------------------------------------------------------------------------------------
import_doe_benchmarks_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "DOE Wildlife Benchmarks"
  source_table = "source_doe_benchmarks"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("1996-06-01")
  dir = paste0(toxval.config()$datapath,"doe_benchmarks/doe_benchmarks_files/")
  file = paste0(dir,"DOE_Wildlife_Benchmarks_1996.xlsx")
  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  # Add sequential IDs for linkages handled in load script
  res0_numbered = res0 %>%
    # Remove Form which causes duplicates
    dplyr::select(-Form) %>%
    dplyr::mutate(species_relationship_id = dplyr::row_number())

  # Get data for "test" entries
  res0_test = res0_numbered %>%
    # Filter out endpoint-related columns
    dplyr::select(!tidyselect::matches("Endpoint|Wildlife|Food|Water|Piscivore")) %>%
    # Add experimental_record and species_type columns
    dplyr::mutate(
      experimental_record = "Yes",
      species_type = "test"
    ) %>%
    # Rename species column
    dplyr::rename(species = "Test Species")

  # Get data for "endpoint" entries
  res0_endpoint = res0_numbered %>%
    # Filter out test-related columns
    dplyr::select(!tidyselect::contains("Test")) %>%
    # Add experimental_record and species_type columns
    dplyr::mutate(
      experimental_record = "No",
      species_type = "target"
    ) %>%
    # Rename species column
    dplyr::rename(species = "Endpoint Species")

  res0_endpoint_N = res0_endpoint %>%
    dplyr::select(!tidyselect::contains("LOAEL"))

  res0_endpoint_L = res0_endpoint %>%
    dplyr::select(!tidyselect::contains("NOAEL"))

  # Replace Wildlife NOAEL/LOAEL param fields with type labels if value present
  for(param in names(res0_endpoint_N)[startsWith(names(res0_endpoint_N), "NOAEL")]){
    res0_endpoint_N[[param]][!is.na(res0_endpoint_N[[param]])] = stringr::str_split_i(param, " ", 2)
  }
  for(param in names(res0_endpoint_L)[startsWith(names(res0_endpoint_L), "LOAEL")]){
    res0_endpoint_L[[param]][!is.na(res0_endpoint_L[[param]])] = stringr::str_split_i(param, " ", 2)
  }

  res0_endpoint_N = res0_endpoint_N %>%
    tidyr::unite(col = "toxval_subtype", dplyr::starts_with("NOAEL"),
                 sep = ", ",
                 na.rm = TRUE)
  res0_endpoint_L = res0_endpoint_L %>%
    tidyr::unite(col = "toxval_subtype", dplyr::starts_with("LOAEL"),
                 sep = ", ",
                 na.rm = TRUE)

  res = dplyr::bind_rows(res0_test, res0_endpoint_N, res0_endpoint_L) %>%
    # Add basic columns as necessary
    dplyr::mutate(
      source_url = URL,
      exposure_route = "oral",
      source_url = "https://rais.ornl.gov/documents/tm86r3.pdf",

      # Clean species column
      species = species %>%
        tolower() %>%
        gsub(".+old", "", .) %>%
        gsub("chicks", "chick", .) %>%
        stringr::str_squish()
    ) %>%

    # Set appropriate columns to numeric
    dplyr::mutate(dplyr::across(c(tidyselect::contains("OAEL")), ~as.numeric(.))) %>%

    # Extract source_field and toxval_numeric
    tidyr::pivot_longer(cols = c(tidyselect::contains("OAEL")),
                        names_to = "source_field",
                        values_to = "toxval_numeric") %>%

    # Drop entries with empty toxval_numeric
    tidyr::drop_na(toxval_numeric) %>%

    # Split type/units
    tidyr::separate(col="source_field",
                    into=c("toxval_type", "toxval_units"),
                    sep="\\(",
                    remove = FALSE) %>%

    dplyr::mutate(
      # Clean toxval_units
      toxval_units = gsub("\\)", "", toxval_units) %>%
        gsub("mg/kg/d", "mg/kg-day", ., fixed=TRUE),

      # Get toxval_type
      toxval_type = stringr::str_extract(toxval_type, "[a-zA-Z]OAEL") %>% c(),

      # Get toxval_subtype
      toxval_subtype = dplyr::case_when(
        experimental_record == "Yes" ~ ifelse(toxval_type == "NOAEL",
                                                       "Test Species NOAEL",
                                                       "Test Species LOAEL"),

        TRUE ~ ifelse(toxval_type == "NOAEL",
                      paste0("Target Species Endpoint NOAEL ", toxval_subtype),
                      paste0("Target Species Endpoint LOAEL ", toxval_subtype))
      ),

      # Add (ADJ) note to target_species toxval_type
      toxval_type = dplyr::case_when(
        experimental_record == "No" ~ paste0(toxval_type, " (ADJ)"),
        TRUE ~ toxval_type
      ),

      # Add media column ("food" is default to match previous logic)
      media = dplyr::case_when(
        toxval_subtype == "Water" ~ "water",
        TRUE ~ "food"
      )
    ) %>%

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

  # Perform deduping
  res = toxval.source.import.dedup(res,
                                   dedup_fields=c("species_relationship_id"),
                                   delim=", ")
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
