#--------------------------------------------------------------------------------------
#' @description Import of EPA OW OPP-ALB source into toxval_source
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
#'  \code{\link[dplyr]{rename}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}}
#'  \code{\link[tidyr]{reexports}}, \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{separate}}
#'  \code{\link[stringr]{str_trim}}
#' @rdname import_source_epa_ow_opp_alb
#' @importFrom readxl read_xlsx
#' @importFrom dplyr rename mutate across
#' @importFrom tidyr all_of pivot_longer separate
#' @importFrom stringr str_squish
#' @importFrom tidyselect where
#--------------------------------------------------------------------------------------
import_source_epa_ow_opp_alb <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "EPA OW OPP-ALB"
  source_table = "source_epa_ow_opp_alb"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2023-08-31")
  dir = paste0(toxval.config()$datapath,"epa_ow_opp_alb/epa_ow_opp_alb_files/")
  file = paste0(dir,"source_epa_ow_opp_alb_20230831.xlsx")
  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  # Collect vector of toxval columns to pivot out later
  toxval_cols <- c("Freshwater Vertebrate_Acute", "Freshwater Vertebrate_Chronic",
                   "Freshwater Invertebrates_Acute", "Freshwater Invertebrates_Chronic",
                   "Nonvascular Plants", "Vascular Plants",
                   "Office of Water  Aquatic Life Criteria_Acute",
                   "Office of Water  Aquatic Life Criteria_Chronic")
  res <- res0 %>%
    # Pivot out toxval_numeric
    dplyr::mutate(dplyr::across(tidyselect::all_of(toxval_cols), as.character)) %>%
    tidyr::pivot_longer(tidyr::all_of(toxval_cols), names_to = "species",
                        values_to = "toxval_numeric"
    ) %>%
    # Clean up variables
    dplyr::mutate(
      # Rename columns
      name = Pesticide,
      casrn = `CAS number`,
      # add toxval_units and media column
      toxval_units = "ug/L",
      media = "freshwater",
      # Replace "NR" in casrn with "-"
      casrn = gsub("^NR$", "-", casrn),
      # Handle unicode symbols
      dplyr::across(tidyselect::where(is.character), fix.replace.unicode),
      # Extract toxval_numeric_qualifier
      toxval_numeric_qualifer = stringr::str_extract(toxval_numeric, "([<>=~]+)", group=1),
      # Convert toxval_numeric to numeric type
      toxval_numeric = toxval_numeric %>%
        gsub("[<>=~]+", "", .) %>%
        as.numeric(),

      source_url = url
      ) %>%
    # Separate "species" into "species" and "study_type" by "_"
    tidyr::separate(species, c("species", "study_type"),
                    sep = "_", extra = "merge", fill = "right", remove = FALSE
    ) %>%
    # Normalize species
    dplyr::mutate(species = tolower(species) %>%
                    stringr::str_squish()) %>%
    # Drop NA toxval_numeric
    tidyr::drop_na(toxval_numeric) %>%
    dplyr::distinct()

  # Set toxval_type based on extracted species
  res$toxval_type <- ifelse(res$species == "office of water aquatic life criteria",
                            "Office of Water Aquatic Life Criteria", "OPP Aquatic Life Benchmarks")

  # Remove species from "Office of Water Aquatic Life Criteria" toxval_type
  res$species[res$toxval_type == "Office of Water Aquatic Life Criteria"] <- "-"

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
