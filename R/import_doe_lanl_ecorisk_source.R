#--------------------------------------------------------------------------------------
#' @description Import doe_lanl_ecorisk source into ToxVal
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_doe_lanl_ecorisk_source
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
#'  \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_remove}}
#'  \code{\link[stringr]{fixed}}, \code{\link[stringr]{str_squish}}
#'  \code{\link[tidyr]{pivot_longer}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{rename}}
#'  \code{\link[tidyselect]{all_of}}
#' @rdname import_doe_lanl_ecorisk_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_squish str_trim str_remove fixed
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate distinct rename
#' @importFrom tidyselect all_of
#' ---------------------------------------------------
import_doe_lanl_ecorisk_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "DOE LANL ECORISK"
  source_table = "source_doe_lanl_ecorisk"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2022-09-01")
  dir = paste0(toxval.config()$datapath,"doe_lanl_ecorisk/doe_lanl_ecorisk_files/")

  file = paste0(dir,"ESLs_R4.3.xlsx")

  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Build original_doe_lanl_ecorisk_table\n")

  # Rename fields due to file error with shifted columns
  rename_map = c("Analyte Group"="Analyte Group",
                 "Analyte Code"="Analyte Name",
                 "Analyte Name"="Analyte Code",
                 "Analyte CAS"="ESL Medium",
                 "ESL Medium"="ESL Receptor",
                 "ESL Receptor"="No Effect ESL",
                 "No Effect ESL"="Low Effect ESL",
                 "Low Effect ESL"="Units",
                 "Units"="Minimum ESL",
                 "Minimum ESL"="Note",
                 "ESL ID"="ESL ID")
  res = res0 %>%
    dplyr::rename(tidyselect::all_of(rename_map))

  # Handle bad casrn values (could be replaced with cas_checkSum.R)
  bad.casrn = c("AL","SB","AS","BA","BE",
                "B","CD","CL(-1)","CR","CR(+6)","CO","CU",
                "CN(-1)","F(-1)","FE","PB","LI","MN","HGI",
                "HGM","MO","NI","ClO4(-1)","SE","AG","SR",
                "TL","TI","U","V","ZN",
                "AM-241","CS-134","CS-137/ BA-137","CO-60","EU-152",
                "PB-210","NP-237","PU-238","PU-239/240","PU-241","RA-226","RA-228",
                "NA-22","SR-90/ Y-90","TH-228","TH-229","TH-230","TH-232","H-3",
                "U-233","U-234","U-235","U-236","U-238", "5H 4:1 FTOH", "6:2 FTOH", "GenX",
                "TPH F2F3", "N-EtFOSE")
  # Set bad casrn values as "-"
  res$`Analyte CAS`[res$`Analyte CAS` %in% bad.casrn] = "-"

  # Add new toxval columns as needed but retain original columns
  res = res %>%
    dplyr::mutate(
      name =`Analyte Name`,
      casrn = `Analyte CAS`,
      toxval_units = fix.greek.symbols(Units),
      media = `ESL Medium` %>%
        tolower(),
      species = `ESL Receptor` %>%
        # Remove parenthesis and medium
        # Elected not to remove because diet is model specific to the ESL
        # gsub("(.*)(\\([^\\(].*)", "\\1", .) %>%
        # sub('-.*', '', .) %>%
        # Remove (water), - sediment, - water
        gsub("\\(water\\)|- water|- sediment", "", .) %>%
        stringr::str_squish() %>%
        tolower()) %>%
  # Pivot to add toxval_type and toxval_numeric
    tidyr::pivot_longer(cols = c('No Effect ESL',
                          'Low Effect ESL'),
                 names_to = 'toxval_type',
                 values_to = 'toxval_numeric'
    ) %>%
    dplyr::distinct() %>%

    # Extract diet information as toxval_subtype
    tidyr::separate(species,
                    sep = " \\(",
                    into = c("species", "toxval_subtype"),
                    fill="right") %>%

    # Final cleaning
    dplyr::mutate(
      # Remove closing parentheses from toxval_subtype
      toxval_subtype = stringr::str_remove(toxval_subtype, stringr::fixed(")")),

      # Handle symbols in name
      name = name %>%
        # Fix Greek symbols
        fix.greek.symbols() %>%

        # Fix escaped quotation marks
        gsub("[\\]{1,}'", "'", .) %>%
        gsub('[\\]{1,}"', '"', .) %>%

      # Remove whitespace
      stringr::str_squish()
    )

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  # Add version date. Can be converted to a mutate statement as needed
  res$source_version_date <- src_version_date

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"
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




