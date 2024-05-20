#--------------------------------------------------------------------------------------
#' @#' Import of EPA OW NPDWR source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
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
#'  \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{separate}}
#'  \code{\link[stringr]{str_trim}}
#' @rdname import_source_epa_ow_npdwr
#' @export 
#' @importFrom readxl read_xlsx
#' @importFrom dplyr rename mutate across
#' @importFrom tidyr pivot_longer separate
#' @importFrom stringr str_squish
#--------------------------------------------------------------------------------------
import_source_epa_ow_npdwr <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "EPA OW NPDWR"
  source_table = "source_epa_ow_npdwr"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2023-02-28")
  dir = paste0(toxval.config()$datapath,"epa_ow_npdwr/epa_ow_npdwr_files/")
  file = paste0(dir,"epa_ow_npdwr_raw.xlsx")
  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  #
  # the final file should have column names that include "name" and "casrn"
  # additionally, the names in res need to match names in the source
  # database table. You do not need to add any of the generic columns
  # described in the SOP - they will get added in source_prep_and_load
  #
  # Load and clean source
  res <- res0 %>%
    dplyr::rename(name = Contaminant) %>%
    # Pivot MCLG and MCL columns to toxval_type and _numeric
    tidyr::pivot_longer(c("MCLG (mg/L)", "MCL or TT (mg/L)"),
                        names_to = "toxval_type",
                        values_to = "toxval_numeric") %>%
    # Split up units
    tidyr::separate(toxval_type, c("toxval_type", "toxval_units"), sep = "\\s\\(",
                    extra = "merge", fill = "right", remove = FALSE) %>%
    # Remove closing parentheses from units
    dplyr::mutate(toxval_units = gsub("\\)$", "", toxval_units),
                  # Clean up some of the low-hanging issues in toxval_numeric
                  toxval_numeric = gsub(" as of \\d\\d/\\d\\d/\\d\\d$|^none -+ ",
                                        "", toxval_numeric),
                  toxval_numeric = gsub("zero", 0, toxval_numeric),
                  toxval_numeric = gsub("(^--> )?n/a$", "N/A", toxval_numeric),
                  toxval_numeric = gsub(" MFL$", " million fibers per liter", toxval_numeric),
                  name = name %>%
                    gsub("Quick reference guide|Rule information|Consumer fact sheet", "", .) %>%
                    stringr::str_squish()
                    ) %>%
    dplyr::mutate(dplyr::across(c("toxval_type", "toxval_numeric", "toxval_units"),
                         ~stringr::str_squish(.))) %>%
    dplyr::rename(critical_effect="Potential Health Effects from Long-Term Exposure Above the MCL (unless specified as short-term)") %>%
    # Drop rows with subsource type as Microorganisms or Radionuclides
    dplyr::filter(!subsource_type %in% c("Microorganisms", "Radionuclides"),
                  # Drop rows with toxval_numeric TT/-/NA
                  !grepl("TT;|^TT", toxval_numeric),
                  !toxval_numeric %in% c("N/A", "NA", "-"),
                  !is.na(toxval_numeric))

  # Fixing toxval_numeric values that have the units with them (split into numeric and units)
  res$toxval_units[res$toxval_numeric == '7 million fibers per liter'] <- 'MFL'
  res$toxval_numeric[res$toxval_numeric == '7 million fibers per liter'] <- '7'
  res$toxval_units[res$toxval_numeric == '7 million fibers per liter (MFL)'] <- 'MFL'
  res$toxval_numeric[res$toxval_numeric == '7 million fibers per liter (MFL)'] <- '7'
  # Fixing toxval_numeric values that have toxval_types in them (split into numeric and toxval_type)
  res$toxval_type[res$toxval_numeric=='MRDL=4.0'] <- 'MRDL'
  res$toxval_numeric[res$toxval_numeric=='MRDL=4.0'] <- '4'
  res$toxval_type[res$toxval_numeric=='MRDLG=4'] <- 'MRDLG'
  res$toxval_numeric[res$toxval_numeric=='MRDLG=4'] <- '4'
  res$toxval_type[res$toxval_numeric=='MRDLG=0.8'] <- 'MRDLG'
  res$toxval_numeric[res$toxval_numeric=='MRDLG=0.8'] <- '0.8'
  res$toxval_type[res$toxval_numeric=='MRDL=0.8'] <- 'MRDL'
  res$toxval_numeric[res$toxval_numeric=='MRDL=0.8'] <- '0.8'

  # Convert to numeric
  res$toxval_numeric <- as.numeric(res$toxval_numeric)

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




