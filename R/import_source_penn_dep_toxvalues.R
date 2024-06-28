#--------------------------------------------------------------------------------------
#' @title import_source_penn_dep_toxvalues
#' @description Load Penn DEP ToxValues Source into toxval_source
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
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{case_when}}
#'  \code{\link[stringr]{str_trim}}
#'  \code{\link[tidyr]{unite}}, \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{separate}}
#' @rdname source_penn_dep_toxvalues
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate filter case_when
#' @importFrom stringr str_squish
#' @importFrom tidyr unite pivot_longer separate
import_source_penn_dep_toxvalues <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "Pennsylvania Dep ToxValues"
  source_table = "source_penn_dep_toxvalues"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2021-11-20")
  dir = paste0(toxval.config()$datapath,"penn_dep_toxvalues/penn_dep_toxvalues_files/")
  file = paste0(dir,"PENN_DEP_Table 5a_20211120.xlsx")
  res0 = readxl::read_xlsx(file, skip = 7, col_names = FALSE, n_max = 371)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  # Manually write header to account for cols with no name
  names(res0) = c("Regulated Subtance", "CAS", "RfDo (mg/kg-d) - val", "RfDo (mg/kg-d) - cat",
                  "CSFo (mg/kg-d)-1 - val", "CSFo (mg/kg-d)-1 - cat", "RfCi (mg/m3) - val",
                  "RfCi (mg/m3) - cat", "IUR (ug/m3)-1 - val", "IUR (ug/m3)-1 - cat",
                  "Koc", "VOC?", "Aqueous Solubility (mg/L)", "Aqueous Solubility Reference",
                  "TF Vol from Surface Soil", "TF Vol from SubSurface Soil", "Organic Liquid",
                  "Boiling Point (degrees C)", "Degradation Coefficient (K)(yr-1)")

  res = res0 %>%
    dplyr::mutate(
      # Rename/add columns as needed
      name = stringr::str_squish(`Regulated Subtance`),
      casrn = CAS,
      source_url = "https://files.dep.state.pa.us/EnvironmentalCleanupBrownfields/LandRecyclingProgram/LandRecyclingProgramPortalFiles/GuidanceTechTools/VaporIntrusion/November_2021/Table%205a.pdf"
    )  %>%

    # Combine values and categories to enable later transformations
    tidyr::unite(
      "RfDo (mg/kg-d)",
      c(`RfDo (mg/kg-d) - val`,
        `RfDo (mg/kg-d) - cat`),
      sep = " ",
      remove = TRUE
    ) %>%
    tidyr::unite(
      "CSFo (mg/kg-d)-1",
      c(`CSFo (mg/kg-d)-1 - val`,
        `CSFo (mg/kg-d)-1 - cat`),
      sep = " ",
      remove = TRUE
    ) %>%
    tidyr::unite(
      "RfCi (mg/m3)",
      c(`RfCi (mg/m3) - val`,
        `RfCi (mg/m3) - cat`),
      sep = " ",
      remove = TRUE
    ) %>%
    tidyr::unite(
      "IUR (ug/m3)-1",
      c(`IUR (ug/m3)-1 - val`,
        `IUR (ug/m3)-1 - cat`),
      sep = " ",
      remove = TRUE
    ) %>%

    # Get toxval_type_units
    tidyr::pivot_longer(cols=c("RfDo (mg/kg-d)",
                               "CSFo (mg/kg-d)-1",
                               "RfCi (mg/m3)",
                               "IUR (ug/m3)-1"),
                        names_to = "toxval_type_units",
                        values_to = "source_value") %>%

    # Remove NA rows
    dplyr::filter(!grepl("NA", source_value)) %>%

    # Separate toxval_numeric and subsource from source_value
    tidyr::separate(
      col = source_value,
      into = c("toxval_numeric", "subsource"),
      sep = " ",
      remove = FALSE
    ) %>%

    # Separate toxval_type and toxval_units
    tidyr::separate(
      col = toxval_type_units,
      into = c("toxval_type", "toxval_units"),
      sep = " ",
      remove = TRUE
    ) %>%

    dplyr::mutate(

      # Set toxval_numeric as numeric
      toxval_numeric = as.numeric(toxval_numeric),

      # Translate subsource abbreviations
      subsource = dplyr::case_when(
        subsource == "C" ~ "California EPA",
        subsource == "D" ~ "ATSDR Minimal Risk Level",
        subsource == "H" ~ "Health Effects Assessment Summary Table (HEAST)",
        subsource == "I" ~ "Integrated Risk information System (IRIS)",
        subsource == "M" ~ "EPA Drinking Water Regulations and Health Advisories",
        subsource == "O" ~ "EPA Office of Pesticide Programs Human Health Benchmarks for Pesticides",
        subsource == "P" ~ "EPA  Provisional Peer-Reviewed Toxicity Value",
        subsource == "X" ~ "EPA Provisional Peer-Reviewed Toxicity Value Appendix",
        subsource == "TE" ~ "TERA ITER Peer-Reviewed Value",
        subsource == "S1" ~ "Acenaphthene surrogate",
        subsource == "S2" ~ "Trans-Crotonaldehyde surrogate",
        subsource == "S3" ~ "Endosulfan surrogate",
        subsource == "S4" ~ "Naphthalene surrogate",
        subsource == "S5" ~ "2-Naphthylamine surrogate",
        subsource == "S6" ~ "4-Nitrophenol surrogate",
        subsource == "S7" ~ "Total PCBS surrogate",
        subsource == "S8" ~ "Anthracene surrogate",
        subsource == "S9" ~ "O-Toluidine surrogate",
        subsource == "S10" ~ "1,2,4-Trichlorobenzene surrogate",
        TRUE ~ NA_character_,
      )
    )

  # Remove parentheses form toxval_units (but not case of ^-1)
  res$toxval_units[grepl("\\)$", res$toxval_units)] = res$toxval_units[grepl("\\)$", res$toxval_units)] %>%
    gsub("[\\(\\)]", "", .)

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




