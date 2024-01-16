#--------------------------------------------------------------------------------------
#' @title import_rsl_source
#'
#' @description Import of RSL 2023 source into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile1 The input file ./rsl/rsl_files/rsl_thq10_nov_2022.xlsx
#' @param infile2 The input file ./rsl/rsl_files/rsl_thq01_nov_2022.xlsx
#' @param infile3 The input file ./rsl/rsl_files/rsl_subchronic_nov_2022.xlsx
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @return None; data is sent to toxval_source
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{mutate_all}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{na_if}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{case_when}}
#'  \code{\link[tidyr]{unite}}, \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{drop_na}}
#'  \code{\link[stringr]{str_trim}}
#'  \code{\link[purrr]{reexports}}
#' @rdname import_rsl_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr bind_rows mutate_all mutate na_if across where case_when
#' @importFrom tidyr unite pivot_longer separate drop_na
#' @importFrom stringr str_squish
#' @importFrom purrr is_character
#--------------------------------------------------------------------------------------
import_rsl_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "RSL"
  source_table = "source_rsl"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2023-11-01")
  dir = paste0(toxval.config()$datapath,"rsl/rsl_files/")

  # Read from three input files
  file0 = paste0(dir, "rsl_thq01_nov_2023.xlsx")
  res0 = readxl::read_xlsx(file0, skip = 2)
  res0$raw_input_file = "rsl_thq01_nov_2023.xlsx"

  file1 = paste0(dir, "rsl_thq10_nov_2023.xlsx")
  res1 = readxl::read_xlsx(file1, skip = 2)
  res1$raw_input_file = "rsl_thq10_nov_2023.xlsx"

  file2 = paste0(dir, "rsl_subchronic_nov_2023.xlsx")
  res2 = readxl::read_xlsx(file2, skip = 2)
  res2$raw_input_file = "rsl_subchronic_nov_2023.xlsx"
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  # Hard-code renaming to account for "key" columns and allow for smooth combination
  names(res0) = c("SFO (mg/kg-day)-1 - val", "SFO (mg/kg-day)-1 - key",
                     "IUR (ug/m3)-1 - val", "IUR (ug/m3)-1 - key",
                     "RfD (mg/kg-day) - val", "RfD (mg/kg-day) - key",
                     "RfC (mg/m3) - val", "RfC (mg/m3) - key",
                     "vol", "mutagen", "GIABS", "ABSd", "Csat (mg/kg)",
                     "Analyte", "CAS N.",
                     "Resident Soil (mg/kg) - val", "Resident Soil (mg/kg) - key",
                     "Industrial Soil (mg/kg) - val", "Industrial Soil (mg/kg) - key",
                     "Resident Air (ug/m3) - val", "Resident Air (ug/m3) - key",
                     "Industrial Air (ug/m3) - val", "Industrial Air (ug/m3) - key",
                     "Tap Water (ug/L) - val", "Tap Water (ug/L) - key", "MCL (ug/L)",
                     "Risk-based SSL (mg/kg) - val", "Risk-based SSL (mg/kg) - key",
                     "MCL-based SSL (mg/kg)", "raw_input_file")
  names(res1) = c("SFO (mg/kg-day)-1 - val", "SFO (mg/kg-day)-1 - key",
                    "IUR (ug/m3)-1 - val", "IUR (ug/m3)-1 - key",
                    "RfD (mg/kg-day) - val", "RfD (mg/kg-day) - key",
                    "RfC (mg/m3) - val", "RfC (mg/m3) - key",
                    "vol", "mutagen", "GIABS", "ABSd", "Csat (mg/kg)",
                    "Analyte", "CAS N.",
                    "Resident Soil (mg/kg) - val", "Resident Soil (mg/kg) - key",
                    "Industrial Soil (mg/kg) - val", "Industrial Soil (mg/kg) - key",
                    "Resident Air (ug/m3) - val", "Resident Air (ug/m3) - key",
                    "Industrial Air (ug/m3) - val", "Industrial Air (ug/m3) - key",
                    "Tap Water (ug/L) - val", "Tap Water (ug/L) - key", "MCL (ug/L)",
                    "Risk-based SSL (mg/kg) - val", "Risk-based SSL (mg/kg) - key",
                  "MCL-based SSL (mg/kg)", "raw_input_file")

  # First two files are in the same format and can be immediately combined
  res_thq = dplyr::bind_rows(res0, res1) %>%
    # Set all cols as characters to ease merging/pivot
    dplyr::mutate_all(as.character) %>%

    # Combine values and keys for later transformations
    tidyr::unite("SFO (mg/kg-day)-1", c(`SFO (mg/kg-day)-1 - val`, `SFO (mg/kg-day)-1 - key`),
                 sep = " ", remove = TRUE) %>%
    tidyr::unite("IUR (ug/m3)-1", c(`IUR (ug/m3)-1 - val`, `IUR (ug/m3)-1 - key`),
                 sep = " ", remove = TRUE) %>%
    tidyr::unite("RfD (mg/kg-day)",c(`RfD (mg/kg-day) - val`,`RfD (mg/kg-day) - key`),
                 sep = " ", remove = TRUE) %>%
    tidyr::unite("RfC (mg/m3)", c(`RfC (mg/m3) - val`, `RfC (mg/m3) - key`),
                 sep = " ", remove = TRUE ) %>%
    tidyr::unite("Resident Soil (mg/kg)", c(`Resident Soil (mg/kg) - val`, `Resident Soil (mg/kg) - key`),
                 sep = " ", remove = TRUE ) %>%
    tidyr::unite("Industrial Soil (mg/kg)", c(`Industrial Soil (mg/kg) - val`, `Industrial Soil (mg/kg) - key`),
                 sep = " ", remove = TRUE ) %>%
    tidyr::unite("Resident Air (ug/m3)", c(`Resident Air (ug/m3) - val`, `Resident Air (ug/m3) - key`),
                 sep = " ", remove = TRUE ) %>%
    tidyr::unite("Industrial Air (ug/m3)", c(`Industrial Air (ug/m3) - val`, `Industrial Air (ug/m3) - key`),
                 sep = " ", remove = TRUE ) %>%
    tidyr::unite("Tap Water (ug/L)", c(`Tap Water (ug/L) - val`, `Tap Water (ug/L) - key`),
                 sep = " ", remove = TRUE ) %>%
    tidyr::unite("Risk-based SSL (mg/kg)", c(`Risk-based SSL (mg/kg) - val`, `Risk-based SSL (mg/kg) - key`),
                 sep = " ", remove = TRUE ) %>%

    # Get toxval_type_units, and source_value from united cols
    tidyr::pivot_longer(cols=c("SFO (mg/kg-day)-1", "IUR (ug/m3)-1",
                               "RfD (mg/kg-day)", "RfC (mg/m3)",
                               "Resident Soil (mg/kg)", "Industrial Soil (mg/kg)",
                               "Resident Air (ug/m3)", "Industrial Air (ug/m3)",
                               "Tap Water (ug/L)", "Risk-based SSL (mg/kg)",
                               "GIABS", "ABSd", "Csat (mg/kg)", "MCL (ug/L)",
                               "MCL-based SSL (mg/kg)"),
                        names_to = "toxval_type_units",
                        values_to = "source_value")

  # Remove noise from subchronic field names
  names(res2) = stringr::str_squish(names(res2)) %>%
    gsub("\\(", " \\(", .) %>%
    gsub("i\\b|o\\b", "", .)

  # Pre-process subchronic data before merging
  res_subchronic = res2 %>%
    # Set all cols as characters to ease merging/pivot
    dplyr::mutate_all(as.character) %>%

    # Combine values and refs for later transformations
    tidyr::unite("RfD (mg/kg-day)", c(`RfD (mg/kg-day)`, `RfD Reference`),
                 sep = " ", remove = TRUE) %>%
    tidyr::unite("SRfD (mg/kg-day)", c(`SRfD (mg/kg-day)`, `SRfD Reference`),
                 sep = " ", remove = TRUE) %>%
    tidyr::unite("RfC (mg/m3)", c(`RfC (mg/m3)`, `RfC Reference`),
                 sep = " ", remove = TRUE) %>%
    tidyr::unite("SRfC (mg/m3)", c(`SRfC (mg/m3)`, `SRfC Reference`),
                 sep = " ", remove = TRUE) %>%

    # Get toxval_type_units, and source_value from united cols
    tidyr::pivot_longer(cols=c("RfD (mg/kg-day)", "RfC (mg/m3)",
                               "SRfD (mg/kg-day)", "SRfC (mg/m3)"),
                        names_to = "toxval_type_units",
                        values_to = "source_value") %>%

    # Add empty cols to enable merging
    dplyr::mutate(vol = NA, mutagen = NA)

  res = dplyr::bind_rows(res_thq, res_subchronic) %>%
    # Add basic columns
    dplyr::mutate(
      name = gsub("~", "", Analyte),
      casrn = sapply(`CAS N.`, FUN=fix.casrn) %>% dplyr::na_if("NOCAS"),
      source_url = "https://www.epa.gov/risk/regional-screening-levels-rsls-generic-tables"
    ) %>%
    # Extract toxval_type and toxval_units
    tidyr::separate(
      col = toxval_type_units,
      into = c("toxval_type", "toxval_units"),
      sep = " \\(",
      remove = TRUE,
      fill = "right"
    ) %>%

    # Extract toxval_numeric and subsource
    tidyr::separate(
      col = source_value,
      into = c("toxval_numeric", "subsource"),
      sep = " ",
      remove = FALSE,
      fill = "right"
    ) %>%

    # Replace NAs with actual NA value
    dplyr::mutate(dplyr::across(dplyr::where(purrr::is_character),
                                .fns = ~replace(., . == "NA", NA))) %>%

    # Clean values as needed
    dplyr::mutate(
      # Fix toxval_units formatting and set "unitless" where appropriate (from older script)
      toxval_units = dplyr::case_when(
        grepl("\\-1", toxval_units) ~ paste0("(", toxval_units),
        toxval_type %in% c("GIABS", "ABSd") ~ "unitless",
        TRUE ~ gsub("[\\(\\)]", "", toxval_units)
      ),

      # Clean toxval_numeric and set as number
      toxval_numeric = gsub("\\(G\\)", "", toxval_numeric) %>% as.numeric(),

      # Rename toxval_type to better align with previous import script
      toxval_type = dplyr::case_when(
        toxval_type == "MCL-based SSL" ~ "Protection of Groundwater: MCL-based SSL",
        toxval_type == "Risk-based SSL" ~ "Protection of Groundwater: Risk-based SSL",
        grepl("Industrial|Resident|Tap Water|MCL", toxval_type) ~ paste0("Screening Level (", toxval_type, ")"),
        toxval_type == "Csat" ~ "Soil Saturation Limit (Csat)",
        TRUE ~ toxval_type
      ),

      # Get study_type from subsource field
      study_type = dplyr::case_when(
        grepl("c|n|\\*|m|s", subsource) ~ subsource %>%
          gsub("c", "cancer", .) %>%
          gsub("\\bn|n\\b", "noncancer", .) %>%
          gsub("\\*\\*", " where noncancer SL < 10X cancer SL, SSL values are based on DAF=1", .) %>%
          gsub("\\*", " where noncancer SL < 100X cancer SL", .) %>%
          gsub("m", ", ceiling limit exceeded", .) %>%
          gsub("\\bs\\b|s$", ", Csat exceeded", .) %>%
          gsub("G", "", .) %>%
          stringr::str_squish() %>%
          gsub(",$", "", .),
        TRUE ~ NA
      ),

      # Translate subsource values according to key in source docs
      subsource = dplyr::case_when(
        grepl("Screening Level", toxval_type) ~ "EPA Regions",
        subsource == "I" ~ "IRIS",
        subsource == "P" ~ "PPRTV",
        subsource == "O" ~ "OPP",
        subsource == "A" ~ "ATSDR",
        subsource %in% c("C", "CALEPA") ~ "Cal EPA",
        subsource == "X" ~ "PPRTV Screening Level",
        subsource == "H" ~ "HEAST",
        subsource == "D" ~ "OW",
        subsource == "R" ~ "ORD",
        subsource == "N" ~ "WI",
        subsource == "W" ~ "TEF applied",
        subsource == "E" ~ "RPF applied",
        subsource == "G" ~ "see user's guide",
        grepl("[A-Z]", subsource) ~ subsource,
        TRUE ~ NA
      ),

      # Get risk_assessment_type
      risk_assessment_type = dplyr::case_when(
        toxval_type %in% c("SFO", "IUR") ~ "carcinogenicity",
        TRUE ~ ""
      ),

      # Get "toxval_subtype" column for all 'Screening' toxval_type entries with a "cancer" risk_assessment_class
      toxval_subtype = dplyr::case_when(
        grepl("Screening Level", toxval_type) & study_type == "cancer"  ~ "TR = 1E-06",
        grepl("Screening Level", toxval_type) & grepl("\\bcancer where noncancer", study_type) ~ "TR = 1E-06",
        raw_input_file == "rsl_thq10_nov_2023.xlsx" ~ "Thq = 1",
        raw_input_file == "rsl_thq01_nov_2023.xlsx" ~ "Thq = 0.1",
        TRUE ~ ""
      ),

      # Get exposure_route based on toxval_type
      exposure_route = dplyr::case_when(
        toxval_type =="RfC"  ~ "inhalation",
        toxval_type =="SRfC"  ~ "inhalation",
        toxval_type =="RfD"  ~ "oral",
        toxval_type =="SRfD"  ~ "oral",
        toxval_type =="Protection of Groundwater: MCL-based SSL"  ~ "oral",
        toxval_type =="Protection of Groundwater: Risk-based SSL"  ~ "oral",
        toxval_type =="Screening Level (Resident Soil)"  ~ "oral, dermal, and inhalation",
        toxval_type =="Screening Level (Industrial Soil)"  ~ "oral and inhalation",
        toxval_type =="Screening Level (Resident Air)"  ~ "inhalation",
        toxval_type =="Screening Level (Industrial Air)"  ~ "inhalation",
        toxval_type =="Screening Level (Tap Water)"  ~ "oral, dermal, and inhalation",
        toxval_type =="Screening Level (MCL)"  ~ "vary by chemical",
        TRUE ~ ""
      ),

      # Get risk_assessment_class based on toxval_type
      risk_assessment_class = dplyr::case_when(
        toxval_type %in% c("SRfC", "SRfD") ~ "subchronic",
        toxval_type %in% c("GIABS", "ABSd") ~ "PhysChem",
        TRUE ~ "chronic"
      ),

      # Handle entries with "(units in fibers)
      toxval_units = dplyr::case_when(
        grepl("units in fibers", name) ~ gsub("ug|mg", "fibers", toxval_units),
        TRUE ~ toxval_units
      ),
      name = gsub(" \\(units in fibers\\)", "", name),

      # Uncomment logic below to rename SRfC/SRfD/RfD/RfC values in toxval_type to match old import script
      # toxval_type = dplyr::case_when(
      #   toxval_type == "SRfC" ~ "SRfCi",
      #   toxval_type == "SRfD" ~ "SRfDo",
      #   toxval_type == "RfC" ~ "RfCi",
      #   toxval_type == "RfD" ~ "RfDo",
      #   TRUE ~ toxval_type
      # )
    ) %>%

    # Filter out NA/blank toxval_numeric values
    tidyr::drop_na(toxval_numeric)

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




