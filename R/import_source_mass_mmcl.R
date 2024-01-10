#--------------------------------------------------------------------------------------
#' @title import_source_mass_mmcl
#' @description Load Mass. Drinking Water Standards into toxval_source
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
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{coalesce}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{case_when}}
#'  \code{\link[purrr]{reexports}}
#'  \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{drop_na}}, \code{\link[tidyr]{separate}}
#'  \code{\link[stringr]{str_match}}, \code{\link[stringr]{str_trim}}
#' @rdname import_source_mass_mmcl
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate across rename coalesce filter case_when
#' @importFrom purrr is_character
#' @importFrom tidyr pivot_longer drop_na separate
#' @importFrom stringr str_match str_squish
#--------------------------------------------------------------------------------------
import_source_mass_mmcl <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "Mass. Drinking Water Standards"
  source_table = "source_mass_mmcl"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2020-12-01")
  dir = paste0(toxval.config()$datapath,"mass_mmcl/mass_mmcl_files/")
  file = paste0(dir,"mass_drinking_water_standards_winter_2020.xlsx")
  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  res = res0 %>%
    # Remove any instances of N/A
    dplyr::mutate(dplyr::across(where(purrr::is_character),
                                .fns = ~replace(., . %in% c("N/A", "N/A10"), NA))) %>%

    # Rename columns as needed to avoid duplicates
    dplyr::rename(casrn="CASRN", table_title="Table Title") %>%

    dplyr::mutate(
      # Add/mutate new columns as needed
      name = dplyr::coalesce(SUBSTANCE, `Chemicals/Parameter`) %>% fix.replace.unicode(),
      casrn = sapply(casrn, FUN=fix.casrn),
      table_title = fix.replace.unicode(table_title),
      exposure_route = "oral",
      media = "water",
      year = "2020",
      subsource = "Massachusetts DEP",
      source_url = "https://www.mass.gov/guides/drinking-water-standards-and-guidelines",
      risk_assessment_class = "chronic"
    ) %>%

    # Remove "Biologicals" entries
    dplyr::filter(`Substance Type`!="Biologicals" | is.na(`Substance Type`)) %>%

    # Get initial toxval_type_units and source_value
    tidyr::pivot_longer(cols=c("MMCL (mg/L)",
                               "MMCL",
                               "ORSG (mg/L)",
                               "SMCL (mg/L)"),
                        names_to = "toxval_type_units",
                        values_to = "source_value") %>%

    # Remove entries without a numeric value
    tidyr::drop_na("source_value") %>%

    # Separate toxval_type_units into toxval_type and toxval_units
    tidyr::separate(
      col = toxval_type_units,
      into = c("toxval_type", "toxval_units"),
      sep = " ",
      fill = "right",
      remove = TRUE
    ) %>%

    dplyr::mutate(
      name = dplyr::case_when(
        # Remove annotation numbers from names (Radon and Petroleum edge cases)
        grepl("Radon", name) ~ gsub("[0-9]$", "", name),
        grepl("Petroleum hydrocarbons", name) ~ gsub("carbons8", "carbons", name),
        TRUE ~ gsub("[0-9]+$", "", name)
      ),

      # Remove annotation numbers from Status column and translate
      Status = Status %>% gsub("[0-9]", "", .) %>%
        gsub("F", "Final", .) %>%
        gsub("A", "Advisory", .),

      # Remove parentheses form toxval_units
      toxval_units = gsub("[\\(\\)]", "", toxval_units),

      # Remove symbols from source_value
      source_value = fix.replace.unicode(source_value),

      # Override toxval_type when different type specified
      toxval_type = dplyr::case_when(
        # Check TYPE OF GUIDANCE column
        !is.na(`TYPE OF GUIDANCE`) ~ `TYPE OF GUIDANCE`,
        # Check units column
        grepl("\\([A-Z]+[0-9]?\\)", source_value) ~ stringr::str_match(source_value,
                                                                   "\\(([A-Z]+)[0-9]?\\)")[,2],
        # Handle edge case type in source_value
        grepl("Action Level", source_value) ~ "Action Level",
        # Otherwise use original value
        TRUE ~ toxval_type
      ),

      # Override toxval_units when different units specified
      toxval_units = dplyr::case_when(
        # Units specified in format x/y
        grepl("[a-zA-Z]+/[a-zA-Z]+", source_value) ~ stringr::str_match(source_value,
                                                                                "[a-zA-Z]+/[a-zA-Z]+"),
        # Handle edge case units
        grepl("Color Units|threshold odor numbers", source_value) ~ stringr::str_match(
                                                                      source_value,
                                                                      "Color Units|threshold odor numbers"
                                                                    ),
        TRUE ~ toxval_units
      ),

      # Get toxval_numeric_qualifier (convert to vector)
      toxval_numeric_qualifier = stringr::str_match(source_value, "[<>=]") %>% c(),

      # Get study_duration
      study_duration = stringr::str_match(source_value, "[0-9]+ days|lifetime"),

      # Finalize toxval_numeric
      toxval_numeric = source_value %>%
        # Hard-code handle scientific notation
        gsub("3 x 10-8", "0.00000003", .) %>%
        # Hard-code handle 7 million
        gsub("7 million fibers/liter", "7000000", .) %>%
        # Replace to with hyphen for range
        gsub(" to ", "-", .) %>%
        # Replace NA
        gsub("use guidance for individual chemicals|^.*Treatment Technique.*$", NA, .) %>%
        # Remove extraneous strings
        gsub("millirem/yr|pCi/L|concentration which produces an annual dose of ", "", .) %>%
        # Remove parenthetic toxval_type
        sub('\\(.*', '', .) %>%
        # Remove excess whitespace
        stringr::str_squish()
    ) %>%

    # Get study_duration_value and study_duration_units
    tidyr::separate(
      col = study_duration,
      into = c("study_duration_value", "study_duration_units"),
      sep = " ",
      fill = "left",
      remove = TRUE
    ) %>%
    dplyr::mutate(
      study_duration_value = dplyr::case_when(
        study_duration_units == "lifetime" ~ "1",
        TRUE ~ study_duration_value
      ) %>% as.numeric
    ) %>%

    # Drop entries with NA toxval_numeric
    tidyr::drop_na(toxval_numeric) %>%

    # Remove select non-chemicals/categories
    dplyr::filter(!name %in% c("Odor", "Color", "Corrosivity", "Foaming agents",
                               "pH", "Total dissolved solids (TDS)"))

  # Compare to input PDF file for spot check
  # res %>% select(name, toxval_type, toxval_numeric, toxval_units, population, study_duration_value, study_duration_units) %>% View()

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




