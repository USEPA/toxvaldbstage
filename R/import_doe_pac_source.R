#--------------------------------------------------------------------------------------
#' @description Load DOE Source into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#' @title import_doe_pac_source
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  [read_xlsx][readxl::read_xlsx]
#'  [str_squish][stringr::str_squish], [str_extract_all][stringr::str_extract_all]
#'  [mutate][dplyr::mutate], [across][dplyr::across], [rename][dplyr::rename], [select][dplyr::select], [rowwise][dplyr::rowwise], [ungroup][dplyr::ungroup]
#'  [pivot_longer][tidyr::pivot_longer]
#' @rdname import_doe_pac_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_squish str_extract_all
#' @importFrom dplyr mutate across rename select rowwise ungroup
#' @importFrom tidyr pivot_longer
#' @param do.reset PARAM_DESCRIPTION, Default: FALSE
#' @param do.insert PARAM_DESCRIPTION, Default: FALSE
#' @importFrom tidyselect where
import_doe_pac_source <- function(db,
                                  chem.check.halt=FALSE,
                                  do.reset=FALSE,
                                  do.insert=FALSE) {
  printCurrentFunction(db)
  source = "DOE Protective Action Criteria"
  source_table = "source_doe_pac"

  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2023-10-01")

  dir = paste0(toxval.config()$datapath,"doe_pac/doe_pac_files/")

  file = paste0(dir, "source_doe_pac_oct_2023.xlsx")

  # NOTE: Information on REC TEELS sheet is redundant (included on Input sheet)
  # RELEVANT SOURCE TEXT: "Table 2 PACs by Chemical Name is a list of the same
  # PAC values as presented in Table 1, but only shows the PACs and Source for
  # the PACs values. They are presented in either ppm or mg/m³."
  res0 <- readxl::read_xlsx(file, sheet="Input", skip = 2, col_names = FALSE)

  #####################################################################
  cat("Build new_doe_table\n")

  # Extract headers
  header <- readxl::read_xlsx(file, sheet="Input", n_max = 2, col_names=FALSE) %>%
    unlist() %>%
    # Remove NA
    .[!is.na(.)] %>%
    unname() %>%
    # Remove merged parent headers
    .[!. %in% c("Vapor  Pressure", "PACs based on AEGLs, ERPGs, or TEELs", "PAC-TEEL Derivation/Review/Revision Dates")] %>%
    # Remove excess whitespace
    stringr::str_squish()

  # Add vapor pressure prefix where needed
  header[header %in% c("mm Hg", "T (°C)")] = paste0("Vapor Pressure - ", header[header %in% c("mm Hg", "T (°C)")])

  # Apply headers
  names(res0) = header

  # Check for numeric conversion NA coercions to fix
  # data.frame(res0$`LEL (ppm)`, num=as.numeric(res0$`LEL (ppm)`)) %>% distinct() %>% filter(is.na(num)) %>% View()

  # Clean and pivot toxval_type and numeric fields
  res = res0 %>%
    dplyr::mutate(dplyr::across(c("PAC-1", "PAC-2", "PAC-3", "LEL (ppm)"),
                                # Remove * and convert to numeric
                                ~gsub("[*]", "", .) %>%
                                  as.numeric() %>%
                                  # Set significant figures to 3
                                  signif(digits=3))) %>%
    tidyr::pivot_longer(cols=c("PAC-1", "PAC-2", "PAC-3", "LEL (ppm)"),
                        names_to = "toxval_type",
                        values_to = "toxval_numeric") %>%

    # Remove entries with NA toxval_numeric value
    tidyr::drop_na("toxval_numeric") %>%

    dplyr::rename("BP (°C)"="BP (°C) @ 760 mm Hg unless indicated",
                  "SG"="SG @ 25°C unless indicated") %>%
    dplyr::mutate(
      # Reference Jira Ticket: TOXVAL-681
      # Determined species and experimental_record from reading Temporary Emergency
      # Exposure Limits for Chemicals: Method and Practice (doe.gov)
      # and the content of the column "PACs based on AEGLs, ERPGs, or TEELs" in the source document.
      species = 'human',
      human_eco = "human health",
      experimental_record = "No",
      # Add toxval columns, not replacing original
      name = `Chemical Compound`,
      casrn = `CAS Number (CASRN)`,
      toxval_units = Units,
      study_type = "acute",
      exposure_route = "inhalation",
      long_ref = "U.S. Department of Energy (DOE) Protective Action Criteria (PAC). 2023. PAC Chemical Database. Updated 11 October 2023. Available: https://edms3.energy.gov/pac/ (Accessed November 16, 2023)",
      toxval_type = toxval_type %>%
        gsub("\\(ppm\\)", "", .),
      # Remove excess whitespace for all character columns
      dplyr::across(tidyselect::where(is.character), ~stringr::str_squish(.))
    ) %>%
    dplyr::select(-`No.`) %>%
    # Split CASRN lists
    tidyr::separate_rows(casrn, sep = " ")

  # Fill LEL units
  res$toxval_units[res$toxval_type == "LEL"] = "ppm"
  # Fill in BP values column
  res$`BP (°C)`[is.na(res$`BP (°C)`)] <- 760
  # Fix SP column
  res$`SG`[is.na(res$`SG`)] <- 25

  res <- res %>%
    dplyr::rowwise() %>%
    dplyr::mutate(# Fill in year Revised > Reviewed > Derived
                  year = ifelse(!is.na(`Last Revised`), format(as.Date(`Last Revised`, format="%m/%d/%y"),"%Y"),
                                ifelse(!is.na(`Last Reviewed`), format(as.Date(`Last Reviewed`, format="%m/%d/%y"),"%Y"),
                                       format(as.Date(`Originally Derived`, format="%m/%d/%y"),"%Y")))) %>%
    dplyr::ungroup()

  # Chemical name cleaning
  res <- res %>% dplyr::mutate(
    name = name %>%
      # Fix symbols
      fix.replace.unicode() %>%
      # Remove excess whitespace
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




