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
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{c("rowwise", "rowwise", "rowwise")}}, \code{\link[dplyr]{distinct}}
#'  \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{reexports}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{replace_na}}
#'  \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_replace}}, \code{\link[stringr]{str_extract}}
#' @rdname import_doe_pac_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate filter select across rename rowwise distinct
#' @importFrom tidyr pivot_longer all_of separate replace_na
#' @importFrom stringr str_squish str_replace_all str_extract
#--------------------------------------------------------------------------------------
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
  # res0 <- readxl::read_xlsx(file, sheet="Input", range="A4:V3149", col_names=FALSE)
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

  # No longer fixing molecular weight since we do not use it anywhere

  # # Remove artifacts from weight column
  # # If weight is shown in a range, then record only the lower weight
  # res0$`Molecular Weight (MW)` <- gsub("\\-.*", "", res0$`Molecular Weight (MW)`) %>%
  #   gsub("(~)", "", .) %>%
  #   gsub("\\s+$", "", .)
  #
  # # Handle non-numeric values in Molecular Weight Column
  # non_numeric_mw <- grep("(kDa)", res0$`Molecular Weight (MW)`, value = T)
  # non_numeric_val <- gsub("(kDa)", "", non_numeric_mw)
  # non_numeric_val <- gsub("\\s+$", "", non_numeric_val)
  # non_numeric_val <- as.numeric(non_numeric_val)
  # non_numeric_val <- 1000 * non_numeric_val
  #
  # res0$`Molecular Weight (MW)`[res0$`Molecular Weight (MW)` %in% non_numeric_mw] <- non_numeric_val
  # res0$`Molecular Weight (MW)` <- as.numeric(res0$`Molecular Weight (MW)`)

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
    dplyr::rename("BP (°C)"="BP (°C) @ 760 mm Hg unless indicated",
                  "SG"="SG @ 25°C unless indicated") %>%
    dplyr::mutate(
      # Add toxval columns, not replacing original
      name = `Chemical Compound`,
      casrn = `CAS Number (CASRN)`,
      toxval_units = Units,
      study_type = "acute",
      exposure_route = "inhalation",
      toxval_type = toxval_type %>%
        gsub("\\(ppm\\)", "", .),
      # Remove excess whitespace for all character columns
      dplyr::across(where(is.character), ~stringr::str_squish(.))
    ) %>%
    dplyr::select(-`No.`)

  # Fill LEL units
  res$toxval_units[res$toxval_type == "LEL"] = "ppm"
  # Fill in BP values column
  res$`BP (°C)`[is.na(res$`BP (°C)`)] <- 760
  # Fix SP column
  res$`SG`[is.na(res$`SG`)] <- 25

  # Species list to attempt string matches
  species_list <- list(dog=list("dog", "dogs"),
                       human=list("human", "humans", "occupational", "epidemiology", "epidemiological", "epidemiologic"),
                       mouse=list("mouse", "mice", "mouses"),
                       `monkey`=list("nonhuman primate", "monkey", "primate", "monkies", "monkeys",
                                     "rhesus monkeys (macaca mulatta)", "rhesus monkeys",
                                     "cynomolgus monkeys (macaca fascicularis)"),
                       rat=list("rat", "rats"),
                       rabbit = list("rabbit", "rabbits"),
                       `guinea pig` = list("guinea pig", "guinea pigs"),
                       frog = list("frog", "frogs"),
                       hen = list("hen"),
                       hamster = list("hamster", "hamsters")
  ) %>% unlist() %>%
    paste0(collapse="|")

  res <- res %>%
    dplyr::rowwise() %>%
    dplyr::mutate(species = `Source of PACs PAC-1, PAC-2, PAC-3` %>%
                    tolower() %>%
                    # Extract species from `Source of PACsPAC-1, PAC-2, PAC-3`
                    stringr::str_extract_all(., species_list) %>%
                    unlist() %>%
                    unique() %>%
                    paste0(collapse="; "),
                  # Fill in year Revised > Reviewed > Derived
                  year = ifelse(!is.na(`Last Revised`), format(as.Date(`Last Revised`, format="%m/%d/%y"),"%Y"),
                                ifelse(!is.na(`Last Reviewed`), format(as.Date(`Last Reviewed`, format="%m/%d/%y"),"%Y"),
                                       format(as.Date(`Originally Derived`, format="%m/%d/%y"),"%Y")))) %>%
    dplyr::ungroup()

  # Check year
  # res %>% select(`Last Revised`, `Last Reviewed`, `Originally Derived`, year) %>% distinct() %>% View()

  # Fill in missing with "-"
  res$species[res$species %in% c("", "NA")] <- "-"

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




