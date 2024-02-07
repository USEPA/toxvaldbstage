#--------------------------------------------------------------------------------------
#' @title import_source_epa_aegl
#' @description Import EPA AEGL data into toxval_source
#' @param db PARAM_DESCRIPTION
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
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
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{mutate-joins}}
#'  \code{\link[tidyr]{separate_rows}}, \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{reexports}}
#'  \code{\link[stringr]{modifiers}}, \code{\link[stringr]{str_remove}}, \code{\link[stringr]{str_split}}, \code{\link[stringr]{str_trim}}
#' @rdname import_source_epa_aegl
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr filter mutate select distinct right_join
#' @importFrom tidyr separate_rows pivot_longer separate
#' @importFrom stringr regex str_remove str_squish
#' @importFrom lubridate parse_date_time
#--------------------------------------------------------------------------------------
import_source_epa_aegl <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "EPA AEGL"
  source_table = "source_epa_aegl"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2018-07-27")
  dir = paste0(toxval.config()$datapath,"epa_aegl/epa_aegl_files/")
  file = paste0(dir, "automated_epa_aegl_output.xlsx")
  file1 = paste0(dir, "automated_epa_aegl_lel_loa_output.xlsx")

  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  # Handle cases where one row represents multiple substances
  res0_multiple_substance = res0 %>%
    # Extract all entries with multiple CAS numbers
    # dplyr::filter(grepl("and", CASRN)) %>%

    # Extract just jet fuel entries
    # Comment out this line and use the above line to split up HFE-7100 entries as well
    dplyr::filter(grepl("Jet Fuels", Name)) %>%

    # Adjust jet fuel name to match convention (hardcoded)
    dplyr::mutate(
      Name = gsub("Jet Fuels \\(JP-5 and JP-8\\)", "Jet Fuel (JP-5) and Jet Fuel (JP-8)", Name)
    ) %>%

    # Split each row into two (one row per substance)
    tidyr::separate_rows(CASRN, Name, sep = " and ")

  # Replace entries with multiple names/casrn
  # Switch the commented and un-commented filter lines if splitting HFE-7100
  # res0 = res0 %>% dplyr::filter(!grepl("and", CASRN))
  res = res0 %>%
    dplyr::filter(!grepl("Jet Fuels", Name)) %>%
    dplyr::bind_rows(res0_multiple_substance) %>%

    # Extract study duration
    tidyr::pivot_longer(cols = c('10 min', '30 min', '60 min', '4 hr', '8 hr'),
                        names_to = 'study_duration',
                        values_to = 'toxval_numeric'
    ) %>%
    # Split study duration into value and units
    tidyr::separate(study_duration,
                    into = c("study_duration_value", "study_duration_units"),
                    sep = " ") %>%
    # Create toxval_units column
    dplyr::mutate(toxval_units = Units)

  # Handle case of multiple values/units reported
  res_first_units = res %>%
    dplyr::filter(grepl("\\[|\\(", toxval_units)) %>%
    # Extract first values listed
    dplyr::mutate(
      toxval_numeric = toxval_numeric %>%
        gsub("\\(.+\\)|\\[.+\\]", "", .) %>%
        stringr::str_squish(),
      toxval_units = toxval_units %>%
        gsub("\\(.+\\)|\\[.+\\]", "", .) %>%
        stringr::str_squish(),
    )
  res_second_units = res %>%
    dplyr::filter(grepl("\\[|\\(", toxval_units)) %>%
    # Extract values between () and []
    dplyr::mutate(toxval_numeric = stringr::str_extract(string = toxval_numeric,
                                                        pattern = "(?<=\\(|\\[).*(?=\\)|\\])") %>% c(),
                  toxval_units = stringr::str_extract(string = toxval_units,
                                                        pattern = "(?<=\\(|\\[).*(?=\\)|\\])") %>% c()
                  )

  # Recombine
  res = res %>%
    dplyr::filter(!grepl("\\[|\\(", toxval_units)) %>%
    dplyr::bind_rows(res_first_units, res_second_units) %>%
    # Remove non-numeric values in toxval_numeric column
    dplyr::filter(toxval_numeric != "NR" & toxval_numeric != "ND") %>%
    # Add other columns as necessary
    dplyr::mutate(
      toxval_type = AEGL,
      source = "EPA AEGL",
      subsource = "EPA OW",
      source_url = "https://www.epa.gov/aegl/access-acute-exposure-guideline-levels-aegls-values#chemicals",
      risk_assessment_class = "acute",
      exposure_route = "inhalation",

      # Clean toxval_numeric
      toxval_numeric = toxval_numeric %>%
        gsub("\\*|,", "", .) %>%
        stringr::str_squish(),
      # Get year by splitting date and ensuring YYYY format
      # https://stackoverflow.com/questions/13764514/how-to-change-multiple-date-formats-in-same-column
      year = lubridate::parse_date_time(Date,
                                        orders = c('mdy')) %>%
        as.Date() %>%
        format("%Y")
    ) %>%

    dplyr::distinct()

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  ##############################################################################
  #############INCORPORATE LEL/LOA DATA - COMMENT OUT IF NOT NEEDED#############
  ##############################################################################
  res1 = readxl::read_xlsx(file1)

  # Get DF with full LEL/LOA data
  res_lel_loa = res %>%
    # Create res copy without columns to be replaced
    dplyr::select(!c(toxval_type, toxval_numeric, toxval_units, units, aegl,
                     study_duration_units, study_duration_value)) %>%

    # Only one entry needed per chemical
    dplyr::distinct() %>%

    # Join LEL/LOA data with data from res
    dplyr::right_join(res1,
                      by=c("casrn", "name")) %>%

    # Add columns needed for concatenation
    dplyr::mutate(
      units = toxval_units,
      aegl = "-",
      study_duration_units = "-",
      study_duration_value = "-",

      # Clean toxval_numeric field
      toxval_numeric = toxval_numeric %>%
        gsub(",", "", .) %>%
        # Pull lower end of range
        sub('to.*', '', .) %>%
        stringr::str_squish()
    )

  # Concatenate data
  res = dplyr::bind_rows(res, res_lel_loa)
  ##############################################################################
  ###########################END LEL/LOA DATA SECTION###########################
  ##############################################################################
  res = res %>%
    # Remove LEL values
    dplyr::filter(toxval_type!="LEL") %>%

    # Fix toxval_type by translating LOA and adding AEGL information
    dplyr::mutate(
      toxval_type = dplyr::case_when(
        toxval_type == "LOA" ~ "Level of Distinct Odor Awareness (LOA)",
        grepl("AEGL", toxval_type) ~ paste(toxval_type, "-", study_duration_value,
                                           study_duration_units, aegl_status, sep=" ")
      ) %>%
        gsub("Final AEGLs", "(final)", .) %>%
        gsub("Holding AEGLs", "(holding)", .) %>%
        gsub("Interim AEGLs", "(interim)", .) %>%
        gsub("Proposed AEGLs", "(proposed)", .),

      # Set toxval_numeric to numeric type
      toxval_numeric = as.numeric(toxval_numeric)
    )

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




