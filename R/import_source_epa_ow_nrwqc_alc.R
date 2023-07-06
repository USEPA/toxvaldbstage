#--------------------------------------------------------------------------------------
#' @description Import of EPA OW NRWQC-ALC source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
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
#'  \code{\link[stringr]{str_trim}}
#'  \code{\link[dplyr]{rename}}, \code{\link[dplyr]{c("rowwise", "rowwise", "rowwise")}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{case_when}}
#'  \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{reexports}}, \code{\link[tidyr]{separate_rows}}
#' @rdname import_source_epa_ow_nrwqc_alc
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_squish
#' @importFrom dplyr rename rowwise mutate ungroup across case_when
#' @importFrom tidyr pivot_longer separate matches separate_rows
#--------------------------------------------------------------------------------------
import_source_epa_ow_nrwqc_alc <- function(db,chem.check.halt=F) {
  printCurrentFunction(db)
  source = "EPA OW NRWQC-ALC"
  source_table = "source_epa_ow_nrwqc_alc"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2023-02-28")
  dir = paste0(toxval.config()$datapath,"epa_ow_nrwqc_alc/epa_ow_nrwqc_alc_files/")
  files = list.files(dir)
  res0 = readxl::read_xlsx(paste0(dir,files))
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  #
  # the final file should have column names that include "name" and "casrn"
  # additionally, the names in res need to match names in the source
  # database table. You do not need to add any of the generic columns
  # described in the SOP - they will get added in source_prep_and_load
  #
  # Standardize the names
  names(res0) <- names(res0) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .)

  res <- res0 %>%
    # Renaming columns
    dplyr::rename(name="Pollutant_(P_=_Priority_Pollutant)",
                  casrn="CAS_Number") %>%
    # Remove non-chemicals, like pH
    filter(!name %in% c("pH")) %>%
    dplyr::rowwise() %>%
    # Making priority_pollutant column based on (P) in name column
    dplyr::mutate(priority_pollutant = ifelse(endsWith(name, "(P)"), "yes", "no")) %>%
    dplyr::ungroup() %>%
    # wide to long based on toxval_type
    tidyr::pivot_longer(
      cols = tidyr::starts_with(c("Freshwater", "Saltwater")),
      names_to= "toxval_type",
      values_to= "toxval_numeric") %>%
    # splitting toxval_type into other toxval columns
    tidyr::separate(toxval_type, c("media", "toxval_type", "study_type", "toxval_units"),
                    sep="_", fill="right", remove=TRUE) %>%
    dplyr::mutate(
      # replacing greek letters
      # toxval_units = gsub("?", "u", toxval_units),
      dplyr::across(c("name", "toxval_units"), ~fix.greek.symbols(.)),
      # getting rid of units still in toxval_numeric column
      toxval_numeric = gsub("ug/L", "", toxval_numeric),
      # getting rid of parenthesis around values in certain columns
      toxval_units = gsub("[()]", "", toxval_units),
      study_type = gsub("[()]", "", study_type),
      # removing numbers from toxval_type
      toxval_type = gsub("[[:digit:]]+", "", toxval_type)) %>%
    # replacing multiple dashes with single dash for empty columns
    dplyr::mutate(dplyr::across(c("name","casrn","Publication_Year","toxval_numeric"),
                                ~ gsub(paste0(c("\u2013", "\u2014", "---", "--"), collapse="|"), "-", .)
                                ),
                  # getting rid of (P) at end of 'name' column values
                  name = gsub("\\(P[)]$", "", name) %>%
                    # Remove asterisk
                    gsub("*", "", ., fixed=TRUE),
                  dplyr::across(c("name", "casrn", "toxval_numeric"), ~stringr::str_squish(.)),
                  toxval_numeric = suppressWarnings(as.numeric(toxval_numeric))
    ) %>%
    # drop rows with NA for toxval_numeric
    filter(!is.na(toxval_numeric)) %>%
    # Split CASRN lists into unique rows
    # https://stackoverflow.com/questions/15347282/split-delimited-strings-in-a-column-and-insert-as-new-rows
    tidyr::separate_rows(casrn, sep=" ")

  # Fix publication year split by media for select chemicals
  res$Publication_Year[res$name == "Ammonia" & res$media == "Freshwater"] = 2013
  res$Publication_Year[res$name == "Ammonia" & res$media == "Saltwater"] = 1989
  res$Publication_Year[res$name == "Selenium" & res$media == "Freshwater"] = 2016
  res$Publication_Year[res$name == "Selenium" & res$media == "Saltwater"] = 1999

  # make column names lowercase now (didn't earlier to keep liter in toxval_units uppercase)
  names(res) <- tolower(names(res))

  res <- res %>%
    # Make row-by-row adjustments
    dplyr::rowwise() %>%
    # Apply fix.casrn() to non-NA elements of res0$casrn
    dplyr::mutate(casrn = ifelse(is.na(casrn), casrn, fix.casrn(casrn)))

  # Add version date. Can be converted to a mutate statement as needed
  res$source_version_date <- src_version_date
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db=db,
                       source=source,
                       table=source_table,
                       res=res0,
                       do.reset=do.reset,
                       do.insert=do.insert,
                       chem.check.halt=chem.check.halt)
}
