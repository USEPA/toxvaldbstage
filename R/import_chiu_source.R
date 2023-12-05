#--------------------------------------------------------------------------------------
#' @#' Load chiu Source into dev_toxval_source_v3.
#' Data from the Chiu et al. paper on RfD values
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile The input file ./chiu/chiu_files/Full_RfD_databaseQAed-FINAL.xlsx
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn, #' stop to look at the results in indir/chemcheck.xlsx
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
#'  \code{\link[dplyr]{select}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{distinct}}
#'  \code{\link[tidyr]{separate_wider_delim}}, \code{\link[tidyr]{drop_na}}
#'  \code{\link[stringr]{str_remove}}, \code{\link[stringr]{modifiers}}, \code{\link[stringr]{str_trim}}
#' @rdname import_chiu_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr select mutate distinct
#' @importFrom tidyr separate_wider_delim drop_na
#' @importFrom stringr str_remove fixed str_squish
#--------------------------------------------------------------------------------------
import_chiu_source <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "Chiu"
  source_table = "source_chiu"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2018-06-28")
  dir = paste0(toxval.config()$datapath,"chiu/chiu_files/")
  file = paste0(dir,"Full_RfD_databaseQAed-FINAL.xlsx")
  res0 = readxl::read_xlsx(file, sheet="Full_RfD_database_QAed")
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  # Select only non-POD data
  res1 <- res0 %>% dplyr::select(!c(POD.type, numPOD, strUnitsPOD))

  # Select only POD data
  res2 <- res0 %>% dplyr::select(!c(Type, numValue))

  # For non-POD entries, extract toxval_type, toxval_units, and toxval_numeric
  res1 <- res1 %>%
    # Extract toxval_type and toxval_units
    tidyr::separate_wider_delim(Type,
                                delim = " (",
                                names = c("toxval_type", "toxval_units"),
                                too_few = "align_start") %>%

    # Finalize values
    dplyr::mutate(
      # Remove closing parentheses from toxval_units
      toxval_units = stringr::str_remove(toxval_units, stringr::fixed(")")),

      # Extract toxval_numeric
      toxval_numeric = as.numeric(numValue),

      # Add in columns to enable rbind
      POD.type = "-",
      numPOD = "-",
      strUnitsPOD = "-"
    )

  # For non-POD entries, extract toxval_type, toxval_units, and toxval_numeric
  res2 <- res2 %>%
    dplyr::mutate(
      toxval_type = POD.type,
      toxval_units = strUnitsPOD,
      toxval_numeric = as.numeric(numPOD),

      # Add in columns to enable rbind
      numValue = "-"
    )

  res = rbind(res1,res2)

  # Mutate columns in combined data as needed
  res <- res %>%
    dplyr::mutate(
      # Updated from deprecated import script
      name = strName,
      casrn = sapply(strCAS, FUN=fix.casrn),
      subsource = Source,
      record_url = strHyperlink,
      long_ref = strReference,
      critical_effect = strCriticalEffect,
      year = as.numeric(strDateAssessed),
      study_duration_value = strDuration,
      study_duration_units = tolower(`Duration type`),
      uncertainty_factor = numUF,
      ufa = numUFa,
      ufh = numUFh,
      ufs = numUFs,
      ufl = numUFl,
      ufd = numUFd,
      ufother = numUFother,

      # Moved from deprecated load script
      exposure_method = "-",
      risk_assessment_class = "chronic",
      study_type = "chronic",
      toxval_units = "mg/kg-day",
      exposure_route = ifelse(grepl("Oral", Route), "oral",
                              ifelse(Route == "Other", "other", "-")),
      exposure_method = ifelse(grepl("other", tolower(Route)), "other",
                               ifelse(grepl("diet", Route), "diet",
                                      ifelse(grepl("drinking water", Route), "drinking water",
                                             ifelse(grepl("gavage", Route), "gavage", "-")))),
      critical_effect = enc2utf8(critical_effect),
      critical_effect = gsub("\\[", "(", critical_effect),
      critical_effect = gsub("\\]", ")", critical_effect)
    ) %>%

    dplyr::distinct() %>%

    tidyr::drop_na(toxval_numeric)

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  # Final changes after standardizing names, avoids duplicate cols
  res <- res %>%
    dplyr::mutate(
      species = tolower(species),
      sex = gsub("BOTH", "M/F", toupper(tblorgan_strsex))
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




