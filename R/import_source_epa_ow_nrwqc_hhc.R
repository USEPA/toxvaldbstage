#--------------------------------------------------------------------------------------
#' @description Import of EPA OW NRWQC-HHC source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
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
#'  \code{\link[dplyr]{rename}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{c("rowwise", "rowwise", "rowwise")}}
#'  \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{reexports}}, \code{\link[tidyr]{separate}}
#'  \code{\link[stringr]{str_detect}}, \code{\link[stringr]{str_trim}}
#' @rdname import_source_epa_ow_nrwqc_hhc
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr rename mutate across rowwise
#' @importFrom tidyr pivot_longer matches separate
#' @importFrom stringr str_detect str_squish
#--------------------------------------------------------------------------------------
import_source_epa_ow_nrwqc_hhc <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "EPA OW NRWQC-HHC"
  source_table = "source_epa_ow_nrwqc_hhc"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2023-02-28")
  dir = paste0(toxval.config()$datapath,"epa_ow_nrwqc_hhc/epa_ow_nrwqc_hhc_files/")
  file = paste0(dir,"source_epa_ow_nrwqc_hhc_20230228.xlsx")
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

  # Define helper function for cleaning up scientific notation
  parse_scientific <- function(s) {
    # Handle scientific notation conversion (either 10's or e notation)
    if(grepl("[Xx]", s) && grepl("^[0-9]+\\.?[0-9]* ?[Xx] ?10\\^.*$", s)){
      mantissa <- as.numeric(gsub(" ?[Xx].*", "", s))
      exponent <- as.numeric(gsub(".*\\^", "", s))
      return(mantissa * 10^exponent)
    } else if(grepl("[eE]", s) && grepl("^(-?[0-9]*)\\.?[0-9]+[eE]?[-\\+]?[0-9]+$", s)){
      mantissa <- as.numeric(gsub(" ?[eE].*", "", s))
      exponent <- as.numeric(gsub(".*?[eE]", "", s))
      return(mantissa * 10^exponent)
    }
    return(suppressWarnings(as.numeric(s)))
  }

  # Load and clean source
  res <- res0 %>%
    # Rename colums
    dplyr::rename(name = Pollutant,
                  casrn = "CAS Number"
                  ) %>%
    # Pivot out toxvals
    # Use matches due to concern for unicode handling of micrograms in field name
    tidyr::pivot_longer(tidyr::matches("Human Health for the consumption of"),
                        names_to = "toxval_type",
                        values_to = "toxval_numeric"
                        ) %>%
    # Separate by "  (", which precedes units
    tidyr::separate(toxval_type, c("toxval_type", "toxval_units"),
                    sep = "  \\(", extra = "merge", fill = "right", remove = FALSE
                    ) %>%
    # Additional separation and renaming of toxval_type
    # Tidy up variables
    dplyr::mutate(
      # Remove trailing paren from toxval_units
      toxval_units = gsub("\\)$", "", toxval_units),
      # Create priority_pollutant column...
      priority_pollutant = ifelse(
        stringr::str_detect(name, "\\(P\\)$"), "yes", "no"),
      # ...and remove the " (P)" tag from name
      name = gsub("\\(P\\)$", "", name) %>%
        stringr::str_squish(),
      # Replace all multiple and/or non-standard dashes with a standard dash
      dplyr::across(.fns = ~gsub("(--)?—", "-", .)),
      # ...and fix unicode symbols
      dplyr::across(tidyselect::where(is.character), fix.replace.unicode)
      ) %>%
    # Make row-by-row adjustments
    dplyr::rowwise() %>%
    # Apply fix.casrn() to non-NA elements of res0$casrn
    dplyr::mutate(casrn = ifelse(is.na(casrn), casrn, fix.casrn(casrn))) %>%
    dplyr::ungroup()

  # fix Asbestos and Methlymercury because they have their toxval_units with their numeric
  res$toxval_numeric[res$name == 'Asbestos' & res$toxval_numeric=='7 million fibers/L'] <- '7000000'
  res$toxval_units[res$name == 'Asbestos' & res$toxval_numeric=='7000000'] <- 'fibers/L'
  res$toxval_numeric[res$name == 'Methylmercury' & res$toxval_numeric=='0.3 mg/kg'] <- '0.3'
  res$toxval_units[res$name == 'Methylmercury' & res$toxval_numeric=='0.3'] <- 'mg/kg'

  # Chemicals with range values
  chem_toxval_numeric_range <- unique(res$name[grepl("[0-9]+-[0-9]+", res$toxval_numeric)])
  # Handle splitting of range groups
  res_range <- res %>%
    dplyr::filter(name %in% chem_toxval_numeric_range)
  # Remove range ranges
  res <- res %>%
    dplyr::filter(!name %in% res_range$name) %>%
    dplyr::mutate(toxval_subtype = NA)

  # Split cancer slope upper and lower
  res_range <- res_range %>%
    tidyr::separate(toxval_numeric,
                    c("cancer slope lower", "cancer slope upper"),
                    sep="-") %>%
    tidyr::pivot_longer(c("cancer slope lower", "cancer slope upper"),
                        names_to = "toxval_subtype",
                        values_to = "toxval_numeric") %>%
    dplyr::mutate(toxval_numeric = as.numeric(toxval_numeric))

  # Recombine range splits
  res <- rbind(res, res_range)

  # make toxval_numeric dash values NA
  res$toxval_numeric[res$toxval_numeric == "-"] <- NA
  # drop toxval_numeric Total values
  res <- res[res$toxval_numeric != "Total",]
  # drop pH row since it is not a chemical
  res <- res[res$name != "pH",]

  # Fix scientific notation issue
  res = res %>%
    # Add rowwise so mutate can vectorize the parse_scientific function
    dplyr::rowwise() %>%
    dplyr::mutate(toxval_numeric = parse_scientific(toxval_numeric)) %>%
    dplyr::ungroup()

  # drop rows with NA for toxval_numeric
  res <- res[!(is.na(res$toxval_numeric)), ]

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

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
                       chem.check.halt=chem.check.halt)
}




