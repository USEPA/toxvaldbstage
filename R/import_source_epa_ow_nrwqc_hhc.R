#--------------------------------------------------------------------------------------
#' Import of EPA OW NRWQC-HHC source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#--------------------------------------------------------------------------------------
import_generic_source <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "EPA OW NRWQC-HHC"
  source_table = "source_epa_ow_nrwqc_hhc"
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
  # Load and clean source
  res <- res0 %>%
    # Rename colums
    dplyr::rename(name = Pollutant,
                  casrn = "CAS Number"
                  ) %>%
    # Pivot out toxvals
    # Use matches due to concern for unicode handling of micrograms in field name
    tidyr::pivot_longer(matches("Human Health for the consumption of"),
                        names_to = "toxval_type",
                        values_to = "toxval_numeric"
                        ) %>%
    # Separate by "  (", which precedes units
    tidyr::separate(toxval_type, c("toxval_type", "toxval_units"),
                    sep = "  \\(", extra = "merge", fill = "right", remove = FALSE
                    ) %>%
    # TODO: additional separation and renaming of toxval_type TBD
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
      across(.fns = ~gsub("(--)?—", "-", .)),
      # ...and replace all greek letters "µ" with "u"
      across(.fns = ~gsub("µ", "u", .))
      ) %>%
    # Make row-by-row adjustments
    dplyr::rowwise() %>%
    # Apply fix.casrn() to non-NA elements of res0$casrn
    dplyr::mutate(casrn = ifelse(is.na(casrn), casrn, fix.casrn(casrn)))

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

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




