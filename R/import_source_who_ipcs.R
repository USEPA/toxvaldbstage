#--------------------------------------------------------------------------------------
#' @description Import of WHO IPCS data
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_who_ipcs
#' @return None. Data is processed into the toxval_source database
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
#' @rdname import_generic_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_squish
#--------------------------------------------------------------------------------------
import_who_ipcs <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "WHO IPCS"
  source_table = "source_who_ipcs"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2019-01-01")
  dir = paste0(toxval.config()$datapath,"who_ipcs/who_ipcs_files/")
  file = paste0(dir,"who_ipcs_2019_raw_combined.xlsx")
  res0_sheets <- readxl::excel_sheets(file) %>%
    .[grepl("^Table", .)]
  table_names <- readxl::read_xlsx(file, sheet="table_names")
  res0_caption_key <- readxl::read_xlsx(file, sheet="caption_key")
  res0 <- lapply(res0_sheets, function(s){
    tmp <- readxl::read_xlsx(file, sheet=s) %>%
      dplyr::mutate(table_name = table_names$name[table_names$table == s])

    # Fix field name issues
    names(tmp) <- names(tmp) %>%
      stringr::str_squish() %>%
      gsub("LD_\\{50\\} mg/kg|LD_\\{50\\} mg/ kg", "LD50_mg/kg", .)

    return(tmp)

  }) %>%
    dplyr::bind_rows()

  res = res0 %>%
    # Rename toxval names
    dplyr::rename(name = `Common name`,
                  cas = `CAS no`,
                  toxval_numeric = `LD50_mg/kg`) %>%
    dplyr::mutate(subsource = "Pesticide Classification 2019",
                  source_url = "https://www.who.int/publications/i/item/9789240005662",
                  toxval_type = "LD50",
                  toxval_units = "mg/kg",
                  risk_assessment_class = "TBD",
                  toxval_numeric_qualifier = "=",
                  exposure_route = "TBD",
                  name = name %>%
                    # Remove [ISO] from chemical name
                    gsub("\\[ISO\\]", "", .)) %>%
    # TODO insert toxval_numeric_qualitifer from toxval_numeric (e.g., >, <)
    # toxval_numeric_qualifier (=, >, <, >=)
    # replace key abbreviated values with text values using dictionary
    dplyr::mutate(dplyr::across(c("Chem type", "Phys state", "Main use"),
                                            # Dictionary for Chem type
                  .fns = ~ dplyr::case_when(`.` == "AS" ~ "Arsenic compound",
                                            `.` == "BP" ~ "Bipyridylium derivative",
                                            `.` == "C" ~ "Carbamate",
                                            `.` == "CO" ~ "Coumarin derivative",
                                            `.` == "CU" ~ "Copper compound",
                                            `.` == "HG" ~ "Mercury compound",
                                            `.` == "NP" ~ "Nitrophenol derivative",
                                            `.` == "OC" ~ "Organochlorine compound",
                                            `.` == "OP" ~ "Organophosphorus compound",
                                            `.` == "OT" ~ "Organotin compound",
                                            `.` == "PAA" ~ "Phenoxyacetic acid derivative",
                                            `.` == "PZ" ~ "Pyrazole",
                                            `.` == "PY" ~ "Pyrethroid",
                                            `.` == "T" ~ "Triazine derivative",
                                            `.` == "TC" ~ "Thiocarbamate",
                                            # Physical state dictionary
                                            `.` == "S" ~ "Active ingredient is Solid, including waxes",
                                            `.` == "L" ~ "Active ingredient is Liquid, including solids with a melting point below 50 Celsius",
                                            `.` == "Oil" ~ "Active ingredient is oily liquid",
                                            `.` == "oil" ~ "Active ingredient is oily liquid",
                                            # Main Use Dictionary
                                            `.` == "AC" ~ "acaricide",
                                            `.` == "AP" ~ "aphicide",
                                            `.` == "B" ~ "bacteriostat (soil)",
                                            `.` == "FM" ~ "fumigant",
                                            `.` == "F" ~ "fungicide, other than for seed treatment",
                                            `.` == "FST" ~ "fungicide, for seed treatment",
                                            `.` == "H" ~ "herbicide",
                                            `.` == "I" ~ "insecticide",
                                            `.` == "IGR" ~ "insect growth regulator",
                                            `.` == "Ix" ~ "ixodicide (for tick control)",
                                            `.` == "L" ~ "larvicide",
                                            `.` == "M" ~ "molluscicide",
                                            `.` == "MT" ~ "miticide",
                                            `.` == "N" ~ "nematocide",
                                            `.` == "O" ~ "other use for plant pathogens",
                                            `.` == "PGR" ~ "plant growth regulator",
                                            `.` == "R" ~ "rodenticide",
                                            `.` == "RP" ~ "repellant",
                                            `.` == "-S" ~ "applied to soil: not used with herbicides or plant growth regulators",
                                            `.` == "SY" ~ "synergist",
                                            TRUE ~ `.`
                                            )
                  )
                  )



  # Fill qualifier NA with "="
  res$toxval_numeric_qualifier[is.na(res$toxval_numeric_qualifier)] = "="

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
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  res = source.specific.transformations(res0)

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




