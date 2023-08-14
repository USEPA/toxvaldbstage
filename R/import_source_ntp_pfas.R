#--------------------------------------------------------------------------------------
#' @description A functio for adding source NTP PFAS data to toxval_source
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
#'  \code{\link[stringr]{str_trim}}
#' @rdname import_generic_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_squish
#--------------------------------------------------------------------------------------
import_source_ntp_pfas <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "NTP PFAS"
  source_table = "source_ntp_pfas"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2023-07-14")
  dir = paste0(toxval.config()$datapath,"ntp_pfas/ntp_pfas_files/")

  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  #
  # the final file should have column names that include "name" and "casrn"
  # additionally, the names in res need to match names in the source
  # database table. You do not need to add any of the generic columns
  # described in the SOP - they will get added in source_prep_and_load
  #

  # Load all files and combine
  res0 <- lapply(list.files(dir, full.names = TRUE, pattern = ".xlsx"), function(f){
    readxl::read_xlsx(f) %>%
      # Rename chemical name field
      dplyr::rename(name = chemical_name) %>%
      # Set dose to character across files
      dplyr::mutate(dplyr::across(c("dose_value"), ~as.character(.))) %>%
      # Pivot except for curated fields
      tidyr::pivot_longer(-c("name", "casrn", "sex", "species", "strain",
                             "administration_route", "study_duration_value",
                             "study_duration_units", "dose_vehicle", "dose_value", "dose_units",
                             "table_title", "ntp_study_identifier", "url"),
                          names_to = "field_name",
                          values_to = "field_value")
  }) %>%
    dplyr::bind_rows()

  res = res0 %>%
    # TODO Split field_name fields into appropriate fields by "-"
    # May need to group separate and recombine dataframes...
    tidyr::separate(field_name, into = c("critical_effect", "media"), sep = "-",
                    extra="merge", fill="right") %>%
    # Split dose notes
    tidyr::separate(dose_value, into = c("dose_value", "notes"), sep = "-",
                    extra="merge", fill="right") %>%
    # Remove excess whitespace from splits
    dplyr::mutate(dplyr::across(c("critical_effect", "media",
                                  "dose_value", "notes"), ~stringr::str_squish(.)),
                  dose_value = as.numeric(dose_value))

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




