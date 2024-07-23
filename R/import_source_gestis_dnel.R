#--------------------------------------------------------------------------------------
#' @description Import GESTIS DNEL into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_source_gestis_dnel
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
#'  [str_squish][stringr::str_squish]
#'  [mutate][dplyr::mutate], [across][dplyr::across], [select][dplyr::select], [distinct][dplyr::distinct], [filter][dplyr::filter]
#'  [pivot_longer][tidyr::pivot_longer]
#' @rdname import_source_gestis_dnel
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_squish
#' @importFrom dplyr mutate across select distinct filter
#' @importFrom tidyr pivot_longer
#--------------------------------------------------------------------------------------
import_source_gestis_dnel <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "GESTIS DNEL"
  source_table = "source_gestis_dnel"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2022-11-01")
  dir = paste0(toxval.config()$datapath,"gestis_dnel/gestis_dnel_files/")
  file = paste0(dir,"source_gestis_dnel_raw_nov_2022.xlsx")

  res0 = readxl::read_xlsx(file, sheet="DNEL-list", skip=3, col_names=FALSE)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  #
  # the final file should have column names that include "name" and "casrn"
  # additionally, the names in res need to match names in the source
  # database table. You do not need to add any of the generic columns
  # described in the SOP - they will get added in source_prep_and_load
  #

  # Extract headers
  header <- readxl::read_xlsx(file, sheet="DNEL-list", skip=1, n_max = 2, col_names=FALSE) %>%
    unlist() %>%
    # Remove NA
    .[!is.na(.)] %>%
    unname() %>%
    # Remove excess whitespace
    stringr::str_squish() %>%
    # Remove merged parent headers
    .[!. %in% c("Set", "Number", "DNEL INHALATION [mg/m³]", "Limit values")]

  # Fix header formatting
  header[header == "Data"] = "Data Set"
  header[header %in% c("CAS", "EC")] = paste0(header[header %in% c("CAS", "EC")], " Number")
  header[header %in% c("local", "systemic")] = paste0("DNEL ",
                                                      header[header %in% c("local", "systemic")])
  header[header %in% c("TRGS900", "MAK", "EU")] = paste0("Limit values ",
                                                         header[header %in% c("TRGS900", "MAK", "EU")])

  # Apply header
  names(res0) = header

  # Clean and pivot toxval_type and numeric fields
  res = res0 %>%
    dplyr::mutate(dplyr::across(c("DNEL local", "DNEL systemic"),
                                # Replace , and convert to numeric
                                ~gsub(",", ".", .) %>%
                                  as.numeric() # %>%
                                  # # Set significant figures to 3
                                  # signif(digits=3)
                                )
                  ) %>%

    # Pivot toxval values on DNEL Inhalation
    tidyr::pivot_longer(cols=c("DNEL local", "DNEL systemic"),
                        names_to = "toxval_type",
                        values_to = "toxval_numeric") %>%

    dplyr::mutate(
      # Add toxval columns, not replacing original
      casrn = `CAS Number`,
      toxval_units = "mg/mg3",
      study_type = "long-term occupational",
      species = "human",
      exposure_route = "inhalation",

      source_url = "https://www.dguv.de/ifa/gestis/gestis-dnel-liste/index-2.jsp",
      subsource_url = source_url,

      # Add version date
      source_version_date = src_version_date,

      # Fix symbols in name field
      Name = Name %>%
        # Fix unicode symbols
        fix.replace.unicode() %>%

        # Remove excess whitespace
        stringr::str_squish(),

      # Replace "link" column with hyperlink URL (ZVG append must be length of 6)
      Link = ifelse(!is.na(ZVG) & nchar(ZVG) == 6, paste0("https://gestis-database.dguv.de/data?name=", ZVG),
                    ifelse(!is.na(ZVG), (paste0("https://gestis-database.dguv.de/data?name=",
                                         strrep("0", 6-nchar(ZVG)), ZVG)),
                    NA)),

      # Make ZVG numeric
      ZVG = as.numeric(ZVG),
    ) %>%
    # Remove column causing duplicates
    dplyr::select(-`Data Set`) %>%
    dplyr::distinct() %>%
    # Filter out NA toxval_numeric values
    dplyr::filter(!is.na(toxval_numeric))

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

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




