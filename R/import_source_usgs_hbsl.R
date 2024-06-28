#--------------------------------------------------------------------------------------
#' @description Import USGS HBSL data into toxval_source
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
#' @importFrom dplyr mutate na_if select everything all_of
#' @importFrom tidyr pivot_longer separate
#--------------------------------------------------------------------------------------
import_source_usgs_hbsl <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "USGS HBSL"
  source_table = "source_usgs_hbsl"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2018-05-31")
  dir = paste0(toxval.config()$datapath,"usgs_hbsl/usgs_hbsl_files/")
  file = paste0(dir,"HBSL-data_20180531.xlsx")
  res0 = readxl::read_xlsx(file, skip = 1)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  #
  # the final file should have column names that include "name" and "casrn"
  # additionally, the names in res need to match names in the source
  # database table. You do not need to add any of the generic columns
  # described in the SOP - they will get added in source_prep_and_load
  #
  # names(res1) <- sub("\\s*\\(.*\\)","", names(res1))
  res0 <- res0 %>%
    dplyr::mutate(name = `Chemical Name` %>%
                    fix.replace.unicode(),
                  casrn = ifelse(grepl("/", `CAS Registry Number`),
                                 sapply(strsplit(as.character(`CAS Registry Number`), "/"),
                                        function(x){
                                          year <- x[3]
                                          month <- sprintf("%02d", as.integer(x[1]))
                                          day <- x[2]
                                          paste(year, month, day, sep = '-')
                                        }),
                                 `CAS Registry Number`
                  )
                  )

  # Pivot fields
  res = res0 %>%
    tidyr::pivot_longer(cols=c(`MCL (micrograms/L)`,
                               `Chronic Noncancer HHBP (micrograms/L)`,
                               `Carcinogenic HHBP (micrograms/L)`, `Noncancer HBSL (micrograms/L)`,
                               `Cancer HBSL (micrograms/L)`),
                        names_to = "toxval_type",
                        values_to = "toxval_numeric",
                        values_transform = as.character,
                        # Remove NA values
                        values_drop_na = TRUE
                        ) %>%
    tidyr::separate(col="toxval_type",
                    into = c("toxval_type", "toxval_units"), sep = "\\(") %>%
    dplyr::mutate(
                  # Uncomment if splitting toxval_type into toxval_subtype
                  toxval_subtype = toxval_type %>%
                    gsub("HBSL|HHBP|MCL", "", .) %>%
                    gsub("Carcinogenic", "Cancer", .) %>%
                    stringr::str_squish() %>%
                    dplyr::na_if(""),
                  toxval_type = toxval_type %>%
                    gsub("Noncancer|Chronic|Cancer|Carcinogenic", "", .) %>%
                    stringr::str_squish(),
                  toxval_units = toxval_units %>%
                    # Remove training parentheses
                    gsub("\\)", "", .),
                  source = "USGS HBSL",
                  subsource = "Water Quality Data",
                  source_url = "https://water.usgs.gov/water-resources/hbsl",
                  subsource_url = source_url,
                  risk_assessment_class = "chronic",
                  exposure_route = "oral",
                  # Add version date. Can be converted to a mutate statement as needed
                  source_version_date = src_version_date,
                  long_ref = paste0("Norman, J.E., Toccalino, P.L., Morman, S.A., 2018, ",
                                    "Health-Based Screening Levels for evaluating water-quality data (2d ed.). ",
                                    "U.S. Geological Survey web page, accessible at ",
                                    "https://water.usgs.gov/water-resources/hbsl/, doi:10.5066/F71C1TWP."))


  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"

  # Reorder hashing cols to end of dataframe
  res = res %>%
    dplyr::select(dplyr::everything(), dplyr::all_of(toxval.config()$hashing_cols))
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




