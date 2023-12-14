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
#--------------------------------------------------------------------------------------
import_source_usgs_hbsl <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "USGS HBSL"
  source_table = "source_usgs_hbsl"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2018-05-31")
  dir = paste0(toxval.config()$datapath,"usgs_hbsl/usgs_hbsl_files/")
  file = paste0(dir,"HBSL-data_20180531.xlsx")
  res1 = readxl::read_xlsx(file, skip = 1)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  #
  # the final file should have column names that include "name" and "casrn"
  # additionally, the names in res need to match names in the source
  # database table. You do not need to add any of the generic columns
  # described in the SOP - they will get added in source_prep_and_load
  #
  names(res1) <- sub("\\s*\\(.*\\)","", names(res1))
  res0 <- res1 %>%
    dplyr::mutate(name = `Chemical Name` %>%
                    # Replace Prime symbol
                    gsub("\u00b4|<U+00B4>|\u2018|<U+2018>|\u0092|<U+0092>|\u2019|<U+2019>|\u2032", "'", .) %>%
                    fix.greek.symbols(),
                  # Fix several cases where casrn gets reformatted as mm/dd/yyyy
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

  # Split rows where multiple columns have numeric values before derivation below
  res0 <- res0 %>%
    dplyr::bind_rows(
      res0 %>%
        dplyr::filter(!is.na(`Noncancer HBSL`) & !is.na(`Cancer HBSL`)) %>%
        dplyr::mutate(
          `Cancer HBSL` = NA_character_
        ),
      res0 %>%
        dplyr::filter(!is.na(`Noncancer HBSL`) & !is.na(`Cancer HBSL`)) %>%
        dplyr::mutate(
          `Noncancer HBSL` = NA_real_
        ),
      res0 %>%
        dplyr::filter(!is.na(`Chronic Noncancer HHBP`) & !is.na(`Carcinogenic HHBP`)) %>%
        dplyr::mutate(
          `Chronic Noncancer HHBP` = NA_real_
        ),
      res0 %>%
        dplyr::filter(!is.na(`Chronic Noncancer HHBP`) & !is.na(`Carcinogenic HHBP`)) %>%
        dplyr::mutate(
          `Carcinogenic HHBP` = NA_character_
        )
    ) %>%
    select(names(res0))

  # Remove original, non-split rows
  res0 <- res0 %>%
    dplyr::filter(!((!is.na(`Noncancer HBSL`) & !is.na(`Cancer HBSL`)) |
             (!is.na(`Chronic Noncancer HHBP`) & !is.na(`Carcinogenic HHBP`))))

  # Handle hyphenated numeric values
  res0 <- res0 %>%
    dplyr::mutate(
      final_cancer_hbsl = as.numeric(str_extract(`Cancer HBSL`, "(?<=-)[0-9.]+")),
      final_carcinogenic_hhbp = as.numeric(str_extract(`Carcinogenic HHBP`, "(?<=-)[0-9.]+"))
    )

  # Set toxval fields based on which column the toxval_numeric is coming from
  res0 <- res0 %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      toxval_type = case_when(
        !is.na(`MCL`) ~ "MCL",
        !is.na(`Chronic Noncancer HHBP`) ~ "HHBP",
        !is.na(`Carcinogenic HHBP`) ~ "HHBP carcinogenicity",
        !is.na(`Noncancer HBSL`) ~ "HBSL",
        !is.na(final_cancer_hbsl) ~ "HBSL",
        TRUE ~ NA_character_
      ),
      toxval_subtype = case_when(
        !is.na(`MCL`) ~ "-",
        !is.na(`Chronic Noncancer HHBP`) ~ "Chronic, non_cancer",
        !is.na(`Carcinogenic HHBP`) ~ "Cancer",
        !is.na(`Noncancer HBSL`) ~ "Noncancer",
        !is.na(final_cancer_hbsl) ~ "Cancer",
        TRUE ~ NA_character_
      ),
      toxval_numeric = case_when(
        !is.na(`MCL`) ~ `MCL`,
        !is.na(`Chronic Noncancer HHBP`) ~ `Chronic Noncancer HHBP`,
        !is.na(final_carcinogenic_hhbp) ~ final_carcinogenic_hhbp,
        !is.na(`Noncancer HBSL`) ~ `Noncancer HBSL`,
        !is.na(final_cancer_hbsl) ~ final_cancer_hbsl,
        TRUE ~ NA_real_
      )) %>%
    dplyr::mutate(
      toxval_units = ifelse(!is.na(toxval_numeric),"microgram/L", NA_character_)
    )

  # Standardize the names
  names(res0) <- names(res0) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  # Fill blank hashing cols
  res0[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res0)]] <- "-"

  # Add version date. Can be converted to a mutate statement as needed
  res0$source_version_date <- src_version_date
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




