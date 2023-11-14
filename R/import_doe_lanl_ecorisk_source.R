#--------------------------------------------------------------------------------------
#' @description Import doe_lanl_ecorisk source into ToxVal
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_doe_lanl_ecorisk_source
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
#'  \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_trim}}
#'  \code{\link[tidyr]{pivot_longer}}
#' @rdname import_generic_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_squish str_trim
#' @importFrom tidyr pivot_longer
#' ---------------------------------------------------
import_doe_lanl_ecorisk_source <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "doe_lanl_ecorisk"
  source_table = "source_{source}"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2022-09-01")
  dir = paste0(toxval.config()$datapath,"{source}/{source}_files/")
  
  file = paste0(dir,"ESLs_R4.3.xlsx")
  
  res = readxl::read_xlsx(file)
  #####################################################################
  cat("Build original_doe_lanl_ecorisk_table\n")
  
  # Handle bad codes
  bad.codes = c("AL","SB","AS","BA","BE",
                "B","CD","CL(-1)","CR","CR(+6)","CO","CU",
                "CN(-1)","F(-1)","FE","PB","LI","MN","HGI",
                "HGM","MO","NI","ClO4(-1)","SE","AG","SR",
                "TL","TI","U","V","ZN",
                "AM-241","CS-134","CS-137/ BA-137","CO-60","EU-152",
                "PB-210","NP-237","PU-238","PU-239/240","PU-241","RA-226","RA-228",
                "NA-22","SR-90/ Y-90","TH-228","TH-229","TH-230","TH-232","H-3",
                "U-233","U-234","U-235","U-236","U-238")
  res = res[!generics::is.element(res$`Analyte Code`, bad.codes),]
  
  # Rename columns (column names are mixed up)
  nlist = c("Analyte Category","Analyte Group","Analyte Name","Analyte Code","ESL Medium","ESL Receptor","No Effect ESL",
            "Low Effect ESL","Units","Minimum ESL","ESL ID" )
  names(res) = nlist
  
  # Add new columns as needed but retain original columns
  res$`name` <- res$`Analyte Name`
  res$`casrn` <- res$`Analyte Code`
  res$`toxval_units` <- res$`Units`
  res$`species` <- res$`ESL Receptor`
  
  # Pivot to add toxval_type and toxval_numeric
  res <- res %>%
    pivot_longer(cols = c('No Effect ESL',
                          'Low Effect ESL'),
                 names_to = 'toxval_type',
                 values_to = 'toxval_numeric'
    )
  
  # Add empty columns
  res$`toxval_subtype` <- NA
  res$`toxval_numeric_qualifier` <- NA
  res$`study_type` <- NA
  res$`study_duration_value` <- NA
  res$`study_duration_units` <- NA
  res$`strain` <- NA
  res$`sex` <- NA
  res$`critical_effect` <- NA
  res$`population` <- NA
  res$`exposure_route` <- NA
  res$`exposure_method` <- NA
  res$`exposure_form` <- NA
  res$`lifestage` <- NA
  res$`generation` <- NA
  res$`year` <- NA
  
  # Code pulled into import from load file (toxval.load.doe.lanl.ecorisk.R, 51-62)
  res$species <-  gsub("(.*)(\\([^\\(].*)", "\\1", res$species)
  res$species = str_trim(res$species)
  # End load file code
  #####################################################################
  #
  # the final file should have column names that include "name" and "casrn"
  # additionally, the names in res need to match names in the source
  # database table. You do not need to add any of the generic columns
  # described in the SOP - they will get added in source_prep_and_load
  #

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




