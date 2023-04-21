#--------------------------------------------------------------------------------------
#' @#' Import of EPA OW NPDWR source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
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
#'  \code{\link[dplyr]{rename}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}}
#'  \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{separate}}
#'  \code{\link[stringr]{str_trim}}
#' @rdname import_source_epa_ow_npdwr
#' @export 
#' @importFrom readxl read_xlsx
#' @importFrom dplyr rename mutate across
#' @importFrom tidyr pivot_longer separate
#' @importFrom stringr str_squish
#--------------------------------------------------------------------------------------
import_source_epa_ow_npdwr <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "EPA OW NPDWR"
  source_table = "source_epa_ow_npdwr"
  dir = paste0(toxval.config()$datapath,"epa_ow_npdwr/epa_ow_npdwr_files/")
  file = paste0(dir,"epa_ow_npdwr_raw.xlsx")
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
  res0 <- res0 %>%
    dplyr::rename(name = Contaminant) %>%
    # Pivot MCLG and MCL columns to toxval_type and _numeric
    tidyr::pivot_longer(c("MCLG (mg/L)", "MCL or TT (mg/L)"),
                        names_to = "toxval_type",
                        values_to = "toxval_numeric") %>%
    # Split up units
    tidyr::separate(toxval_type, c("toxval_type", "toxval_units"), sep = "\\s\\(",
                    extra = "merge", fill = "right", remove = FALSE) %>%
    # Remove closing parentheses from units
    dplyr::mutate(toxval_units = gsub("\\)$", "", toxval_units),
                  # Clean up some of the low-hanging issues in toxval_numeric
                  toxval_numeric = gsub(" as of \\d\\d/\\d\\d/\\d\\d$|^none -+ ",
                                        "", toxval_numeric),
                  toxval_numeric = gsub("zero", 0, toxval_numeric),
                  toxval_numeric = gsub("(^--> )?n/a$", "-", toxval_numeric),
                  toxval_numeric = gsub(" MFL$", " million fibers per liter", toxval_numeric)) %>%
    dplyr::mutate(dplyr::across(c("toxval_type", "toxval_numeric", "toxval_units"),
                         ~stringr::str_squish(.)))

  # TODO Handle toxval_numeric case of 15 picocuries per Liter (pCi/L) or 5 pCi/L
  # TODO Handle toxval_numeric case of 30 ug/L
  # TODO Handle toxval_numeric case of 4 millirems per year
  # TODO Handle toxval_numeric case of 7 million fibers per liter or 7 million fibers per liter (MFL)
  # TODO Handle toxval_numeric case of MRDL=
  # TODO Handle toxval_numeric case of TT; Action Level=0.015 or TT; Action Level=1.3
  # TODO Handle toxval_numeric case of TT (TBD if keeping or filtering out)

  # Standardize the names
  names(res0) <- names(res0) %>%
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




