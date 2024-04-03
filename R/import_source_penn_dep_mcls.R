#--------------------------------------------------------------------------------------
#' @description Load Pennsylvania DEP MCLs into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_source_penn_dep
#' @return None; data is loaded to MySQL server
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{distinct}}
#'  \code{\link[stringr]{str_trim}}
#'  \code{\link[tidyr]{unite}}, \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{separate}}
#' @rdname import_source_penn_dep_mcls
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate case_when distinct
#' @importFrom stringr str_squish
#' @importFrom tidyr unite pivot_longer separate
#--------------------------------------------------------------------------------------
import_source_penn_dep_mcls <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "Pennsylvania DEP MCLs"
  source_table = "source_penn_dep_mcls"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2021-11-20")
  dir = paste0(toxval.config()$datapath,"penn_dep_mcls/penn_dep_mcls_files/")
  file = paste0(dir,"PENN_DEP_MCLs_Table 1_20211120.xlsx")
  res0 = readxl::read_xlsx(file, skip = 5, n_max = 369, col_names = FALSE)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  # Hardcode headers due to strange formatting (depth of 3, some cols have no label)
  names(res0) = c("Regulated Substance", "CASRN",
                  "Used Aquifers, TDS <= 2500, Residential - val",
                  "Used Aquifers, TDS <= 2500, Residential - cat",
                  "Used Aquifers, TDS <= 2500, Nonresidential - val",
                  "Used Aquifers, TDS <= 2500, Nonresidential - cat",
                  "Used Aquifers, TDS > 2500, Residential - val",
                  "Used Aquifers, TDS > 2500, Residential - cat",
                  "Used Aquifers, TDS > 2500, Nonresidential - val",
                  "Used Aquifers, TDS > 2500, Nonresidential - cat",
                  "Nonuse Aquifers, Residential - val",
                  "Nonuse Aquifers, Residential - cat",
                  "Nonuse Aquifers, Nonresidential - val",
                  "Nonuse Aquifers, Nonresidential - cat")

  res = res0 %>%
    dplyr::mutate(
      # Add columns as appropriate
      name = stringr::str_squish(`Regulated Substance`),
      toxval_units = "ug/L",
      subsource = "MSC Table 1",
      source_url = "https://www.dep.pa.gov/Business/Land/LandRecycling/Standards-Guidance-Procedures/Pages/Statewide-Health-Standards.aspx",
      risk_assessment_class = "chronic",
      exposure_route = "oral",
      # Reference Jira ticket: TOXVAL-688
      toxval_type = "medium-specific concentration",
      species = "human",
      experimental_record = "No"
    ) %>%

    # Combine values and categories for later transformations
    tidyr::unite(
      "Used Aquifers, TDS <= 2500, Residential",
      c(`Used Aquifers, TDS <= 2500, Residential - val`,
        `Used Aquifers, TDS <= 2500, Residential - cat`),
      sep = " ",
      remove = TRUE
    ) %>%
    tidyr::unite(
      "Used Aquifers, TDS <= 2500, Nonresidential",
      c(`Used Aquifers, TDS <= 2500, Nonresidential - val`,
        `Used Aquifers, TDS <= 2500, Nonresidential - cat`),
      sep = " ",
      remove = TRUE
    ) %>%
    tidyr::unite(
      "Used Aquifers, TDS > 2500, Residential",
      c(`Used Aquifers, TDS > 2500, Residential - val`,
        `Used Aquifers, TDS > 2500, Residential - cat`),
      sep = " ",
      remove = TRUE
    ) %>%
    tidyr::unite(
      "Used Aquifers, TDS > 2500, Nonresidential",
      c(`Used Aquifers, TDS > 2500, Nonresidential - val`,
        `Used Aquifers, TDS > 2500, Nonresidential - cat`),
      sep = " ",
      remove = TRUE
    ) %>%
    tidyr::unite(
      "Nonuse Aquifers, Residential",
      c(`Nonuse Aquifers, Residential - val`,
        `Nonuse Aquifers, Residential - cat`),
      sep = " ",
      remove = TRUE
    ) %>%
    tidyr::unite(
      "Nonuse Aquifers, Nonresidential",
      c(`Nonuse Aquifers, Nonresidential - val`,
        `Nonuse Aquifers, Nonresidential - cat`),
      sep = " ",
      remove = TRUE
    ) %>%

    # Get toxval_subtype and value from source
    tidyr::pivot_longer(cols=c("Used Aquifers, TDS <= 2500, Residential",
                               "Used Aquifers, TDS <= 2500, Nonresidential",
                               "Used Aquifers, TDS > 2500, Residential",
                               "Used Aquifers, TDS > 2500, Nonresidential",
                               "Nonuse Aquifers, Residential",
                               "Nonuse Aquifers, Nonresidential"),
                        names_to = "toxval_subtype",
                        values_to = "source_value") %>%

    # Separate toxval_numeric and category from source_value
    tidyr::separate(
      col = source_value,
      into = c("toxval_numeric", "category"),
      sep = " ",
      remove = FALSE
    ) %>%

    dplyr::mutate(
      # Set toxval_numeric to be numeric
      toxval_numeric = as.numeric(toxval_numeric),

      # Translate category abbreviations (use table definitions for now)
      category = dplyr::case_when(
        category == "M" ~ "Maximum Containment Level",
        category == "H" ~ "Lifetime health advisory level",
        category == "G" ~ "Ingestion",
        category == "N" ~ "Inhalation",
        category == "S" ~ "Aqueous solubility cap",
        TRUE ~ ""
      )
    ) %>%

    # Append category to toxval_subtype
    tidyr::unite(
      "toxval_subtype",
      c(toxval_subtype, category),
      sep = ":",
      remove = TRUE
    ) %>%

    # Uncomment if hardcoded "groundwater" should be added to toxval_subtype
    dplyr::mutate(
      toxval_subtype = paste0("groundwater:", toxval_subtype)
    ) %>%

    dplyr::distinct()

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

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




