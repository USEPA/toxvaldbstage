#--------------------------------------------------------------------------------------
#' Import of EPA OW OPP-ALB source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#--------------------------------------------------------------------------------------
import_generic_source <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "EPA OW OPP-ALB"
  source_table = "source_epa_ow_opp_alb"
  dir = paste0(toxval.config()$datapath,"epa_ow_nrwqc_hhc/epa_ow_opp_alb_files/")
  file = paste0(dir,"source_epa_ow_opp_alb_20230228.xlsx")
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
  # Collect vector of toxval columns to pivot out later
  toxval_cols <- c("Freshwater Vertebrate_Acute", "Freshwater Vertebrate_Chronic",
                   "Freshwater Invertebrates_Acute", "Freshwater Invertebrates_Chronic",
                   "Nonvascular Plants", "Vascular Plants",
                   "Office of Water  Aquatic Life Criteria_Acute",
                   "Office of Water  Aquatic Life Criteria_Chronic")
  res0 <- res0 %>%
    # Rename colums
    dplyr::rename(
      name = Pesticide,
      casrn = "CAS number"
    ) %>%
    # Clean up variables
    dplyr::mutate(
      # Replace "NR" in casrn with "-"
      casrn = gsub("^NR$", "-", casrn),
      # Replace all greek letters "µ" with "u"
      across(.fns = ~gsub("µ", "u", .)),
      # Remove ">" and "<" from toxval columns...
      across(toxval_cols, ~gsub(" *[<>] *", "", .)),
      # ...and convert them all to doubles
      across(toxval_cols, ~as.numeric(.))
      ) %>%
    # Pivot out toxvals
    tidyr::pivot_longer(toxval_cols, names_to = "toxval_type",
                        values_to = "toxval_numeric"
    ) %>%
    # Separate "toxval_type" into "toxval_type" and "study_type" by "_"
    tidyr::separate(toxval_type, c("toxval_type", "study_type"),
                    sep = "_", extra = "merge", fill = "right", remove = FALSE
    )

  # Standardize the names
  names(res0) <- names(res0) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  res = source.specific.transformations(res0)


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
