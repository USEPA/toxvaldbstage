
#--------------------------------------------------------------------------------------
#' @description Import of IRIS 2023-05-09 source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_source_iris
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
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{c("rowwise", "rowwise", "rowwise")}}, \code{\link[dplyr]{distinct}}
#'  \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{reexports}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{replace_na}}
#'  \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_replace}}, \code{\link[stringr]{str_extract}}
#' @rdname import_source_pprtv_cphea
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate filter select across rename rowwise distinct
#' @importFrom tidyr pivot_longer all_of separate replace_na
#' @importFrom stringr str_squish str_replace_all str_extract
#--------------------------------------------------------------------------------------
import_source_iris <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "IRIS"
  source_table = "source_iris"
  src_version_date = as.Date("2023-05-09")
  #####################################################################

  # Define helper function for cleaning up scientific notation
  iris_parse_scientific <- function(s) {
    # Handle scientific notation conversion (either 10's or e notation)
    if(grepl("[Xx]", s) && grepl("^[0-9]+\\.?[0-9]* ?[Xx] ?10 ?-?[0-9]+", s)){
      mantissa <- as.numeric(gsub(" ?[Xx].*", "", s))
      exponent <- as.numeric(gsub('.*?[Xx] 10', "", s))
      return(mantissa * 10^exponent)
    } else if(grepl("[eE]", s) && grepl("^[0-9]+\\.?[0-9]* ?[Ee] ?10 ?-?[0-9]+", s)){
      mantissa <- as.numeric(gsub(" ?[eE].*", "", s))
      exponent <- as.numeric(gsub(".*?[eE]", "", s))
      return(mantissa * 10^exponent)
    }
    return(suppressWarnings(as.numeric(s)))
  }

  # Pull list of iris files to load
  iris_files <- paste0(toxval.config()$datapath,"iris/iris_files/") %>%
    list.files(full.names = TRUE) %>%
    # No longer using WOE files
    .[!grepl("woe", ., ignore.case = TRUE)]

  # Load iris files in named list
  iris_data <- lapply(iris_files, function(f) {
    readxl::read_xlsx(f) %>%
      dplyr::distinct()
  }) %T>%
    { names(.) <- basename(iris_files) %>%
      # Remove date stem for processing
      gsub("_20230509", "", .)}

  ####################################################
  ### Transform chemicals_details.xlsx
  ####################################################
  # Rename fields
  iris_data$chemicals_details.xlsx <- iris_data$chemicals_details.xlsx %>%
    dplyr::rename(name=`CHEMICAL NAME`, casrn=CASRN, route=`EXPOSURE ROUTE`,
                  type=`ASSESSMENT TYPE`, critical_effect_tumor_type=`CRIT EFFECT TUMOR TYPE`,
                  toxval_type=`TOXICITY VALUE TYPE`, toxval_numeric=`TOXICITY VALUE`)

  # Split toxval_numeric into value/units
  tmp <- iris_data$chemicals_details.xlsx %>%
    # Add tmp_id field to help filter out matched cases
    dplyr::mutate(tmp_id = 1:n())
  # Process exponent notation into numeric
  tmp_exp <- tmp %>%
    dplyr::filter(grepl("^[0-9]+\\.?[0-9]* ?[Xx] ?10 ?-?[0-9]+", toxval_numeric)) %>%
    dplyr::mutate(toxval_units = toxval_numeric %>%
                    gsub("^[0-9]+\\.?[0-9]* ?[Xx] ?10 ?-?[0-9]+", "", .) %>%
                    stringr::str_squish()) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(toxval_numeric = toxval_numeric %>%
                    gsub(toxval_units, "", .) %>%
                    iris_parse_scientific())

  if(any(is.na(tmp_exp$toxval_numeric))){
    stop("Error parsing toxval_numeric to exponent form")
  }
  # Process regular numbers into numeric
  tmp_num <- tmp %>%
    dplyr::filter(!tmp_id %in% tmp_exp$tmp_id) %>%
    tidyr::separate(col = "toxval_numeric",
                    into = c("toxval_numeric", "toxval_units"),
                    sep=" ", extra = "merge") %>%
    dplyr::mutate(toxval_numeric = as.numeric(toxval_numeric))
  # Recombine and remove tmp_id field
  iris_data$chemicals_details.xlsx <- rbind(tmp_exp, tmp_num) %>%
    dplyr::select(-tmp_id)

  ####################################################
  ### Transform Chemical_Rev_History.xlsx
  ####################################################
  # Add IRIS_ stem and replace blank spaces with "_" to field names
  names(iris_data$Chemical_Rev_History.xlsx)[
    !grepl("CHEMICAL|URL|CASRN", names(iris_data$Chemical_Rev_History.xlsx))] <- names(iris_data$Chemical_Rev_History.xlsx)[
      !grepl("CHEMICAL|URL|CASRN", names(iris_data$Chemical_Rev_History.xlsx))] %>%
    paste0("IRIS_", .) %>%
    gsub(" ", "_", .) %>%
    tolower()
  # Convert revision history to JSON format
  iris_data$Chemical_Rev_History.xlsx <- iris_data$Chemical_Rev_History.xlsx %>%
    # Transform record columns into JSON
    dplyr::mutate(iris_revision_history = convert.fields.to.json(dplyr::select(.,
                                                                               -tidyr::any_of(c("CHEMICAL ID", "CHEMICAL NAME", "CASRN", "URL"))))) %>%
    dplyr::select(`CHEMICAL ID`, `CHEMICAL NAME`, CASRN, URL, iris_revision_history) %>%
    dplyr::group_by(`CHEMICAL ID`) %>%
    # Combine chemical revision histories into JSON list
    dplyr::mutate(iris_revision_history = gsub("\\[", "", iris_revision_history) %>%
                    gsub("\\]", "", .) %>%
                    paste0(., collapse = ", ") %>%
                    paste0("[", ., "]")) %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::rowwise() %>%
    # Add "archived" flag
    dplyr::mutate(archived = ifelse(grepl("archive", iris_revision_history, ignore.case = TRUE),
                                    1, 0))

  ####################################################
  ### Transform WOE_Details.xlsx - NO LONGER USING
  ####################################################
  # Fix WOE field names
  # names(iris_data$WOE_Details.xlsx)[
  #   !grepl("CHEMICAL|URL|CASRN", names(iris_data$WOE_Details.xlsx))] <- names(iris_data$WOE_Details.xlsx)[
  #     !grepl("CHEMICAL|URL|CASRN", names(iris_data$WOE_Details.xlsx))] %>%
  #   gsub(" ", "_", .)
  # iris_data$WOE_Details.xlsx <- iris_data$WOE_Details.xlsx %>%
  #   dplyr::rename(WOE_ROUTE = ROUTE)

  ####################################################
  ### Transform WOE_Tox_Vals.xlsx - NO LONGER USING
  ####################################################
  # Fix WOE field names
  # names(iris_data$WOE_Tox_Vals.xlsx)[
  #   !grepl("CHEMICAL|URL|CAS REG DISPLAY", names(iris_data$WOE_Tox_Vals.xlsx))] <- names(iris_data$WOE_Tox_Vals.xlsx)[
  #     !grepl("CHEMICAL|URL|CAS REG DISPLAY", names(iris_data$WOE_Tox_Vals.xlsx))] %>%
  #   paste0("WOE_", .) %>%
  #   gsub(" ", "_", .)
  #
  # iris_data$WOE_Tox_Vals.xlsx <- iris_data$WOE_Tox_Vals.xlsx %>%
  #   dplyr::rename(CASRN = `CAS REG DISPLAY`,
  #                 `Slope Factor`=`WOE_SLOPEFACTOR_VALUE`,
  #                 `Unit Risk`=`WOE_UNIT_RISK_VALUE`)
  #
  # # Pivot toxval_type and numeric
  # iris_data$WOE_Tox_Vals.xlsx <- iris_data$WOE_Tox_Vals.xlsx %>%
  #   tidyr::pivot_longer(cols = c(`Slope Factor`, `Unit Risk`),
  #                       names_to = "toxval_type", values_to = "toxval_numeric") %>%
  #   tidyr::unite(col="toxval_type", `WOE_STUDY_ROUTE_CODE`, toxval_type, sep=" ", remove=FALSE)

  ####################################################
  ### Transform RfC_Toxicity_Values.xlsx
  ####################################################
  iris_data$RfC_Toxicity_Values.xlsx <- iris_data$RfC_Toxicity_Values.xlsx %>%
    dplyr::rename(RfC = `RFC VALUE`)
  iris_data$RfC_Toxicity_Values.xlsx <- iris_data$RfC_Toxicity_Values.xlsx %>%
    tidyr::pivot_longer(cols = c(`RfC`),
                        names_to = "toxval_type", values_to = "toxval_numeric") %>%
    tidyr::separate(col = "toxval_numeric",
                    into = c("toxval_numeric", "toxval_units"),
                    sep=" ", extra = "merge") %>%
    dplyr::mutate(toxval_numeric = as.numeric(toxval_numeric)) %>%
    dplyr::distinct()

  ####################################################
  ### Transform RfD_Toxicity_Values.xlsx
  ####################################################
  iris_data$RfD_Toxicity_Values.xlsx <- iris_data$RfD_Toxicity_Values.xlsx %>%
    dplyr::rename(RfD = `RFD VALUE`)
  iris_data$RfD_Toxicity_Values.xlsx <- iris_data$RfD_Toxicity_Values.xlsx %>%
    tidyr::pivot_longer(cols = c(`RfD`),
                        names_to = "toxval_type", values_to = "toxval_numeric") %>%
    tidyr::separate(col = "toxval_numeric",
                    into = c("toxval_numeric", "toxval_units"),
                    sep=" ", extra = "merge") %>%
    dplyr::mutate(toxval_numeric = as.numeric(toxval_numeric),
                  toxval_units = toxval_units %>%
                    # Remove extra superscript HTML tag
                    gsub("<sup>", "", ., fixed=TRUE) %>%
                    gsub("</sup>", "", ., fixed=TRUE)) %>%
    dplyr::distinct()

  #######################################################
  ### Join files
  #######################################################

  # Combine chemical detail files
  chem_details <- iris_data$chemicals_details.xlsx %>%
    dplyr::left_join(iris_data$Chemical_Rev_History.xlsx %>%
                select(-`CHEMICAL NAME`, -CASRN, -URL),
              by="CHEMICAL ID") %>%
    dplyr::distinct()

  # Combine RfC/RfD files
  rfc_rfd <- rbind(iris_data$RfC_Toxicity_Values.xlsx,
                   iris_data$RfD_Toxicity_Values.xlsx)

  ########################################################################
  ### Check if any values in RfC/RfD not present in Chemical Details file
  ########################################################################
  tmp1 <- chem_details %>%
    dplyr::select(`CHEMICAL ID`, critical_effect_tumor_type,
                  toxval_type, toxval_numeric, toxval_units) %>%
    dplyr::filter(toxval_type %in% c("RfC", "RfD")) %>%
    tidyr::unite(col="compare", sep="_", remove = FALSE)

  tmp2 <- rfc_rfd %>%
    dplyr::select(`CHEMICAL ID`, critical_effect_tumor_type = `PRINCIPAL CRITICAL DESCRIPTION`,
                  toxval_type, toxval_numeric, toxval_units) %>%
    tidyr::unite(col="compare", sep="_", remove = FALSE)

  # Check if any records present in one but not the other
  if(length(tmp1$compare[!tmp1$compare %in% tmp2$compare]) |
     length(tmp1$compare[!tmp1$compare %in% tmp2$compare])){
    stop("Data present in one join but not the other...check chemical details and RfC/RfD data")
  }

  # Combine RFC_RFD data to chemical details
  res0 <- chem_details %>%
    dplyr::ungroup() %>%
    # Required so that the join will work due to float/double precision
    dplyr::mutate(toxval_numeric = as.character(toxval_numeric)) %>%
    # Used to check for dups as needed
    # dplyr::mutate(tmp_id = 1:n()) %>%
    #chem_details %>%
    dplyr::left_join(rfc_rfd %>%
                       dplyr::select(-`CHEMICAL NAME`, -CASRN, -URL) %>%
                       # Required so that the join will work due to float/double precision
                       dplyr::mutate(toxval_numeric = as.character(toxval_numeric)),
              by = c("CHEMICAL ID",
                     "critical_effect_tumor_type" = "PRINCIPAL CRITICAL DESCRIPTION",
                     "toxval_type",
                     "toxval_numeric",
                     "toxval_units")) %>%
    dplyr::rename(iris_chemical_id = `CHEMICAL ID`,
                  critical_effect = critical_effect_tumor_type,
                  assessment_type = type,
                  endpoint = `PRINCIPAL CRITICAL EFFECT SYSTEM`,
                  risk_assessment_duration = DURATION,
                  study_reference = `STUDY CITATION`) %>%
    dplyr::mutate(toxval_units = fix.greek.symbols(toxval_units))

  # Check joins (identified issue with float/double versus character toxval_numeric join)
  # tmp <- chem_details %>%
  #   filter(toxval_type %in% c("RfC", "RfD"),
  #          `CHEMICAL ID` == 71) %>%
  #   select(c("CHEMICAL ID",
  #            "critical_effect_tumor_type",
  #            "toxval_type",
  #            "toxval_numeric",
  #            "toxval_units")) %>%
  #   # Required so that the join will work due to float/double precision
  #   dplyr::mutate(toxval_numeric = as.character(toxval_numeric))
  #
  # tmp2 <- rfc_rfd %>%
  #   filter(toxval_type %in% c("RfC", "RfD"),
  #          `CHEMICAL ID` == 71) %>%
  #   select(c("CHEMICAL ID",
  #            "critical_effect_tumor_type" = "PRINCIPAL CRITICAL DESCRIPTION",
  #            "toxval_type",
  #            "toxval_numeric",
  #            "toxval_units"),
  #          `POD VALUE`) %>%
  #   # Required so that the join will work due to float/double precision
  #   dplyr::mutate(toxval_numeric = as.character(toxval_numeric))
  #
  # tmp3 <- tmp %>%
  #   left_join(tmp2)

  # Check for duplicates - some known for chemical ID 73. 701, and 197 (different POD or Duration)
  # dups <- out %>% group_by(tmp_id) %>% dplyr::summarise(n=n()) %>% filter(n > 1)
  # View(out %>% filter(tmp_id %in% dups$tmp_id))

  # Species list to attempt string matches
  species_list <- list(dog=list("dog", "dogs"),
                       human=list("human", "humans", "occupational", "epidemiology", "epidemiological", "epidemiologic"),
                       mouse=list("mouse", "mice", "mouses"),
                       `monkey`=list("nonhuman primate", "monkey", "primate", "monkies", "monkeys",
                                     "rhesus monkeys (macaca mulatta)", "rhesus monkeys",
                                     "cynomolgus monkeys (macaca fascicularis)"),
                       rat=list("rat", "rats"),
                       rabbit = list("rabbit", "rabbits"),
                       `guinea pig` = list("guinea pig", "guinea pigs"),
                       frog = list("frog", "frogs"),
                       hen = list("hen")
  ) %>% unlist() %>%
    paste0(collapse="|")

  res0 <- res0 %>%
    dplyr::rowwise() %>%
    dplyr::mutate(species = `PRINCIPAL STUDY` %>%
                    tolower() %>%
                    # Extract species from `PRINCIPAL STUDY`
                    stringr::str_extract_all(., species_list) %>%
                    unlist() %>%
                    unique() %>%
                    paste0(collapse="; "),
                  # Extract sex from `PRINCIPAL STUDY`
                  sex = `PRINCIPAL STUDY` %>%
                    tolower() %>%
                    # Extract any matches to sex
                    stringr::str_extract_all(., "female|females|male|males") %>%
                    unlist() %>%
                    unique() %>%
                    paste0(collapse="; "))
  # Fill in missing with "-"
  res0$species[res0$species %in% c("", "NA")] <- "-"
  # Set occupational or epidemilog* studies to human species
  res0$species[grepl("occupation|epidemiolog", res0$species)] <- "human"
  # res0 %>% select(`PRINCIPAL STUDY`, species) %>% distinct() %>% View()
  res0$sex[res0$sex %in% c("", "NA")] <- "-"

  # Transform/relabel `EXPERIMENTAL DOSE TYPE` into a toxval_type with POD as the toxval_numeric/units
  pod_fix <- res0 %>%
    # Only available for those with toxval_type RfC and RfD
    filter(toxval_type %in% c("RfC", "RfD")) %>%
    dplyr::select(-toxval_type, -toxval_numeric, -toxval_units) %>%
    dplyr::rename(toxval_type = `EXPERIMENTAL DOSE TYPE`,
                  toxval_numeric = `POD VALUE`) %>%
    # Rename POD labels
    dplyr::mutate(toxval_type = case_when(
      `toxval_type` == "No Observable Adverse Effect Level" ~ "NOAEL",
      `toxval_type` == "Lowest Effect Level" ~ "LEL",
      `toxval_type` == "Lowest Observable Adverse Effect Level" ~ "LOAEL",
      `toxval_type` == "Human Equivalent Dose" ~ "HED",
      `toxval_type` == "Human Equivalent Concentration" ~ "HEC",
      `toxval_type` == "No Observed Effect Level" ~ "NOEL",
      `toxval_type` == "Benchmark Concentration" ~ "BMC",
      `toxval_type` == "Benchmark Dose based on the LED10 (Lowest Effective Dose)" ~ "BMD (LED10)",
      `toxval_type` == "Benchmark Concentration based on the LEC10 (Lowest Effective Concentration)" ~ "BMC (LEC10)",
      `toxval_type` == "Benchmark Dose" ~ "BMD",
      `toxval_type` == "Lowest Effect Concentration" ~ "LEC",
      `toxval_type` == "Lowest Effect Dose" ~ "LED"
    )) %>%
    # Split POD value into value and units
    tidyr::separate(col = "toxval_numeric",
                    into = c("toxval_numeric", "toxval_units"),
                    sep=" ", extra = "merge") %>%
    # dplyr::mutate(toxval_numeric = as.numeric(toxval_numeric)) %>%
    dplyr::distinct()
  # Check toxval_type missingness
  # Known missing POD for CHEMICAL ID 17, 31, 32, 92, 93, 101, 106, 199
  # pod_fix %>% filter(is.na(toxval_type)) %>% select(`CHEMICAL ID`) %>% unique()

  # Combine POD values to main dataset
  res0 <- res0 %>%
    dplyr::select(-`EXPERIMENTAL DOSE TYPE`, -`POD VALUE`) %>%
    rbind(pod_fix)

  # Add source version date
  res0$source_version_date <- src_version_date

  # Fix names
  names(res0) <- names(res0) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  # Add study_type, study_duration_class, and exposure_route, fix critical effect
  res0 <- res0 %>%
    dplyr::rename(exposure_route=route,
                  study_type = assessment_type,
                  study_duration_class=risk_assessment_duration) %>%
    tidyr::unite(col="study_type", study_type, principal_study, sep=" - ", remove = FALSE) %>%
    # Combine endpoint and critical_effect
    tidyr::unite(col="critical_effect", endpoint, critical_effect, sep=": ", remove = FALSE) %>%
    dplyr::mutate(critical_effect = critical_effect %>%
                    gsub("-: ", "", .),
                  study_type = study_type %>%
                    gsub(" - -| - NA", "", .)) %>%
    # Team decision to remove uncertainty_factor, modifying_factor, and dose_type (causing unnecessary duplicates)
    dplyr::select(-uncertainty_factor, -modifying_factor, -dose_type) %>%
    dplyr::distinct()

  # Fill blank hashing cols
  res0[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res0)]] <- "-"

  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db=db,
                       source=source,
                       table=source_table,
                       res=res0,
                       do.reset=do.reset,
                       do.insert=do.insert,
                       chem.check.halt=chem.check.halt,
                       hashing_cols=toxval.config()$hashing_cols)

}
