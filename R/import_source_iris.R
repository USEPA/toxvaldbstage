#--------------------------------------------------------------------------------------
#' @description Import of IRIS 2023-05-09 source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @param do.summary_data If TRUE, add IRIS Summary data to table before insertion
#' @title import_source_iris
#' @return None; data is pushed to toxval_source
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
import_source_iris <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE, do.summary_data=FALSE) {
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
    .[!grepl("woe|draft", ., ignore.case = TRUE)]

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
  # Pull most recent revision year to map to year field
  iris_year_map = iris_data$Chemical_Rev_History.xlsx %>%
    dplyr::group_by(`CHEMICAL ID`) %>%
    dplyr::filter(iris_revision_number == max(`iris_revision_number`)) %>%
    dplyr::select(iris_chemical_id=`CHEMICAL ID`, year=iris_revision_date) %>%
    dplyr::mutate(year = year %>%
                    format(format = "%Y") %>%
                    as.numeric())
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

  iris_data$RfC_Toxicity_Values.xlsx$`POD VALUE`[is.na(iris_data$RfC_Toxicity_Values.xlsx$`POD VALUE`)] <- "NO-POD"
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

  iris_data$RfD_Toxicity_Values.xlsx$`POD VALUE`[is.na(iris_data$RfD_Toxicity_Values.xlsx$`POD VALUE`)] <- "NO-POD"

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
    tidyr::unite(col="compare", sep="_", remove = FALSE, na.rm = TRUE)

  tmp2 <- rfc_rfd %>%
    dplyr::select(`CHEMICAL ID`, critical_effect_tumor_type = `PRINCIPAL CRITICAL DESCRIPTION`,
                  toxval_type, toxval_numeric, toxval_units) %>%
    tidyr::unite(col="compare", sep="_", remove = FALSE, na.rm = TRUE)

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
    dplyr::mutate(toxval_units = fix.replace.unicode(toxval_units),
                  study_reference = study_reference %>%
                    fix.replace.unicode() %>%
                    stringr::str_squish())

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
                    sep=" ", extra = "merge", fill="right") %>%
    # dplyr::mutate(toxval_numeric = as.numeric(toxval_numeric)) %>%
    dplyr::distinct() %>%
    # Remove NA POD Value entries relabeled as NO-POD
    dplyr::filter(toxval_numeric != "NO-POD")
  # Check toxval_type missingness
  # Known missing POD for CHEMICAL ID 17, 31, 32, 92, 93, 101, 106, 199
  # pod_fix %>% filter(is.na(toxval_type)) %>% select(`CHEMICAL ID`) %>% unique()

  # Combine POD values to main dataset
  res0 <- res0 %>%
    dplyr::select(-`EXPERIMENTAL DOSE TYPE`, -`POD VALUE`) %>%
    dplyr::bind_rows(pod_fix)

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
    tidyr::unite(col="study_type", study_type, principal_study, sep=" - ", remove = FALSE, na.rm = TRUE) %>%
    # Combine endpoint and critical_effect
    tidyr::unite(col="critical_effect", endpoint, critical_effect, sep=": ", remove = FALSE, na.rm = TRUE) %>%
    dplyr::mutate(critical_effect = critical_effect %>%
                    gsub("-: ", "", .) %>%
                    stringr::str_squish(),
                  study_type = study_type %>%
                    gsub(" - -| - NA", "", .),
                  exposure_route = tolower(exposure_route)) %>%
    # Team decision to remove uncertainty_factor, modifying_factor, and dose_type (causing unnecessary duplicates)
    dplyr::select(-uncertainty_factor, -modifying_factor, -dose_type) %>%
    dplyr::distinct()


  # Join with year map to add year field
  res0 = res0 %>%
    dplyr::left_join(iris_year_map,
                     by="iris_chemical_id")

  # Fix toxval_units where needed (RfC listed as None)
  res0$toxval_units[res0$toxval_type == "RfC" & res0$toxval_units %in% c("None")] = "mg/m3"
  # inhalation with oral units (inhalation = mg/m3)
  res0$toxval_units[res0$exposure_route == "inhalation" & res0$toxval_units == "mg/kg-day"] = "mg/m3"
  # oral with inalation units (oral = mg/kg-day)
  res0$toxval_units[res0$exposure_route == "oral" & res0$toxval_units == "mg/m3"] = "mg/kg-day"

  ################################################################################
  ### Append IRIS Summary information
  ################################################################################

  # Set defaults for new fields
  res0$document_type = 'IRIS Export'
  # Set key_findings, to yes if not in subset of toxval_type
  not_key_finding = c("RfD", "RfC", "Cancer Slope", "Unit Risk Factor", "Inhalation Unit Risk", "Oral Slope Factor")
  res0$key_finding = ifelse(res0$toxval_type %in% not_key_finding, 'no', 'key')

  # Add summary data to df before prep and load
  if(do.summary_data){
    # Import manually curated IRIS Summary information
    res1 <- iris_data$source_iris_summary_curation.xlsx %>%
      dplyr::mutate(
        document_type = 'IRIS Summary',
        key_finding = 'key',
        iris_chemical_id = url %>%
          sub('.*=', '', .) %>%
          as.numeric(),
        route = tolower(route),
        long_ref=full_reference) %>%
      # Remove IRIS Export fields
      # dplyr::select(-principal_study, -document_type, -endpoint) %>%
      dplyr::rename(
        exposure_route = route,
        species = species_original,
        sex = sex_original,
        age = age_original
      )
    # Set non-manually curated fields to blank
    res1[, c(#"principal_study", "exposure_route", "critical_effect", "assessment_type",
      "risk_assessment_duration", "endpoint")] <- "-"
    res = res0 %>%
      dplyr::mutate(full_reference = as.character(NA)) %>%
      dplyr::bind_rows(res1) %>%
      dplyr::distinct()
  } else {
    res = res0 %>%
      dplyr::distinct() %>%
      dplyr::mutate(long_ref = as.character(NA))
  }

  res = res %>%
    # Handle toxval_numeric_qualifier
    dplyr::mutate(toxval_numeric_qualifier = case_when(
      grepl("<", toxval_numeric) ~ "<",
      grepl(">", toxval_numeric) ~ ">",
      grepl("~", toxval_numeric) ~ "~",
      grepl("=", toxval_numeric) ~ "=",
      TRUE ~ NA_character_
    )) %>%
    # Remove qualifier symbols
    dplyr::mutate(toxval_numeric = gsub("[<>=~]", "", toxval_numeric))

  # Handle ranged toxval_numeric values
  ranged_res = res %>% dplyr::filter(grepl("[0-9]+-[0-9]+", toxval_numeric))
  if(nrow(ranged_res)) {
    ranged_res = ranged_res %>%
      dplyr::mutate(
        numeric_relationship_id = dplyr::row_number()
      ) %>%
      tidyr::separate(
        col = toxval_numeric,
        into = c("Lower Range", "Upper Range"),
        sep = "-",
        remove = TRUE
      ) %>%
      tidyr::pivot_longer(
        cols = c("Lower Range", "Upper Range"),
        values_to = "toxval_numeric",
        names_to = "numeric_relationship_description"
      )
  } else {
    # Empty dataframe with res cols to bind_rows()
    res$numeric_relationship_description = as.character(NA)
    ranged_res = res[0,]
  }

  if(!"toxval_subtype" %in% names(res)){
    res$toxval_subtype = NA
  }

  # Add ranged data to res
  res = res %>%
    dplyr::filter(!grepl("[0-9]+-[0-9]+", toxval_numeric)) %>%
    dplyr::bind_rows(ranged_res) %>%
    dplyr::mutate(toxval_numeric = as.numeric(toxval_numeric)) %>%
    # Combine numeric_relationship_description with toxval_subtype
    tidyr::unite(col="toxval_subtype", toxval_subtype, numeric_relationship_description,
                 sep = " ",
                 remove = FALSE,
                 na.rm = TRUE) %>%
    dplyr::mutate(toxval_subtype = toxval_subtype %>%
                    dplyr::na_if(""),
                  sex = dplyr::case_when(
                    grepl("^male;", sex) ~ "male/female",
                    grepl("not reported", sex, ignore.case=TRUE) ~ "not reported",
                    TRUE ~ sex
                  ))

  # Fix special case of study_type information set to toxval_subtype
  res = res %>%
    dplyr::mutate(toxval_subtype = dplyr::case_when(
      grepl("acute:", study_type) ~ study_type,
      TRUE ~ toxval_subtype
    ),
    long_ref = dplyr::case_when(
      long_ref %in% c(NA, "-") ~ study_reference,
      TRUE ~ long_ref
    ),

    dplyr::across(dplyr::where(is.character), ~tidyr::replace_na(., "-") %>%
                    fix.replace.unicode() %>%
                    stringr::str_squish()),
    dplyr::across(dplyr::where(is.character), ~gsub("\\r|\\n|\\\\r|\\\\n", "", .)),
    dplyr::across(dplyr::where(is.character), ~gsub("\\\\'", "'", .)),
    dplyr::across(dplyr::where(is.character), ~gsub('\\\\\\"', '"', .))
    )

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"

  # Map summary information back to export entries
  if("IRIS Summary" %in% unique(res$document_type)) {
    # Separate out relevant summary data
    summary_data = res %>%
      dplyr::filter(document_type == "IRIS Summary") %>%
      dplyr::select(iris_chemical_id, toxval_numeric, toxval_units, toxval_type,
                    study_type, principal_study) %>%
      # Set IRIS Export document_type to avoid many-to-many mapping
      dplyr::mutate(document_type = "IRIS Export") %>%
      dplyr::rename(principal_study_summary = principal_study)

    res = res %>%
      # Add summary data to export entries
      dplyr::rename(export_study_type = study_type) %>%
      dplyr::left_join(summary_data,
                       by = c("iris_chemical_id", "toxval_numeric", "toxval_units",
                              "toxval_type", "document_type")) %>%
      # Use export study_type when summary mapping not available
      dplyr::mutate(
        study_type = dplyr::case_when(
          !(study_type %in% c(as.character(NA), "-", "")) ~ study_type,
          TRUE ~ export_study_type
        ),

        # Set toxval_subtype from summary study_type without overwriting range subtypes
        toxval_subtype = dplyr::case_when(
          study_type %in% c(as.character(NA), "-", "") ~ toxval_subtype,
          grepl("(?:Upper|Lower) Range", toxval_subtype) ~ stringr::str_c(study_type,
                                                                          stringr::str_extract(toxval_subtype,
                                                                                               "((?:Upper|Lower) Range)",
                                                                                               group = 1),
                                                                          sep = " "),
          TRUE ~ study_type
        ),

        # Directly assign study_duration_class from summary principal_study
        study_duration_class = dplyr::case_when(
          !(principal_study_summary %in% c(as.character(NA), "-", "")) ~ principal_study_summary,
          TRUE ~ study_duration_class
        )
      ) %>%
      dplyr::select(-export_study_type, -principal_study_summary)

    # Separate out summary reference data
    summary_ref_data = res %>%
      dplyr::filter(document_type == "IRIS Summary") %>%
      dplyr::select(study_reference, principal_study, full_reference) %>%
      # Set full_reference to be first group appearance to avoid many-to-many issue
      dplyr::group_by(study_reference, principal_study) %>%
      dplyr::mutate(full_reference = dplyr::first(full_reference)) %>%
      dplyr::ungroup() %>%
      # Set IRIS Export document_type to avoid many-to-many mapping
      dplyr::mutate(document_type = "IRIS Export") %>%
      dplyr::distinct() %>%
      dplyr::filter(!(full_reference %in% c(as.character(NA), "-", "")))

    # Add summary long_ref to export entries
    res = res %>%
      dplyr::rename(prev_full_reference = full_reference) %>%
      dplyr::left_join(summary_ref_data,
                       by = c("study_reference", "principal_study", "document_type")) %>%
      dplyr::mutate(
        long_ref = dplyr::case_when(
          !(full_reference %in% c(as.character(NA), "-", "")) ~ full_reference,
          TRUE ~ long_ref
        )
      ) %>%
      dplyr::select(-prev_full_reference)
  }

  # Species list to attempt string matches
  species_list <- list(dog=list("dog"),
                       human=list("human", "occupational", "epidemiology", "epidemiological", "epidemiologic", "workers"),
                       mouse=list("mouse", "mice"),
                       monkey=list("nonhuman primate", "monkey", "primate", "monkies", "monkeys"),
                       rat=list("\\brat\\b", "\\brats\\b"),
                       rabbit = list("rabbit"),
                       `guinea pig` = list("guinea pig"),
                       frog = list("frog"),
                       hen = list("\\bhen\\b"),
                       hamster = list("hamster")
  ) %>% unlist() %>%
    paste0(collapse="|")

  # Lifestage list to attempt string matches
  lifestage_list <- list(child=list("child", "children"),
                         young=list("young"),
                         infant=list("infant", "infants"),
                         adult=list("adult", "adults"),
                         maternal=list("maternal"),
                         paternal=list("paternal"),
                         fetal=list("fetal", "fetotoxic")
  ) %>% unlist() %>%
    paste0(collapse="|")

  res = res %>%
    dplyr::mutate(
      # Extract species from principal_study if species not already present
      species = dplyr::case_when(
        species %in% c(NA, "-", "") ~ purrr::map_chr(stringr::str_extract_all(tolower(principal_study), species_list), paste, collapse = "; ") %>%
          stringr::str_split("; ") %>%
          sapply(stringr::str_squish) %>%
          sapply(unique) %>%
          sapply(paste, collapse="; ") %>%
          tolower() %>%
          gsub("mice", "mouse", .) %>%
          gsub("rats", "rat", .) %>%
          gsub("occupational|epidemiology|epidemiological|epidemiologic|workers", "human", .) %>%
          stringr::str_split("; ") %>%
          sapply(stringr::str_squish) %>%
          sapply(unique) %>%
          sapply(paste, collapse="; ") %>%
          gsub("rat; human", "human", .),
        TRUE ~ species
      ),

      # Extract sex from principal_study if sex not already present
      sex = dplyr::case_when(
        !(sex %in% c("", "-", as.character(NA), "not reported")) ~ sex,
        grepl("\\bmale", principal_study) & grepl("female", principal_study) ~ "male/female",
        grepl("female", principal_study) ~ "female",
        grepl("\\bmale", principal_study) ~ "male",
        TRUE ~ as.character(NA)
      ),

      # Extract strain from principal_study
      strain = dplyr::case_when(
        strain %in% c(NA, "-", "") ~ purrr::map_chr(stringr::str_extract_all(principal_study, paste0("(CD\\-1|B6C3F1|Carworth Farm E|New Zealand White",
                                                                                                     "|F344(?:\\/N)?|(?:Crl:CD |Harlan )?Sprague\\-Dawley|beagle)")),
                                                    paste, collapse = "; ") %>%
          stringr::str_split("; ") %>%
          sapply(stringr::str_squish) %>%
          sapply(unique) %>%
          sapply(paste, collapse="; "),
        TRUE ~ strain
      ),

      # Extract study_duration from principal_study
      study_duration = principal_study %>%
        gsub("Ds", "D", .) %>%
        gsub("D([0-9])", "D \\1", .) %>%
        gsub("\\[|\\]", "", .) %>%
        gsub("[Tt]wo", "2", .) %>%
        gsub("3, 6 or 8", "3-8", .) %>%
        gsub("2\\- to 9", "2-9", .) %>%
        gsub("0 to 3 or 8", "0-8", .) %>%
        gsub("\\(GD\\)", "GD", .) %>%
        gsub("([0-9])\\-([a-zA-Z])", "\\1 \\2", .) %>%
        gsub("\\?", "-", .) %>%
        gsub("\\s*to\\s*|\\s*\\-\\s*", "-", .) %>%
        gsub("GD ([0-9]+) GD ([0-9]+)", "GD \\1-\\2", .),
      study_duration_value = dplyr::case_when(
        grepl("GD [0-9]+\\-[0-9+] weeks", study_duration) ~ stringr::str_extract(study_duration,
                                                                                 "(GD [0-9]+\\-[0-9+] weeks)",
                                                                                 group=1),
        grepl("[0-9\\.]+(?:\\-[0-9\\.]+)?\\s*(?:day|generation|year|month|\\bd\\b|week|hr)",
              study_duration,
              ignore.case=TRUE) ~ stringr::str_extract(tolower(study_duration),
                                                       "([0-9\\.]+(?:\\-[0-9\\.]+)?)\\s*(?:day|generation|year|month|\\bd\\b|week|hr)",
                                                       group=1),
        grepl("(?:GD|PND) [0-9]+\\-[0-9]+", study_duration) ~ stringr::str_extract(study_duration,
                                                                                   "(?:GD|PND) ([0-9]+\\-[0-9]+)",
                                                                                   group=1),
        TRUE ~ as.character(NA)
      ),
      study_duration_units = dplyr::case_when(
        is.na(study_duration_value) ~ as.character(NA),
        grepl("GD [0-9]+\\-[0-9+] weeks", study_duration) ~ "GD,weeks",
        grepl("[0-9\\.]+(?:\\-[0-9\\.]+)?\\s*(?:day|generation|year|month|\\bd\\b|week|hr)",
              study_duration,
              ignore.case=TRUE) ~ stringr::str_extract(tolower(study_duration),
                                                       "[0-9\\.]+(?:\\-[0-9\\.]+)?\\s*(day|generation|year|month|\\bd\\b|week|hr)",
                                                       group=1),
        grepl("(?:GD|PND) [0-9]+\\-[0-9]+", study_duration) ~ stringr::str_extract(study_duration,
                                                                                   "(GD|PND) [0-9]+\\-[0-9]+",
                                                                                   group=1),
        TRUE ~ as.character(NA)
      ) %>%
        gsub("\\bd\\b", "day", .) %>%
        gsub("hr", "hour", .),

      # Extract lifestage from principal_study
      lifestage = purrr::map_chr(stringr::str_extract_all(tolower(principal_study), lifestage_list), paste, collapse = "; ") %>%
        stringr::str_split("; ") %>%
        sapply(stringr::str_squish) %>%
        sapply(unique) %>%
        sapply(paste, collapse="; ") %>%
        tolower() %>%
        gsub("fetotoxic", "fetal", .),

      # Extract exposure_method from principal_study
      exposure_method = principal_study %>%
        tolower() %>%
        stringr::str_extract("(drinking water|feed|gavage|diet|capsule)", group=1),
      #  Remove "feed" exposure_method from subcutaneous entry
      exposure_method = dplyr::case_when(
        exposure_route == "subcutaneous" ~ as.character(NA),
        TRUE ~ exposure_method
      ),
      #  Ensure exposure_route matches extracted exposure_method
      exposure_route = dplyr::case_when(
        exposure_method %in% c("gavage", "feed", "drinking water") ~ "oral",
        TRUE ~ exposure_route
      )
    ) %>%
    # Remove intermediate study_duration parsing field
    dplyr::select(-study_duration)
  # Fill in missing with "-"
  res$species[res$species %in% c("", "NA")] <- "-"
  # Set occupational or epidemilog* studies to human species
  res$species[grepl("occupation|epidemiolog", res$species)] <- "human"
  res$sex[res$sex %in% c("", "NA")] <- "-"

  # Hardcode species as human for RfD, RfD, HED, HED, Slope Factor, Unit Risk
  human_toxval_type = c("RfD", "Inhalation Unit Risk", "RfC", "Oral Slope Factor")
  res$species[res$toxval_type %in% human_toxval_type] = "human"
  blank_hash_cols = c("exposure_method", "exposure_form", "media",
                      "generation", "lifestage", "population",
                      "study_duration_qualifier", "study_duration_value", "study_duration_units",
                      "sex", "strain")
  # Set blank if toxval_type is a derived value
  res = res %>%
    dplyr::mutate(
      # Remove species lists, set as "-"
      species = dplyr::case_when(
        grepl(";", species) ~ "-",
        TRUE ~ species
      ),
      dplyr::across(any_of(blank_hash_cols), ~ dplyr::case_when(
        toxval_type %in% human_toxval_type ~ "-",
        TRUE ~ .
      )),
      # Fill blank/NA character fields with "-"
      dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(., "") %>%
                      tidyr::replace_na("-") %>%
                      fix.replace.unicode() %>%
                      stringr::str_squish()))

  # Perform deduping
  res = toxval.source.import.dedup(res, hashing_cols=toxval.config()$hashing_cols)

  # Add source version date
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
