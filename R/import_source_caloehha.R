#--------------------------------------------------------------------------------------
#' @description Load caloehha Source file into toxval_source
#' The raw data can be exported as an Excel sheet from the web site
#' https://oehha.ca.gov/chemicals, selecting the link "Export database as .CSV file"
#'
#' This method parses that file and prepares for loading into toxval source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile The input file ="../caloehha/caloehha_files/OEHHA-chemicals_2018-10-30T08-50-47.xlsx",
#' @param chem.check.halt If TRUE and there are problems with chemicals CASRN checks, halt the program
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
#'  \code{\link[openxlsx]{read.xlsx}}
#' @rdname import_caloehha_source
#' @export
#' @importFrom openxlsx read.xlsx
#--------------------------------------------------------------------------------------
import_caloehha_source <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source="Cal OEHHA"
  source_table = "source_caloehha"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2023-08-10")
  dir = paste0(toxval.config()$datapath,"caloehha/caloehha_files/")
  file = paste0(dir,"OEHHA-chemicals_2023-08-10T12-44-00.xlsx")
  res0 = readxl::read_xlsx(file)

  #####################################################################
  cat("Build original_caloehha table from source file\n")
  #####################################################################
  rename_list <- c(casrn = "CAS Number",
                   name = "Title",
                   inhalation_unit_risk = "Inhalation Unit Risk (ug/cubic meter)-1",
                   inhalation_slope_factor = "Inhalation Slope Factor (mg/kg-day)-1",
                   oral_slope_factor = "Oral Slope Factor (mg/kg-day)-1",
                   acute_rel = "Acute REL (ug/m3)",
                   acute_rel_species = "Species",
                   acute_rel_critical_effect = "Acute REL Toxicologic Endpoint",
                   acute_rel_target_organ = "Acute REL Target Organs",
                   acute_rel_severity = "Severity",
                   acute_rel_year = "Last Acute REL Revision",
                   rel_8_hour_inhalation = "8-Hour Inhalation REL (ug/m3)",
                   rel_8_hour_inhalation_year = "Last 8-Hour REL Revision",
                   chronic_inhalation_rel = "Chronic Inhalation REL (ug/m3)",
                   # chronic_oral_rel = "Chronic oral REL",
                   # chronic_oral_rel_units = "Chronic oral REL units",
                   chronic_rel_critical_effect = "Chronic Toxicologic Endpoint",
                   chronic_rel_target_organ = "Chronic Target Organs",
                   mcl = "MCL value (mg/L)",
                   phg = "Public Health Goal (mg/L)",
                   nsrl_inhalation = "No Significant Risk Level (NSRL) - Inhalation",
                   nsrl_oral = "No Significant Risk Level (NSRL) - Oral",
                   madl_inhalation_reprotox = "Maximum Allowable Dose Level (MADL) for chemicals causing reproductive toxicity - Inhalation",
                   madl_oral_reprotox = "Maximum Allowable Dose Level (MADL) for chemicals causing reproductive toxicity - Oral",
                   chrfd = "Child-specific refence dose (chRD) (mg/kg-day, unless noted)",
                   cancer_potency_year = "Last Cancer Potency Revision",
                   phg_year = "Last PHG Revision",
                   madl_nsrl_year = "Last NSRL/MADL Revision",
                   chrd_year = "Last chRD revision",
                   notification_level = "Notification Level (ug/L)"
                   )

    res = res0 %>%
      dplyr::rename(dplyr::all_of(rename_list)) %>%
      # dplyr::select(dplyr::all_of(names(rename_list))) %>%
      # Unit fields from old field names
      dplyr::mutate(inhalation_unit_risk_units = "(ug/m3)-1",
                    inhalation_slope_factor_units = "(mg/kg-day)-1",
                    oral_slope_factor_units = "(mg/kg-day)-1",
                    acute_rel_units = "ug/m3",
                    rel_8_hour_inhalation_units = "ug/m3",
                    chronic_inhalation_rel_units = "ug/m3",
                    mcl_units = "mg/L",
                    phg_units = "mg/L",
                    notification_level_units = "ug/L") %>%
      dplyr::distinct() %>%
      dplyr::mutate(dplyr::across(where(is.character), ~na_if(., "--")),
                    dplyr::across(where(is.character), ~na_if(., "n/a")),
                    dplyr::across(where(is.character), ~na_if(., "N/A"))) %>%
      # Separate casrn lists
      tidyr::separate_rows(casrn, sep = "; ")

    # Fix greek symbols
    res = res %>%
      dplyr::mutate(dplyr::across(c(rel_8_hour_inhalation, chronic_inhalation_rel,
                                    nsrl_inhalation, nsrl_oral, madl_inhalation_reprotox,
                                    madl_oral_reprotox),
                                  ~gsub("\u00c2\u00b5", "u", .)),
                    casrn = casrn %>%
                      gsub("\u00e2\u20ac\u017d", "", .),
                    name = fix.greek.symbols(name))

    # Split chronic_inhalation_rel into chronic_oral_rel with units
    res = res %>%
      tidyr::separate(chronic_inhalation_rel,
                      into=c("chronic_inhalation_rel", "chronic_oral_rel"),
                      sep="; ",
                      fill="right",
                      extra="merge") %>%
      dplyr::mutate(chronic_oral_rel_units = "ug/kg BW-day",
                    dplyr::across(c(chronic_inhalation_rel, chronic_oral_rel),
                                  ~gsub("\\(inhalation\\)|\\(oral\\)|ug/kg BW-day", "", .) %>%
                                    stringr::str_squish()))

    # Split units from numeric values (fill in/replace as needed)
    # Identified fields in need of unit extraction/fix
    field_list <- c("oral_slope_factor", "acute_rel", "rel_8_hour_inhalation", "chronic_inhalation_rel",
                    "mcl", "phg", "nsrl_inhalation", "nsrl_oral", "madl_inhalation_reprotox", "madl_oral_reprotox", "chrfd")

    # Identified list of units to extract/fix
    unit_list <- c("(fibers/L water)^-1", "micrograms per cubic meter (ug/m3) (700 parts per billion (ppb))",
                   "ug/m3 (0.7 ppb)", "million fibers/L", "pCi/L", "pCI/L", "picograms/L (pg/L)", "ug/day", "ug/dL",
                   "ug/m3 (0.3 ppb)")

    # Loop through identified fields to extract/fix units
    for(f in field_list){
      f_units <- paste0(f, "_units")
      # Create units column if not present
      if(!f_units %in% names(res)){
        res[[f_units]] <- NA
      }
      # Loop through identified units to extract/fix
      for(unit in unit_list){
        res[[f]][grepl(unit, res[[f]], fixed=TRUE)] <- res[[f]][grepl(unit, res[[f]], fixed=TRUE)] %>%
          gsub(unit, "", ., fixed=TRUE) %>%
          stringr::str_squish()
        res[[f_units]][grepl(unit, res[[f]], fixed=TRUE)] <- unit
      }
    }

    # Better handling of year fields where conversion to text (e.g., 6/10/2023) converted to a numeric
    res = res %>%
      dplyr::mutate(dplyr::across(dplyr::contains("year"), ~as.character(.)),
                    dplyr::across(dplyr::contains("year"),
                                  .fns = ~ dplyr::case_when(
                                    # Fix Year
                                    .=="44050" ~ "8/7/2020",
                                    .=="42621" ~ "9/8/2016",
                                    .=="41821" ~ "Jul-14",
                                    .=="42787" ~ "2/21/2017",
                                    .=="44804" ~ "8/31/2022",
                                    .=="43714" ~ "9/6/2019",
                                    .=="42459" ~ "3/30/2016",
                                    .=="42457" ~ "3/28/2016",
                                    .=="39783" ~ "12/1/2008",
                                    .=="39052" ~ "12/1/2006",
                                    .=="38096" ~ "4/19/2004",
                                    .=="38525" ~ "6/22/2005",
                                    .=="36777" ~ "9/8/2000",
                                    .=="28460" ~ "Dec-77",
                                    .=="42461" ~ "4/1/2016",
                                    .=="39356" ~ "Oct-07",
                                    .=="38687" ~ "Dec-05",
                                    .=="40330" ~ "Jun-10",
                                    .=="39173" ~ "Apr-07",
                                    .=="38869" ~ "Jun-06",
                                    .=="43224" ~ "5/4/2018",
                                    TRUE ~ .
                                    )
                                  )
                    )

    # Clean before exponent fix: mcl, phg, nsrl_inhalation, nsrl_oral
    # # Remove comma, text, and parenthetic values
    res = res %>%
      dplyr::mutate(mcl = mcl %>%
                      gsub(",|for total chromium|\\s*\\([^\\)]+\\)", "", .) %>%
                      gsub("as nitrogen|fibers/day", "", .) %>%
                      stringr::str_squish(),
                    phg =  phg %>%
                      gsub(",|for total chromium|\\s*\\([^\\)]+\\)", "", .) %>%
                      gsub("as nitrogen|fibers/day", "", .) %>%
                      stringr::str_squish(),
                    nsrl_inhalation =  nsrl_inhalation %>%
                      gsub(",|for total chromium|\\s*\\([^\\)]+\\)", "", .) %>%
                      gsub("as nitrogen|fibers/day", "", .) %>%
                      stringr::str_squish(),
                    nsrl_oral = nsrl_oral %>%
                      gsub(",|for total chromium|\\s*\\([^\\)]+\\)", "", .) %>%
                      gsub("as nitrogen|fibers/day", "", .) %>%
                      stringr::str_squish(),
                    # Just parenthetic removal for now...
                    notification_level = notification_level %>%
                      gsub("\\s*\\([^\\)]+\\)|, or at the lowest level that can be reliably detected with available technologies", "", .) %>%
                      stringr::str_squish()
                    )

    # Handle notification_level cleaning
    # View(res %>% select(name, notification_level, notification_level_units) %>% distinct() %>% filter(!is.na(notification_level)))
    res <- res %>%
      # Split entries with semicolon separated lists by duration class
      tidyr::separate_rows(notification_level, sep = "; ") %>%
      tidyr::separate(notification_level, into=c("study_duration_class", "notification_level"),
                      sep = ": ",
                      fill="left", extra="merge") %>%
      dplyr::mutate(study_duration_class = tolower(study_duration_class))

    # Handle madl_oral_reprotox and madl_inhalation_reprotox cleaning
    # 56, or 170 as 32% pesticidal formulation
    # Chemical by chemical basis
    # 17000 (dermal) or 700 (oral); 6700 (dermal)

    # Handle exponent strings
    field_list <- c("inhalation_unit_risk", "inhalation_slope_factor", "oral_slope_factor", "rel_8_hour_inhalation",
                    "mcl", "phg", "nsrl_inhalation", "nsrl_oral", "chrfd", "notification_level")

    # Define helper function for cleaning up scientific notation
    cal_oehha_parse_scientific <- function(s) {
      if(is.na(s)) return(as.numeric(s))

      # Remove all whitespace and commas
      s = gsub("[[:space:]]|,", "", s)
      if(!is.na(suppressWarnings(as.numeric(s)))) return(as.numeric(s))

      # Handle scientific notation conversion (either 10's or e notation)
      if(grepl("[Xx]", s) && grepl("^[0-9]+\\.?[0-9]* ?[Xx] ?10 ?-?[0-9]+", s)){
        mantissa <- as.numeric(gsub(" ?[Xx].*", "", s))
        exponent <- as.numeric(gsub('.*?[Xx] ?10', "", s))
        return(mantissa * 10^exponent)
      } else if(grepl("[eE]", s) && grepl("^[0-9]+\\.?[0-9]* ?[Ee] ?[-+]?[0-9]+", s)){
        mantissa <- as.numeric(gsub(" ?[eE].*", "", s))
        exponent <- as.numeric(gsub(".*?[eE]", "", s))
        return(mantissa * 10^exponent)
      }
      # Temporary filtering until these columns are dealt with separately
      if(!s %in% c("Oralcancerpotencycannotbedetermined",
                   "0.05fortotalchromium", "45(10asnitrogen)",
                   "3(1asnitrogen)", "10asnitrogen", "none",
                   "NotAvailable", "0.001(1ppb)", "100fibers/day",
                   "0.090(methylhydrazine)", "0.058(methylhydrazine)",
                   "1.0blood")){
        message(s)
        stop("Found issue with exponent conversion...")
      }
      return(suppressWarnings(as.numeric(s)))
    }

    res = res %>%
      dplyr::rowwise() %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(field_list),
                                  ~cal_oehha_parse_scientific(.))) %>%
      dplyr::ungroup()

    # Realign by toxval_type
    res$year = "2023"
    res[,c("critical_effect", "species_original")] = "-"
    res$study_duration_class[is.na(res$study_duration_class)] <- "chronic"
    res$target_organ = "-"
    res$serverity = "-"

    res <- lapply(c("inhalation_unit_risk", "inhalation_slope_factor",
                     "oral_slope_factor",
                     "acute_rel", "rel_8_hour_inhalation",
                     "chronic_inhalation_rel", "chronic_oral_rel",
                     "mcl", "phg",
                     "nsrl_inhalation", "nsrl_oral",
                     "madl_inhalation_reprotox", "madl_oral_reprotox",
                     "chrfd", "notification_level"),
                   function(f_name){
                     tmp = res %>%
                       dplyr::select(name, casrn,
                                     year, critical_effect, species_original, study_duration_class,
                                     toxval_numeric = !!f_name,
                                     toxval_units = !!paste0(f_name, "_units")) %>%
                       dplyr::mutate(toxval_type = ifelse(f_name %in% c("inhalation_unit_risk"),
                                                          "cancer unit risk",
                                                          ifelse(f_name %in% c("inhalation_slope_factor", "oral_slope_factor"),
                                                                 "cancer slope factor",
                                                                 ifelse(grepl("rel_|_rel", f_name),
                                                                        "REL",
                                                                        toupper(f_name))
                                                          )
                       ),
                       toxval_subtype = ifelse(f_name %in% c("mcl", "phg"),
                                               "-",
                                               f_name %>%
                                                 gsub("_", " ", .)),
                       study_type = ifelse(f_name %in% c("inhalation_unit_risk", "inhalation_slope_factor",
                                                         "oral_slope_factor"),
                                           "carcinogenicity",
                                           ifelse(grepl("acute|8_hour", f_name),
                                                  "acute",
                                                  ifelse(grepl("chronic|mcl|phg|nsrl|madl|chrfd", f_name),
                                                         "chronic",
                                                         "TBD")
                                           )
                       ),
                       exposure_route = ifelse(grepl("inhalation", f_name),
                                               "inhalation",
                                               ifelse(grepl("oral|acute_rel", f_name),
                                                      "oral",
                                                      ifelse(f_name %in% c("mcl", "phg", "chrfd"),
                                                             "oral",
                                                             "TBD")
                                               )
                       ),
                       toxval_numeric = as.numeric(toxval_numeric))

                     # Special cases by toxval_type
                     if(f_name == "acute_rel"){
                       tmp$critical_effect = res$acute_rel_critical_effect
                       tmp$target_organ =  res$acute_rel_target_organ
                       tmp$serverity =  res$acute_rel_severity
                       tmp$year = res$acute_rel_year
                       tmp$species_original = res$acute_rel_species
                       tmp$study_duration_class = "acute"
                       tmp$toxval_subtype = "acute REL"
                     } else if(f_name == "rel_8_hour_inhalation"){
                       tmp$toxval_subtype = "8 hour inhalation REL"
                       tmp$study_duration_class = "acute"
                     } else if(f_name == "chronic_inhalation_rel"){
                       tmp$toxval_subtype = "chronic inhalation REL"
                     } else if(f_name == "chronic_oral_rel"){
                       tmp$critical_effect = res$chronic_rel_critical_effect
                       tmp$target_organ = res$chronic_rel_target_organ
                       tmp$toxval_subtype = "chronic oral REL"
                     } else if(f_name == "phg"){
                       tmp$toxval_type = "OEHHA PHG"
                     } else if(grepl("nsrl", f_name)){
                       tmp$toxval_type = "OEHHA NSRL"
                       tmp$toxval_subtype = "-"
                     } else if(grepl("madl", f_name)){
                       tmp$toxval_type = "OEHHA MADL"
                       tmp$toxval_subtype = "-"
                     } else if(f_name == "chrfd"){
                       tmp$toxval_type = "RfD"
                       tmp$toxval_subtype = "Child RfD"
                     } else if(f_name == "notification_level"){
                       tmp$toxval_type = "OEHHA notification level"
                     }
                     return(tmp)
                   }) %>%
      dplyr::bind_rows() %>%
      # Filter out toxval_numeric NA values
      dplyr::filter(!is.na(toxval_numeric)) %>%
      # Clean up excess whitespace for all character fields
      dplyr::mutate(across(where(is.character), ~stringr::str_squish(.)))

##########################################################################################################

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
