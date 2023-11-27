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
import_caloehha_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
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
                    name = fix.greek.symbols(name) %>%
                      # Special case for p-Chloro-Î±,Î±,Î±-trifluorotoluene (para-Chlorobenzo trifluoride, PCBTF)
                      # Actually supposed to be alphas, so replacing with "a"
                      gsub("\u00ce\u00b1", "a", .))

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
    # Also normalizes mm/dd/yyyy to just year
    for (col_name in names(res)[grepl("year", names(res), ignore.case = TRUE)]){
      res[[col_name]] <- as.character(res[[col_name]])
      date_fix = res[[col_name]][grep("[0-9]{5}",
                          res[[col_name]])]

      if(length(date_fix)){
        date_fix = date_fix %>%
          as.numeric() %>%
          janitor::excel_numeric_to_date(date_system = "modern") %>%
          format(format = "%Y") %>%
          as.character()

        res[grep("[0-9]{5}", res[[col_name]]), col_name] = date_fix
        # res[which(res[[col_name]] == "-"), "year"] = NA
      }
      # Handle case of month, year (e.g., July, 2014)
      res[grep("[a-zA-Z]+", res[[col_name]]), col_name] = gsub(".*\\,\\s+(\\d{4})", "\\1", grep("[a-zA-Z]+", res[[col_name]],value= T))
      # TODO Comment what this line is supposed to accomplish...older code...
      res[[col_name]] <- ifelse(grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", res[[col_name]]),
                                sub("\\d{1,2}/\\d{1,2}/(\\d{4})", "\\1", res[[col_name]]),
                                res[[col_name]])
    }

    # Old year handling code
    # res = res %>%
    #   dplyr::mutate(dplyr::across(dplyr::contains("year"), ~as.character(.)),
    #                 dplyr::across(dplyr::contains("year"),
    #                               .fns = ~ dplyr::case_when(
    #                                 # Fix Year
    #                                 .=="44050" ~ "8/7/2020",
    #                                 .=="42621" ~ "9/8/2016",
    #                                 .=="41821" ~ "Jul-14",
    #                                 .=="42787" ~ "2/21/2017",
    #                                 .=="44804" ~ "8/31/2022",
    #                                 .=="43714" ~ "9/6/2019",
    #                                 .=="42459" ~ "3/30/2016",
    #                                 .=="42457" ~ "3/28/2016",
    #                                 .=="39783" ~ "12/1/2008",
    #                                 .=="39052" ~ "12/1/2006",
    #                                 .=="38096" ~ "4/19/2004",
    #                                 .=="38525" ~ "6/22/2005",
    #                                 .=="36777" ~ "9/8/2000",
    #                                 .=="28460" ~ "Dec-77",
    #                                 .=="42461" ~ "4/1/2016",
    #                                 .=="39356" ~ "Oct-07",
    #                                 .=="38687" ~ "Dec-05",
    #                                 .=="40330" ~ "Jun-10",
    #                                 .=="39173" ~ "Apr-07",
    #                                 .=="38869" ~ "Jun-06",
    #                                 .=="43224" ~ "5/4/2018",
    #                                 TRUE ~ .
    #                                 )
    #                               )
    #                 )

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
                    madl_inhalation_reprotox = madl_inhalation_reprotox %>%
                      gsub(",", "", .) %>%
                      stringr::str_squish(),
                    acute_rel = acute_rel %>%
                      gsub(",", "", .) %>%
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


    # Handle madl_oral_reprotox cleaning
    # Cyanide - 10 (hydrogen cyanide), 19 (sodium cyanide), 25 (potassium cyanide)
    # Di(2-ethylhexyl)phthalate - 410 (adult); 58 (infant boys, age 29 days to 24 months); 20 (neonatal infant boys, age 0 to 28 days)
    # Disodium Cyanodithioimidocarbonate - 56, or 170 as 32% pesticidal formulation
    # Ethyl Dipropylthiocarbamate - 700 (oral); 6700 (dermal)
    # N-Methylpyrrolidone - 17000 (dermal)
    # Sodium dimethyldithiocarbamate fix - 23, or 58 as 40% pesticidal formulation (not certain)

    # None of these should cause any issues, even if these records aren't in the table

    # Print out current violators within madl_oral_reprotox
    # View(res %>% select(madl_oral_reprotox) %>% unique(), "before_madl_oral_fixes")
    flagged <- ifelse(grepl("\\(", res$madl_oral_reprotox), res$madl_oral_reprotox, NA)
    flagged[stats::complete.cases(flagged)]
    flagged <- ifelse(grepl(";", res$madl_oral_reprotox), res$madl_oral_reprotox, NA)
    flagged[stats::complete.cases(flagged)]

    # Disodium Cyanodithioimidocarbonate fix
    res$madl_oral_reprotox[!is.na(res$madl_oral_reprotox) &
                             res$madl_oral_reprotox == "56, or 170 as 32% pesticidal formulation"] = "56"
    # res <- res %>%
    #   dplyr::mutate(madl_oral_reprotox=ifelse(madl_oral_reprotox == "56, or 170 as 32% pesticidal formulation", "56", madl_oral_reprotox))

    # Sodium dimethyldithiocarbamate fix
    res$madl_oral_reprotox[!is.na(res$madl_oral_reprotox) &
                             res$madl_oral_reprotox == "23, or 58 as 40% pesticidal formulation"] = "23"
    # res <- res %>%
    #   dplyr::mutate(madl_oral_reprotox=ifelse(madl_oral_reprotox == "23, or 58 as 40% pesticidal formulation", "23", madl_oral_reprotox))

    # Di(2-ethylhexyl)phthalate split
    row_to_split <- res %>%
      dplyr::filter(madl_oral_reprotox == "410 (adult); 58 (infant boys, age 29 days to 24 months); 20 (neonatal infant boys, age 0 to 28 days)")
    split_rows <- row_to_split %>%
      tidyr::separate_rows(madl_oral_reprotox, sep = "; ") %>%
      tidyr::separate(madl_oral_reprotox, into = c("madl_oral_reprotox", "toxval_subtype"), sep= " \\(") %>%
      dplyr::mutate(
        toxval_subtype = gsub("\\)", "", toxval_subtype)
      )

    # Remove data from all but one record that would be duplicated when normalized to toxval
    # for (i in 1:nrow(split_rows)){
    #   if (split_rows$madl_oral_reprotox[i] %in% c(58,20)){
    #     split_rows$inhalation_unit_risk[i] = NA
    #     split_rows$inhalation_slope_factor[i] = NA
    #     split_rows$oral_slope_factor[i] = NA
    #     split_rows$acute_rel[i] = NA
    #     split_rows$rel_8_hour_inhalation[i] = NA
    #     split_rows$chronic_inhalation_rel[i] = NA
    #     split_rows$mcl[i] = NA
    #     split_rows$phg[i] = NA
    #     split_rows$nsrl_inhalation[i] = NA
    #     split_rows$nsrl_oral[i] = NA
    #     split_rows$madl_inhalation_reprotox[i] = NA
    #     split_rows$chrfd[i] = NA
    #     split_rows$notification_level[i] = NA
    #   }
    # }

    if (nrow(split_rows) > 0){
      res <- res %>%
        dplyr::filter(!madl_oral_reprotox %in% "410 (adult); 58 (infant boys, age 29 days to 24 months); 20 (neonatal infant boys, age 0 to 28 days)")
      res <- dplyr::bind_rows(res, split_rows)
    }

    # Cyanide split
    row_to_split <- res %>%
      dplyr::filter(madl_oral_reprotox == "10 (hydrogen cyanide), 19 (sodium cyanide), 25 (potassium cyanide)")

    # Split rows adopt type of cyanide but other resulting records maintain general Cyanide name
    # Replacement for Cyanide to have "NA" value
    replacement <- row_to_split %>%
      dplyr::mutate(madl_oral_reprotox = NA)
    split_rows <- row_to_split %>%
      tidyr::separate_rows(madl_oral_reprotox, sep = ", ") %>%
      tidyr::separate(madl_oral_reprotox, into = c("madl_oral_reprotox", "name"), sep= " \\(") %>%
      dplyr::mutate(
        name = gsub("\\)", "", name) %>%
          stringr::str_to_title()
      )

    # Remove data from all but one record that would be duplicated when normalized to toxval
    # for (i in 1:nrow(split_rows)){
    #   if (split_rows$madl_oral_reprotox[i] %in% c(10,19,25)){
    #     split_rows$inhalation_unit_risk[i] = NA
    #     split_rows$inhalation_slope_factor[i] = NA
    #     split_rows$oral_slope_factor[i] = NA
    #     split_rows$acute_rel[i] = NA
    #     split_rows$rel_8_hour_inhalation[i] = NA
    #     split_rows$chronic_inhalation_rel[i] = NA
    #     split_rows$mcl[i] = NA
    #     split_rows$phg[i] = NA
    #     split_rows$nsrl_inhalation[i] = NA
    #     split_rows$nsrl_oral[i] = NA
    #     split_rows$madl_inhalation_reprotox[i] = NA
    #     split_rows$chrfd[i] = NA
    #     split_rows$notification_level[i] = NA
    #   }
    # }
    split_rows <- dplyr::bind_rows(split_rows, replacement)

    if (nrow(split_rows) > 0){
      res <- res %>%
        dplyr::filter(!madl_oral_reprotox %in% "10 (hydrogen cyanide), 19 (sodium cyanide), 25 (potassium cyanide)")
      res <- dplyr::bind_rows(res, split_rows)
    }

    # Ethyl Dipropylthiocarbamate split
    # Create columns to handle dermal cases
    # res$madl_dermal_reprotox = NA
    # res$madl_dermal_reprotox_units = NA

    row_to_change <- res %>%
      dplyr::filter(madl_oral_reprotox == "700 (oral); 6700 (dermal)") %>%
      dplyr::mutate(madl_dermal_reprotox = "6700",
                    madl_oral_reprotox = "700",
                    madl_dermal_reprotox_units = madl_oral_reprotox_units)

    if (nrow(row_to_change) > 0){
      res <- res %>%
        dplyr::filter(!madl_oral_reprotox %in% "700 (oral); 6700 (dermal)")
      res <- dplyr::bind_rows(res, row_to_change)
    }

    # N-Methylpyrrolidone - 17000 (dermal)
    row_to_change <- res %>%
      filter(madl_oral_reprotox == "17000 (dermal)") %>%
      dplyr::mutate(madl_dermal_reprotox = "17000",
                    madl_oral_reprotox=NA,
                    madl_dermal_reprotox_units=madl_oral_reprotox_units)

    if (nrow(row_to_change) > 0){
      res <- res %>%
        dplyr::filter(!madl_oral_reprotox %in% "17000 (dermal)")
      res <- dplyr::bind_rows(res, row_to_change)
    }

    # Small check for remaining parentheses or semicolons in this column
    flagged <- ifelse(grepl("\\(", res$madl_oral_reprotox), res$madl_oral_reprotox, NA)
    flagged[stats::complete.cases(flagged)]
    flagged <- ifelse(grepl(";", res$madl_oral_reprotox), res$madl_oral_reprotox, NA)
    flagged[stats::complete.cases(flagged)]
    # View(res %>% select(madl_oral_reprotox) %>% unique(), "after_madl_oral_fixes")

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
    res[,c("critical_effect", "species")] = "-"
    res$study_duration_class[is.na(res$study_duration_class)] <- "chronic"
    res$target_organ = "-"
    res$severity = "-"

    # Combine and transform into ToxVal fields
    res <- lapply(c("inhalation_unit_risk", "inhalation_slope_factor",
                     "oral_slope_factor",
                     "acute_rel", "rel_8_hour_inhalation",
                     "chronic_inhalation_rel", "chronic_oral_rel",
                     "mcl", "phg",
                     "nsrl_inhalation", "nsrl_oral",
                     "madl_inhalation_reprotox", "madl_oral_reprotox",
                     "madl_dermal_reprotox",
                     "chrfd", "notification_level"),
                   function(f_name){
                     message("...working on ", f_name)
                     tmp = res %>%
                       dplyr::select(name, casrn,
                                     year, critical_effect, species, study_duration_class,
                                     toxval_numeric = !!f_name,
                                     toxval_units = !!paste0(f_name, "_units"),
                                     `Human Data`) %>%
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
                                                      ifelse(grepl("dermal", f_name),
                                                            "dermal",
                                                            ifelse(f_name %in% c("mcl", "phg", "chrfd"),
                                                                  "oral",
                                                                  "TBD")
                                                      )
                                               )
                       ))

                     tryCatch({
                       tmp$toxval_numeric = as.numeric(tmp$toxval_numeric)
                     },
                     warning = function(e) {
                       # Only stop for unknown cases
                       if(!f_name %in% c("madl_oral_reprotox")){
                         stop(paste0("Found toxval_numeric case to handle for: ", f_name))
                       }
                    })

                     tmp$toxval_numeric = suppressWarnings(as.numeric(tmp$toxval_numeric))

                     # Special cases by toxval_type
                     if(f_name == "acute_rel"){
                       tmp$critical_effect = res$acute_rel_critical_effect
                       tmp$target_organ =  res$acute_rel_target_organ
                       tmp$severity =  res$acute_rel_severity
                       tmp$year = res$acute_rel_year
                       tmp$species = res$acute_rel_species
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
                       tmp$toxval_subtype = ifelse(is.na(res$toxval_subtype), "-", res$toxval_subtype)
                     } else if(f_name == "chrfd"){
                       tmp$toxval_type = "RfD"
                       tmp$toxval_subtype = "Child RfD"
                     } else if(f_name == "notification_level"){
                       tmp$toxval_type = "OEHHA notification level"
                       # Set study_type and exposure_route for OEHHA notification level
                       tmp$exposure_route = "oral"
                       # Replace toxval type that is already set within else if?
                       tmp$toxval_type = "drinking water quality guideline"
                       tmp$study_type = "chronic"
                     } else if(f_name == "madl_dermal_reprotox"){
                       tmp$exposure_route = "dermal"
                       tmp$toxval_type = "madl_dermal_reprotox"
                     }
                     return(tmp)
                   }) %>%
      dplyr::bind_rows() %>%
      # Filter out toxval_numeric NA values
      dplyr::filter(!is.na(toxval_numeric)) %>%
      # Clean up excess whitespace for all character fields
      dplyr::mutate(across(where(is.character), ~stringr::str_squish(.)),
                    severity = severity %>%
                      gsub("*", "", ., fixed=TRUE) %>%
                      tolower(),
                    critical_effect = paste(critical_effect, "|",
                                                target_organ, "|",
                                                stringr::str_to_title(severity)) %>%
                      # Clean up NA combined cases
                      gsub("\\| NA|NA \\|", "", .) %>%
                      stringr::str_squish(),
                    toxval_numeric_qualifier = "=",
                    `Human Data` = stringr::str_to_title(`Human Data`),
                    species = species %>% tolower()
                    ) %>%
      # Fix severity
      dplyr::distinct()

    # Replace NA with -
    res$critical_effect <- str_replace_all(res$critical_effect, "NA", "-")

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
