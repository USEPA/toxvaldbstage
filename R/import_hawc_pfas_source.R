#--------------------------------------------------------------------------------------
#' @title import_hawc_pfas_source
#' @description Load HAWC PFAS data into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param hawc_num The HAWC number being processed (e.g. 150, 430)
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @return None; data is added to toxval_source
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{case_when}}
#'  \code{\link[tidyselect]{all_of}}
#'  \code{\link[tidyr]{pivot_wider}}, \code{\link[tidyr]{unite}}, \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{drop_na}}, \code{\link[tidyr]{separate_rows}}
#'  \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_detect}}, \code{\link[stringr]{str_extract}}
#'  \code{\link[purrr]{map2}}
#'  \code{\link[digest]{digest}}
#' @rdname import_hawc_pfas_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate select distinct arrange count left_join filter rename case_when
#' @importFrom tidyselect all_of
#' @importFrom tidyr pivot_wider unite pivot_longer separate drop_na separate_rows
#' @importFrom stringr str_squish str_detect str_extract
#' @importFrom purrr map2_chr
#' @importFrom digest digest
#--------------------------------------------------------------------------------------
import_hawc_pfas_source <- function(db, hawc_num=NULL, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  if(is.null(hawc_num)) {
    stop("hawc_num parameter must have a value")
    return()
  }
  infile1 = paste0(toxval.config()$datapath,
                   "hawc_pfas_",hawc_num,"/hawc_pfas_",hawc_num,"_files/",
                   "hawc_pfas_",hawc_num,"_raw3.xlsx")
  infile2 = paste0(toxval.config()$datapath,
                   "hawc_pfas_",hawc_num,"/hawc_pfas_",hawc_num,"_files/",
                   "hawc_pfas_",hawc_num,"_doses3.xlsx")
  infile3 = paste0(toxval.config()$datapath,
                   "hawc_pfas_",hawc_num,"/hawc_pfas_",hawc_num,"_files/",
                   "hawc_pfas_",hawc_num,"_groups3.xlsx")

  source = paste0("HAWC PFAS ", hawc_num)
  source_table = paste0("source_hawc_pfas_", hawc_num)
  # Date provided by the source or the date the data was extracted
  if(source == "HAWC PFAS 150") {
    src_version_date = as.Date("2021-01-11")
  } else {
    # Date for HAWC PFAS 430
    src_version_date = as.Date("2021-06-08")
  }
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  # Build original_hawc_pfas table from source file
  res_pfas3 = readxl::read_xlsx(infile1, col_types = "text")

  res_pfas3 = res_pfas3 %>%
    dplyr::mutate(
      # All closed empty square bracket entries to - in effects
      effects = gsub("^\\[\\]$", "-", effects),

      # Strip square brackets from beginning and end
      animal_group.experiment.study.searches = gsub("\\]|\\[", "", animal_group.experiment.study.searches),
      animal_group.experiment.study.identifiers = gsub("\\]|\\[", "", animal_group.experiment.study.identifiers),

      #variations in NA to NA
      bmd_notes = gsub("[NA\\,\\\n]+", NA, bmd_notes),
      confidence_interval = gsub("[NA\\,\\\n]+", NA, confidence_interval),
      animal_group.experiment.study.block_id = gsub("[NA\\,\\\n]+", NA, animal_group.experiment.study.block_id)
    )

  # Read in dose data
  res_dose3 = readxl::read_xlsx(infile2, col_types = "text") %>%
    dplyr::select(tidyselect::all_of(c("dose_regime", "dose_group_id", "dose", "dose_units.name"))) %>%
    dplyr::distinct()

  # Read in dose groups
  res_groups3 <- openxlsx::read.xlsx(infile3) %>%
    dplyr::select(#endpoint,
      dose_group_id) %>%
    dplyr::distinct()

  res_groups3[] = lapply(res_groups3, as.character)

  # Select relevant columns
  hawc_pfas = res_pfas3 %>%
    dplyr::select(tidyselect::all_of(c("assessment","name" ,"organ","NOEL","LOEL",
                                       "FEL","url", "data_location",
                                       "bmd","animal_group.experiment.study.id","animal_group.experiment.study.title" ,"animal_group.experiment.study.authors_short",
                                       "animal_group.experiment.study.authors","animal_group.experiment.study.year","animal_group.experiment.study.journal",
                                       "animal_group.experiment.study.full_text_url","animal_group.experiment.study.short_citation","animal_group.experiment.study.full_citation",
                                       "animal_group.experiment.study.url","animal_group.experiment.name","animal_group.experiment.type",
                                       "animal_group.experiment.chemical","animal_group.experiment.cas","animal_group.experiment.chemical_source",
                                       "animal_group.experiment.vehicle","animal_group.experiment.guideline_compliance",
                                       "animal_group.dosing_regime.id","animal_group.dosing_regime.route_of_exposure",
                                       "animal_group.dosing_regime.duration_exposure","animal_group.dosing_regime.duration_exposure_text",
                                       "animal_group.species","animal_group.strain" ,"animal_group.sex","animal_group.name","animal_group.generation",
                                       "noel_names.noel","noel_names.loel")))

  # Create ordered dose_dict
  dose_dict <- res_dose3 %>%
    dplyr::left_join(res_groups3,
                     by="dose_group_id") %>%
    dplyr::arrange(dose_regime, dose_units.name, dose)

  # Maintain unaltered dose_dict
  dose_dict_orig = dose_dict

  # Get counts of dose entries per dose_regime - units pairs
  dose_dict = dose_dict %>%
    dplyr::select(dose_regime, dose_units.name, dose) %>%
    dplyr::distinct() %>%
    dplyr::count(dose_regime, dose_units.name) %>%
    dplyr::mutate(dose_units.name_n = paste0("(", n, ") ", dose_units.name)) %>%
    dplyr::select(-n) %>%
    dplyr::left_join(dose_dict, by=c("dose_regime", "dose_units.name")) %>%
    # Combine doses by regime unit groups
    tidyr::pivot_wider(id_cols = c("dose_regime", "dose_units.name", "dose_units.name_n"),
                       names_from = "dose_group_id",
                       values_from = "dose") %>%
    tidyr::unite("dose",
                 -dose_regime, -dose_units.name, -dose_units.name_n,
                 sep=", ", na.rm = TRUE) %>%
    dplyr::mutate(dose = gsub(", NA", "", dose))

  # Helper function to manage the multiple dosing weirdness but maintain value-unit pairs
  split_dose_dose_units <- function(r, before_r = TRUE){
    # Split the input strings
    r = r %>%
      strsplit(split = ";") %>%
      unlist() %>%
      stringr::str_squish()
    # Value vs. units are separated by "||", select which is needed, before/after "||"
    if(before_r){
      lapply(r, function(x){ sub('\\|\\|.*', '', x) }) %>%
        paste(collapse = "; ") %>%
        return()
    } else {
      lapply(r, function(x){ sub('.*\\|\\|', '', x) }) %>%
        paste(collapse = "; ") %>%
        return()
    }
  }

  # Sort out dose value - unit pairs for regimes with multiples
  doses = dose_dict %>%
    dplyr::select(dose_regime, dose, dose_units.name) %>%
    tidyr::unite("dose_dose_units", -dose_regime, sep = "||", na.rm = TRUE) %>%
    tidyr::pivot_wider(id_cols = dose_regime,
                       names_from = "dose_dose_units",
                       values_from = "dose_dose_units") %>%
    tidyr::unite("dose_dose_units", -dose_regime, sep="; ", na.rm = TRUE) %>%
    dplyr::mutate(dose_dose_units = gsub("; NA|NA; ", "", dose_dose_units)) %>%
    dplyr::mutate(dose = purrr::map2_chr(dose_dose_units, TRUE, split_dose_dose_units),
                  dose_units.name = purrr::map2_chr(dose_dose_units, FALSE, split_dose_dose_units)) %>%
    dplyr::select(-dose_dose_units)

  # Join/fill in dose and dose_units
  hawc_pfas$doses <- doses$dose[match(hawc_pfas$animal_group.dosing_regime.id,doses$dose_regime)]
  hawc_pfas$doses_units <- doses$dose_units.name[match(hawc_pfas$animal_group.dosing_regime.id,doses$dose_regime)]

  # Fix NOEL dict
  noel_dict = dose_dict_orig %>%
    dplyr::left_join(hawc_pfas %>%
                       dplyr::select(animal_group.dosing_regime.id, NOEL),
                     by = c("dose_regime"="animal_group.dosing_regime.id")) %>%
    dplyr::filter(NOEL == dose_group_id) %>%
    dplyr::select(-dose_group_id) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dose_regime, dose_units.name, dose)
  # Sort out the NOEL values and units by combining and splitting
  noel_values = noel_dict %>%
    tidyr::unite("dose_dose_units", -dose_regime, -NOEL, sep = "||", na.rm = TRUE) %>%
    tidyr::pivot_wider(id_cols = c("dose_regime", "NOEL"),
                       names_from = "dose_dose_units",
                       values_from = "dose_dose_units") %>%
    tidyr::unite("dose_dose_units", -dose_regime, -NOEL, sep="; ", na.rm=TRUE) %>%
    dplyr::mutate(dose_dose_units = gsub("; NA|NA; ", "", dose_dose_units)) %>%
    dplyr::mutate(dose = purrr::map2_chr(dose_dose_units, TRUE, split_dose_dose_units),
                  dose_units.name = purrr::map2_chr(dose_dose_units, FALSE, split_dose_dose_units)) %>%
    dplyr::select(-dose_dose_units)

  # Fix LOEL dict
  loel_dict = dose_dict_orig %>%
    dplyr::left_join(hawc_pfas %>%
                       dplyr::select(animal_group.dosing_regime.id, LOEL),
                     by = c("dose_regime"="animal_group.dosing_regime.id")) %>%
    dplyr::filter(LOEL == dose_group_id) %>%
    dplyr::select(-dose_group_id) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dose_regime, dose_units.name, dose)
  # Sort out the LOEL values and units by combining and splitting
  loel_values = loel_dict %>%
    tidyr::unite("dose_dose_units", -dose_regime, -LOEL, sep = "||", na.rm = TRUE) %>%
    tidyr::pivot_wider(id_cols = c("dose_regime", "LOEL"),
                       names_from = "dose_dose_units",
                       values_from = "dose_dose_units") %>%
    tidyr::unite("dose_dose_units", -dose_regime, -LOEL, sep="; ", na.rm = TRUE) %>%
    dplyr::mutate(dose_dose_units = gsub("; NA|NA; ", "", dose_dose_units)) %>%
    dplyr::mutate(dose = purrr::map2_chr(dose_dose_units, TRUE, split_dose_dose_units),
                  dose_units.name = purrr::map2_chr(dose_dose_units, FALSE, split_dose_dose_units)) %>%
    dplyr::select(-dose_dose_units)

  # Fix FEL dict
  fel_dict = dose_dict_orig %>%
    dplyr::left_join(hawc_pfas %>%
                       dplyr::select(animal_group.dosing_regime.id, FEL),
                     by = c("dose_regime"="animal_group.dosing_regime.id")) %>%
    dplyr::filter(FEL == dose_group_id) %>%
    dplyr::select(-dose_group_id) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dose_regime, dose_units.name, dose)
  # Sort out the FEL values and units by combining and splitting
  fel_values = fel_dict %>%
    tidyr::unite("dose_dose_units", -dose_regime, -FEL, sep = "||", na.rm=TRUE) %>%
    tidyr::pivot_wider(id_cols = c("dose_regime", "FEL"),
                       names_from = "dose_dose_units",
                       values_from = "dose_dose_units") %>%
    tidyr::unite("dose_dose_units", -dose_regime, -FEL, sep="; ", na.rm = TRUE) %>%
    dplyr::mutate(dose_dose_units = gsub("; NA|NA; ", "", dose_dose_units)) %>%
    dplyr::mutate(dose = purrr::map2_chr(dose_dose_units, TRUE, split_dose_dose_units),
                  dose_units.name = purrr::map2_chr(dose_dose_units, FALSE, split_dose_dose_units)) %>%
    dplyr::select(-dose_dose_units)

  # Match NOEL and LOEL values
  hawc_pfas$NOEL_values <- noel_values$dose[match(paste(hawc_pfas$animal_group.dosing_regime.id,hawc_pfas$NOEL),
                                                  paste(noel_values$dose_regime,noel_values$NOEL))]
  hawc_pfas$NOEL_units <-  noel_values$dose_units.name[match(paste(hawc_pfas$animal_group.dosing_regime.id,hawc_pfas$NOEL),
                                                             paste(noel_values$dose_regime,noel_values$NOEL))]
  hawc_pfas$LOEL_values <- loel_values$dose[match(paste(hawc_pfas$animal_group.dosing_regime.id,hawc_pfas$LOEL),
                                                  paste(loel_values$dose_regime,loel_values$LOEL))]
  hawc_pfas$LOEL_units <-  loel_values$dose_units.name[match(paste(hawc_pfas$animal_group.dosing_regime.id,hawc_pfas$LOEL),
                                                             paste(loel_values$dose_regime,loel_values$LOEL))]
  hawc_pfas$FEL_values <- fel_values$dose[match(paste(hawc_pfas$animal_group.dosing_regime.id,hawc_pfas$FEL),
                                                paste(fel_values$dose_regime,fel_values$FEL))]
  hawc_pfas$FEL_units <-  fel_values$dose_units.name[match(paste(hawc_pfas$animal_group.dosing_regime.id,hawc_pfas$FEL),
                                                           paste(fel_values$dose_regime,fel_values$FEL))]

  fac_cols <- sapply(hawc_pfas, is.factor)                          # Identify all factor columns
  hawc_pfas[fac_cols] <- lapply(hawc_pfas[fac_cols], as.character)  # Convert all factors to characters

  hawc_pfas = hawc_pfas %>%
    # Add url information
    dplyr::mutate(
      endpoint_url = paste("https://hawcprd.epa.gov", url, sep = ""),
      assessment_url = paste("https://hawcprd.epa.gov/assessment/", assessment, "/", sep = "")
    )

  # Rename columns
  names.list <- c("assessment","critical_effect","target_organ","NOEL_original","LOEL_original",
                  "FEL_original","endpoint_url_original", "data_location",
                  "bmd","study_id","title","authors_short","author","year","journal",
                  "full_text_url","short_ref","long_ref","study_url_original","experiment_name","experiment_type",
                  "name","casrn","chemical_source","media","guideline_compliance",
                  "dosing_regime_id","route_of_exposure","exposure_duration_value",
                  "exposure_duration_text","species","strain","sex","population","generation","noel_names","loel_names",
                  "doses", "doses_units", "NOEL_values","NOEL_units","LOEL_values",
                  "LOEL_units","FEL_values","FEL_units",
                  "record_url","source_url")
  names(hawc_pfas) <- names.list

  # Prep final data
  res = hawc_pfas %>%
    # Drop assessment column
    dplyr::select(-assessment) %>%

    # Add exposure and study_duration
    dplyr::mutate(
      exposure_route = route_of_exposure,
      exposure_method = route_of_exposure,
      study_duration_value = exposure_duration_value,
      study_duration_units = exposure_duration_text
    ) %>%

    # Rename columns as appropriate
    dplyr::rename(
      exposure_duration_value_original = exposure_duration_value,
      study_type = experiment_type
    ) %>%

    # Add final values
    dplyr::mutate(
      fel_names = "FEL",
      source = !!source,
      source_table = !!source_table,
      subsource = dplyr::case_when(
        source == "HAWC PFAS 150" ~ "PFAS 150 (2020)",
        source == "HAWC PFAS 430" ~ "PFAS 430 (2020)",
        TRUE ~ paste0("PFAS ", !!hawc_num)
      )
    ) %>%

    # Pivot the NOEL, LOEL, and FEL fields to long form
    tidyr::unite(noel_names, NOEL_values, NOEL_units, NOEL_original,
                 col="NOEL", sep="|") %>%
    tidyr::unite(loel_names, LOEL_values, LOEL_units, LOEL_original,
                 col="LOEL", sep="|") %>%
    tidyr::unite(fel_names, FEL_values, FEL_units, FEL_original,
                 col="FEL", sep="|") %>%
    tidyr::pivot_longer(cols = c("NOEL", "LOEL", "FEL"),
                        names_to = NULL,
                        values_to = "toxval_transform") %>%
    # Preserve the toxval_numeric dose index
    tidyr::separate(col = toxval_transform,
                    into = c("toxval_type", "toxval_numeric", "toxval_units", "toxval_numeric_dose_index"),
                    sep = "\\|") %>%
    dplyr::mutate(toxval_type = gsub("NA", NA, toxval_type),
                  toxval_numeric = gsub("NA", NA, toxval_numeric),
                  toxval_units = gsub("NA", NA, toxval_units)) %>%

    # Drop columns without toxval_numeric, toxval_type, or toxval_units
    tidyr::drop_na(toxval_numeric, toxval_type, toxval_units) %>%
    dplyr::filter(toxval_numeric != "-999",
                  toxval_units != "Normal") %>%
    dplyr::mutate(
      # Replace critical_effect field "-" with NA
      critical_effect = critical_effect %>% dplyr::na_if("-"),
      target_organ = target_organ %>% dplyr::na_if("-"),
      generation = generation %>% dplyr::na_if("-")
    ) %>%
    # Build critical_effect value in form generation: target_organ: critical_effect
    tidyr::unite("critical_effect", generation, target_organ, critical_effect,
                 sep = ": ",
                 na.rm=TRUE,
                 remove=FALSE)

  # # Collapse duplicated that just differ by critical effect
  # res2 = res %>%
  #   dplyr::select(-c("critical_effect","endpoint_url_original","record_url","target_organ"))
  #
  # res2$hashkey = NA
  # res$hashkey = NA
  # for(i in 1:nrow(res2)) {
  #   hashkey = digest::digest(paste0(res2[i,],collapse=""), serialize = FALSE)
  #   res2[i,"hashkey"] = hashkey
  #   res[i,"hashkey"] = hashkey
  # }
  # res2 = dplyr::distinct(res2)
  # res2$critical_effect = NA
  # for(i in 1:nrow(res2)) {
  #   hashkey = res2[[i,"hashkey"]]
  #   res3 = res[res$hashkey==hashkey,]
  #   x = res3$target_organ
  #   y = res3$critical_effect
  #   ce = ""
  #   for(j in 1:length(x)) ce=paste0(ce,x[j],":",y[j],"|")
  #   ce = substr(ce,1,(nchar(ce)-1))
  #   res2[i,"critical_effect"] = ce
  # }
  # res2$endpoint_url_original = NA
  # res2$record_url = NA
  # res2$target_organ = NA
  # res2 = res2[,!names(res2)%in%c("hashkey")]
  # res = res2

  # strip begining and ending quotation marks
  res <- as.data.frame(sapply(res, function(x) gsub("\"", "", x)))

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  # Handle relationship linkages
  relationship <- res %>%
    dplyr::filter(grepl(";", toxval_numeric),
                  grepl(";", toxval_units),
                  grepl(";", doses),
                  grepl(";", doses_units))
  # Check if any available
  if(nrow(relationship)){
    relationship = relationship %>%
      dplyr::mutate(relationship_id = 1:n())
  } else {
    # Empty dataframe with res cols to bind_rows()
    relationship = res[0,]
  }

  # Join back the relationship split rows
  res <- res %>%
    dplyr::filter(!grepl(";", toxval_numeric),
                  !grepl(";", toxval_units),
                  !grepl(";", doses),
                  !grepl(";", doses_units)) %>%
    dplyr::bind_rows(relationship)

  # Final transformations moved from load script
  res = res %>%
    # Separate entries with multiple corresponding toxval_numeric, toxval_units, and dose values
    tidyr::separate_rows(
      c(doses, doses_units, toxval_numeric, toxval_units),
      sep = ";"
    ) %>%

    dplyr::mutate(
      dplyr::across(c(doses, doses_units, toxval_numeric, toxval_units), stringr::str_squish),
      species = tolower(species),
      toxval_numeric = as.numeric(toxval_numeric),
      toxval_units = toxval_units %>% stringr::str_squish(),
      casrn = gsub("([a-zA-Z]+\\s+[a-zA-Z]*\\:*\\s*)(.*)", "\\2", casrn),

      # Fix exposure details
      exposure_route = gsub("(^[a-zA-Z]+)(\\s*.*)", "\\1", route_of_exposure) %>%
        tolower(),
      exposure_method = route_of_exposure %>%
        gsub("(^[a-zA-Z]+\\s*)(.*)", "\\2", .) %>%
        gsub("^\\-\\s+", "", .) %>%
        dplyr::na_if(., ""),

      # Fix fields related to study_duration
      study_duration_value = dplyr::case_when(
        # Remove "dose" entry
        grepl("dose", exposure_duration_text) & !grepl("[0-9](d|wk)\\s", exposure_duration_text) ~ as.character(NA),
        # Handle normal entries
        stringr::str_detect(exposure_duration_text, "[0-9]+\\s?(d|min|wk|h|hr|yr)s?$") ~ stringr::str_extract(exposure_duration_text,
                                                                                                              "([0-9]+)\\s?(d|min|wk|h|hr|yr)s?$",
                                                                                                              group=1),
        stringr::str_detect(exposure_duration_text, "[0-9]+\\s?(d|min|wk|h|hr|yr)s?\\s") ~ stringr::str_extract(exposure_duration_text,
                                                                                                                "([0-9]+)\\s?(d|min|wk|h|hr|yr)s?\\s",
                                                                                                                group=1),
        # Handle GD/LD/PND cases
        grepl("D", exposure_duration_text) ~ gsub(".+\\-[a-zA-Z]*", "", exposure_duration_text),
        # Return NA if no matches
        TRUE ~ as.character(NA)
      ) %>% gsub(",.+", "", .) %>% c() %>% as.numeric(),
      study_duration_units = dplyr::case_when(
        # Handle normal entries
        stringr::str_detect(exposure_duration_text, "[0-9]+\\s?(d|min|wk|h|hr|yr)s?$") ~ stringr::str_extract(exposure_duration_text,
                                                                                                              "([0-9]+)\\s?(d|min|wk|h|hr|yr)s?$",
                                                                                                              group=2),
        stringr::str_detect(exposure_duration_text, "[0-9]+\\s?(d|min|wk|h|hr|yr)s?\\s") ~ stringr::str_extract(exposure_duration_text,
                                                                                                                "([0-9]+)\\s?(d|min|wk|h|hr|yr)s?\\s",
                                                                                                                group=2),
        # Check for GD/LD/PND
        grepl("D", exposure_duration_text) ~ "days",
        # Return NA if no matches
        TRUE ~ as.character(NA)
      ),
      # Translate study_duration_units
      study_duration_units = dplyr::case_when(
        study_duration_units == "d" ~ "days",
        study_duration_units == "min" ~ "minutes",
        study_duration_units == "wk" ~ "weeks",
        study_duration_units %in% c("h", "hr") ~ "hours",
        TRUE ~ study_duration_units
      ),

      study_type = gsub("(^\\w+\\-*\\w*)(\\s*.*)", "\\1", study_type),
      # NA for duration values that mix PND, GD, and LD units
      study_duration_value = dplyr::case_when(
        grepl("PND", exposure_duration_text) & grepl("GD", exposure_duration_text) ~ NA,
        grepl("PND", exposure_duration_text) & grepl("LD", exposure_duration_text) ~ NA,
        grepl("GD", exposure_duration_text) & grepl("LD", exposure_duration_text) ~ NA,
        grepl("GD", exposure_duration_text) & grepl("PNW", exposure_duration_text) ~ NA,
        TRUE ~ study_duration_value
      ),
      # NA if NA value
      study_duration_units = dplyr::case_when(
        is.null(study_duration_value) ~ as.character(NA),
        is.na(study_duration_value) ~ as.character(NA),
        TRUE ~ study_duration_units
      )
    )

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"

  hashing_cols = c(toxval.config()$hashing_cols[!(toxval.config()$hashing_cols %in% c("critical_effect"))]# ,
                   # "dosing_regime_id",
                   # "experiment_url"
  )

  # Perform deduping
  res = toxval.source.import.dedup(res, hashing_cols=hashing_cols) %>%
    # Replace "|::|" in critical_effect with "|" delimiter
    dplyr::mutate(
      critical_effect = critical_effect %>%
        gsub(" \\|::\\| ", "|", .)
    )

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
