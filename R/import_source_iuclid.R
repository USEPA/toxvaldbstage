#--------------------------------------------------------------------------------------
#' @description Import IUCLID data to ToxVal Source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param subf The subfolder containing the IUCLID subsource
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_source_iuclid
#' @return None; data is sent to ToxVal
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{row_number}}, \code{\link[dplyr]{context}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{pull}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{select}}
#'  \code{\link[tidyr]{separate_rows}}, \code{\link[tidyr]{reexports}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}, \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{pivot_wider}}, \code{\link[tidyr]{drop_na}}
#'  \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_extract}}, \code{\link[stringr]{modifiers}} \code{\link[stringr]{str_detect}}
#'  \code{\link[tidyselect]{starts_with}}, \code{\link[tidyselect]{all_of}}
#'  \code{\link[textclean]{mgsub}}
#' @rdname import_source_iuclid
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr filter group_by mutate row_number n case_when pull rename select
#' @importFrom tidyr separate_rows separate unite pivot_longer starts_with pivot_wider drop_na matches
#' @importFrom stringr str_squish str_extract regex str_detect
#' @importFrom tidyselect starts_with any_of all_of
#' @importFrom textclean mgsub
#--------------------------------------------------------------------------------------
import_source_iuclid <- function(db, subf, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)

  # Add endpoint_uuid to hashing_cols to help resolve false duplicates due to replicate studies
  hashing_cols = c(toxval.config()$hashing_cols,
                   "endpoint_uuid")

  # Do not upload BasicToxicokinetics OHT data
  if (subf == "iuclid_basictoxicokinetics") {
    cat("Skipping BasicToxicokinetics OHT\n")
    return(0)
  }

  source = gsub("iuclid", "IUCLID", subf)
  source_table = paste0("source_", subf) %>% tolower()
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2023-08-01")
  dir = paste0(toxval.config()$datapath,"iuclid/",subf,"/",subf,"_files/")
  file = list.files(dir, pattern=".xlsx", full.names = TRUE)
  if(!length(file)) return(cat("...No files to process...\n"))
  if(length(file) > 1) stop("More than 1 IUCLID file stored in '", dir, "'")
  # guess_max used due to large file with some columns guessed as NA/logical when not
  res0 = readxl::read_xlsx(file, guess_max=21474836)

  if(!nrow(res0)){
    return("...No rows found in file...skipping")
  }

  # Load IUCLID field map
  map_orig = readxl::read_xlsx(paste0(toxval.config()$datapath,"iuclid/field_maps/iuclid_field_map.xlsx"))
  map = map_orig %>%
    dplyr::filter(oht == subf,
                  !grepl("not needed", notes))

  if(!nrow(map)) {
    return(paste0("No entries in IUCLID field map for: ", subf))
  }

  # Create a named vector to handle renaming from the map
  map = map %>%
    dplyr::filter(!is.na(to)) %>%
    tidyr::separate_rows(to, sep=" : ") %>%

    # Add "occurrence" stem to duplicate values
    dplyr::group_by(to) %>%
    dplyr::mutate(
      # Get nth time that field name has appeared so far
      appearance_num = dplyr::row_number(),

      # Get total number of appearances for that field name
      tot_appearances = dplyr::n(),

      # Add appearance_num stem when necessary
      to = dplyr::case_when(
        tot_appearances == 1 ~ to,
        TRUE ~ paste0(to, "_", appearance_num)
      )
    )

  tmp = map %>%
    dplyr::pull(from, to)

  # If "name" field exists in source data, rename to avoid conflicts
  if ("name" %in% names(res0)) {
    res0 = res0 %>% dplyr::rename(study_name = name)
  }

  res <- res0 %>%
    # Copy columns and rename new columns
    dplyr::rename(tidyselect::all_of(tmp)) %>%

    # Split columns and name them
    tidyr::separate(study_type_1, c("study_type_1","exposure_route"), sep=": ", fill="right", remove=TRUE)

  # If exposure_method column is present, fix it
  if ("exposure_method" %in% names(res)) {
    res = res %>%
      dplyr::mutate(
        exposure_method = gsub(".+:", "", exposure_method) %>%
          stringr::str_squish()
      )
  }

  # Unite duplicate columns with numbered stems
  for (field in map$to[grepl("_1", map$to) & map$to %in% names(res)]) {
    cat("...Combining duplicate column mapping: ", field, "\n")
    core_field = gsub("_1", "", field)
    res = res %>%
      tidyr::unite(
        col = !!core_field,
        tidyselect::starts_with(core_field),
        sep = "|",
        na.rm = TRUE
      )
  }

  # TODO Handle developmental fetus vs. maternal studies as needed
  if(grepl("developmental", subf) && any(grepl("fetus_|maternal_", names(res)))){
    message("Handling developmental OHT fetus vs. maternal field pivots...")
    # Fill default maternal sex
    res$maternal_sex = "female"
    res = res %>%
      # Get all maternal and fetus fields in one field
      tidyr::pivot_longer(cols=tidyselect::starts_with("fetus_") | tidyselect::starts_with("maternal_"),
                          names_to = "dev_field",
                          values_transform = list(value=as.character)) %>%
      # Split by maternal vs. fetus fields with "generation_type"
      tidyr::separate(dev_field, into=c("generation_type", "field"), sep="_", extra="merge") %>%
      # Spread out fields again, now without theif "fetus_" or "maternal_" prefixes (now stored in "generation_type")
      tidyr::pivot_wider(names_from = field, values_from=value)
  }

  # Add NA toxval_units_other column if it doesn't exist
  if (!("toxval_units_other" %in% names(res))) {
    res$toxval_units_other = as.character(NA)
  }

  # Handle name assignment (check if secondary name is supplied)
  if (!("name_secondary" %in% names(res))) {
    res$name = res$name_primary
  } else {
    res = res %>% dplyr::mutate(
      # OLD NAME LOGIC, WHERE FIRST NAME IN LIST IS SELECTED
      # # Create new name field using name_primary and name_secondary values
      # name = dplyr::case_when(
      #   # Primary exists: if semicolon does not separate different chemicals, use value as-is
      #   stringr::str_detect(name_primary, "\\[[^\\]]+;[^\\]]+\\]") ~ name_primary,
      #   # Primary exists: if semicolon separates different chemicals, choose the first
      #   grepl(";", name_primary) ~ gsub(";.+", "", name_primary),
      #   # Use primary name if it exists
      #   !is.na(name_primary) & name_primary != "-" ~ name_primary,
      #
      #   # Secondary exists: if semicolon does not separate different chemicals, use value as-is
      #   stringr::str_detect(name_secondary, "\\[[^\\]]+;[^\\]]+\\]") ~ name_secondary,
      #   # Secondary exists: if semicolon separates different chemicals, choose the first
      #   grepl(";", name_secondary) ~ gsub(";.+", "", name_secondary),
      #   # Use secondary name if it exists
      #   !is.na(name_secondary) & name_secondary != "-" ~ name_secondary,
      #
      #   # Return NA if there is not a valid name value
      #   TRUE ~ as.character(NA)
      # ),
      name = dplyr::case_when(
        # Primary name exists and is valid (not NA, -, or list)
        !is.na(name_primary) & name_primary != "-" & !grepl(";", name_primary) ~ name_primary,
        # Secondary name exists and is valid (not NA, -, or list)
        !is.na(name_secondary) & name_secondary != "-" & !grepl(";", name_secondary) ~ name_secondary,
        # Primary or secondary name is semicolon separated list
        grepl(";", name_primary) | grepl(";", name_secondary) ~ "DROP THIS NAME",
        # Simply return NA
        TRUE ~ as.character(NA)
      )
    )
  }

  # Before handling range relationships, check for unhandled edge cases
  if("toxval_numeric_lower" %in% names(res) & "toxval_numeric_upper" %in% names(res)) {
    # Check for unhandled edge cases
    # CASE 1: NA lower/upper with a toxval_numeric hyphen range
    # CASE 2: Nested range
    # Both cases handled in following logic:
    res = res %>%
      dplyr::mutate(
        edge_case_check = dplyr::case_when(
          grepl("\\-(?![eE])", toxval_numeric_lower, perl=TRUE) ~ as.character(toxval_numeric_lower),
          grepl("\\-(?![eE])", toxval_numeric_upper, perl=TRUE) ~ as.character(toxval_numeric_upper),
          TRUE ~ "PASSED"
        ),
        # Further translate values
        edge_case_check = dplyr::case_when(
          edge_case_check == "PASSED" ~ "PASSED",
          !is.na(suppressWarnings(as.numeric(edge_case_check))) ~ "PASSED",
          TRUE ~ "FAILED"
        )
      )
    if(nrow(res %>% dplyr::filter(edge_case_check == "FAILED")) > 0) {
      cat("\nRange relationship edge case identified\n")
      cat("Check for hyphenated range in toxval_numeric_lower/upper\n")
      # Export log file to review
      writexl::write_xlsx(res %>% dplyr::filter(edge_case_check == "FAILED"),
                          paste0(toxval.config()$datapath,"iuclid/",subf,"/",
                                 subf, "_edge_case_check.xlsx"))
      stop()
    }
    res = res %>% dplyr::select(-edge_case_check)

    # Combine toxval_numeric_lower and toxval_numeric_upper for relationship tracking
    res = res %>%
      tidyr::unite(
        "toxval_numeric",
        toxval_numeric_lower, toxval_numeric_upper,
        sep = "-",
        remove = TRUE,
        na.rm = TRUE
      )
  }

  # Handle case where toxval_numeric_qualifier is not supplied
  if(!("toxval_numeric_qualifier" %in% names(res))) {
    res$toxval_numeric_qualifier = as.character(NA)
  }
  # Handle case where only toxval_numeric_qualifier is supplied
  if(!("toxval_qualifier_upper" %in% names(res) & "toxval_qualifier_lower" %in% names(res))) {
    # Set both upper and lower qualifiers to single qualifier supplied
    res = res %>% dplyr::mutate(
      toxval_qualifier_upper = toxval_numeric_qualifier,
      toxval_qualifier_lower = toxval_numeric_qualifier
    )
  }

  # Separate toxval_numeric ranges, range_relationship_id created for Load toxval_relationship purposes
  ranged <- res %>%
    dplyr::filter(grepl("-(?![eE])", toxval_numeric, perl=TRUE),
                  !is.na(suppressWarnings(toxval_numeric)))
  # Check if any available
  if(nrow(ranged)){
    ranged = ranged %>%
      dplyr::mutate(range_relationship_id = 1:n()) %>%
      tidyr::separate_rows(toxval_numeric, sep="-") %>%
      dplyr::group_by(range_relationship_id) %>%
      dplyr::mutate(
        toxval_numeric = as.numeric(toxval_numeric),
        toxval_subtype = ifelse(toxval_numeric == min(toxval_numeric), "Lower Range", "Upper Range")
      ) %>%
      ungroup()
  } else {
    # Empty dataframe with res cols to bind_rows()
    ranged = res[0,]
  }

  # Join back the range split rows
  res <- res %>%
    dplyr::filter(!grepl("-(?![eE])", toxval_numeric, perl=TRUE),
                  !is.na(suppressWarnings(toxval_numeric))) %>%
    dplyr::mutate(toxval_numeric = as.numeric(toxval_numeric)) %>%
    dplyr::bind_rows(ranged)

  # Handle case where study_duration_class is not supplied
  if(!("study_duration_class" %in% names(res))) {
    res$study_duration_class = as.character(NA)
  }

  res = res %>%
    dplyr::mutate(
      # Fill "-" casrn with NA
      casrn = dplyr::na_if(casrn, "-"),

      # Clean toxval_units/make value substitutions when necessary
      toxval_units = dplyr::case_when(
        grepl("other:", toxval_units) ~ toxval_units_other,
        TRUE ~ toxval_units
      ) %>%
        gsub("diet", "", .) %>%
        gsub("air", "", .) %>%
        gsub("drinking water", "", .) %>%
        gsub("\\(.+\\)", "", .) %>%
        gsub("micro", "u", .) %>%
        gsub(" per ", "/", .) %>%
        stringr::str_squish()
    ) %>%
    # Filter out entries with inadequate toxval_type
    dplyr::filter(!grepl("dose|other", toxval_type)) %>%
    # Drop entries without necessary toxval columns
    tidyr::drop_na(toxval_numeric, toxval_units, toxval_type) %>%
    # Drop entries without either name or casrn
    dplyr::filter((name != "" | casrn != "")) %>%
    # Drop entries with no CASRN and inadequate name
    dplyr::filter((name != "[No public or meaningful name is available]" | casrn != "")) %>%
    # Drop entries with a list of names separated by semicolons (flagged earlier)
    dplyr::filter(name != "DROP THIS NAME") %>%
    # Remove CrossReference columns
    dplyr::select(-tidyr::matches("CrossReference.*.uuid|CrossReference.*.RelatedInformation")) %>%
    # Remove unused name columns
    dplyr::select(!tidyselect::any_of(c("name_primary", "name_secondary")))

  # If hashing columns still missing, fill them with "-" to avoid conflicts with later logic
  res[, hashing_cols[hashing_cols %in% names(res)]] <- "-"

  res = res %>%
    # Conduct most cleaning operations after dropping rows to improve runtime
    dplyr::mutate(
      # Clean critical_effect column
      critical_effect = critical_effect %>%
        gsub("Results:", "", ., ignore.case=TRUE) %>%
        stringr::str_squish(),

      # Extract study_duration_value and study_duration_units
      study_duration = study_duration_units,
      # Use first number appearance (range possible) as study_duration_value
      study_duration_value = study_duration %>%
        stringr::str_extract(stringr::regex(paste0("(\\d+(?:\\-\\d+)?).*?",
                                                   "(?:hour|\\bh\\b|[0-9]h\\b|",
                                                   "day|\\bd\\b|[0-9]d\\b|",
                                                   "week|\\bw\\b|[0-9]w\\b|wk|weeek|wwek|",
                                                   "month|\\bm\\b|[0-9]m\\b|",
                                                   "year|\\by\\b|[0-9]y\\b|yr)"),
                                            ignore_case = TRUE), group=1) %>%
        c() %>% stringr::str_squish(),
      # Use first "timeframe" appearance as study_duration_units
      study_duration_units = study_duration %>%
        stringr::str_extract(stringr::regex(paste0("\\d+(?:\\-\\d+)?.*?",
                                                   "(hour|\\bh\\b|[0-9]h\\b|",
                                                   "day|\\bd\\b|[0-9]d\\b|",
                                                   "week|\\bw\\b|[0-9]w\\b|wk|weeek|wwek|",
                                                   "month|\\bm\\b|[0-9]m\\b|",
                                                   "year|\\by\\b|[0-9]y\\b|yr)"),
                                            ignore_case = TRUE), group=1) %>%
        c(),
      # Perform final processing
      study_duration_units = dplyr::case_when(
        grepl("h", study_duration_units) ~ "hours",
        grepl("d", study_duration_units) ~ "days",
        grepl("w", study_duration_units) ~ "weeks",
        grepl("m", study_duration_units) ~ "months",
        grepl("y", study_duration_units) ~ "years",
        TRUE ~ as.character(NA)
      ),
      # Set both cols to NA if only one value is present
      study_duration_units = dplyr::case_when(
        study_duration_value == as.character(NA) ~ as.character(NA),
        TRUE ~ study_duration_units
      ),
      study_duration_value = dplyr::case_when(
        study_duration_units == as.character(NA) ~ as.character(NA),
        TRUE ~ study_duration_value
      ),

      # Select only first study_type value when pipe is used
      study_type = gsub("\\|.+", "", study_type),

      # Add "chronic" study_duration_class for T25 toxval_type
      study_duration_class = dplyr::case_when(
        toxval_type == "T25" ~ "chronic",
        TRUE ~ study_duration_class
      ),

      # Clean species column
      species = species %>%
        tolower() %>%
        gsub(":", "", .) %>%
        stringr::str_squish(),

      # Clean strain column
      strain = strain %>%
        tolower() %>%
        gsub("(?:animal )?strain:", "", ., ignore.case=TRUE) %>%
        gsub("WIST", "wistar", .) %>%
        gsub("no data", "", .) %>%
        gsub("other:?", "", .) %>%
        gsub("not specified", "", .) %>%
        gsub(":", ": ", .) %>%
        stringr::str_squish(),
      # After cleaning, further refine strain
      strain = dplyr::case_when(
        # Filter out entries too long to be a strain (generally study details)
        # Remove for now
        # nchar(strain) > 100 ~ as.character(NA),
        # Filter out entries with "age"
        grepl("age", strain, ignore.case=TRUE) ~ as.character(NA),
        TRUE ~ strain
      ),

      # Clean toxval_type
      toxval_type = toxval_type %>%
        gsub(":", "", .),

      # Extract and clean toxval_units
      toxval_units = dplyr::case_when(
        toxval_units == "other:" ~ toxval_units_other,
        TRUE ~ toxval_units
      ) %>%
        gsub("\\(.+\\)", "", .) %>%
        gsub("diet", "", .) %>%
        gsub("\\bin\\b", "", .) %>%
        gsub("drinking water", "", .) %>%
        stringr::str_squish(),

      # Clean exposure_method
      exposure_method = exposure_method %>%
        gsub("\\|other:|other:\\|", "", .) %>%
        gsub("\\|not specified|not specified\\|", "", .) %>%
        stringr::str_squish(),

      # Clean sex field
      sex = dplyr::case_when(
        grepl("no", sex) ~ as.character(NA),
        TRUE ~ sex
      ),

      # Ensure normal range for year
      year = dplyr::case_when(
        is.na(year) ~ NA,
        # Between 1800 and current year
        as.numeric(year) >= 1800 & as.numeric(year) <= as.numeric(format(Sys.Date(), "%Y")) ~ as.numeric(year),
        TRUE ~ NA
      ),

      # Set appropriate experimental flag
      experimental_flag = dplyr::case_when(
        experimental_flag == "experimental_study" ~ 1,
        TRUE ~ NA
      ) %>% as.numeric(),

      # Select and clean appropriate toxval_numeric_qualifier
      toxval_numeric_qualifier = dplyr::case_when(
        !is.na(toxval_qualifier_lower) ~ toxval_qualifier_lower,
        !is.na(toxval_qualifier_upper) ~ toxval_qualifier_upper,
        TRUE ~ toxval_numeric_qualifier
      ) %>% gsub("ca\\.", "~", .),

      # Add toxval_numeric_qualifier for range toxval_subtypes
      toxval_numeric_qualifier = dplyr::case_when(
        !is.na(toxval_numeric_qualifier) ~ toxval_numeric_qualifier,
        toxval_subtype == "Lower Range" ~ "<=",
        toxval_subtype == "Upper Range" ~ ">=",
        TRUE ~ toxval_numeric_qualifier
      ),

      # Ensure that toxval_numeric is of numeric type
      toxval_numeric = as.numeric(toxval_numeric),

      # Call fix.replace.unicode after previous cleaning operations to improve runtime
      name = fix.replace.unicode(name) %>%
        # Remove "no name" info
        gsub("\\|?\\[No public or meaningful name is available\\]\\|?", "", .) %>%
        stringr::str_squish(),
      critical_effect = fix.replace.unicode(critical_effect) %>%
        stringr::str_squish(),
      strain = fix.replace.unicode(strain) %>%
        stringr::str_squish(),
      toxval_units = fix.replace.unicode(toxval_units) %>%
        stringr::str_squish(),

      # Add fda_chem_id column and adjust name values accordingly
      fda_chem_id = dplyr::case_when(
        grepl("NDA[0-9]+\\-", name) ~ gsub("\\-.+", "", name),
        TRUE ~ as.character(NA)
      ),
      name = dplyr::case_when(
        grepl("NDA[0-9]+\\-", name) ~ gsub(".+\\-", "", name),
        TRUE ~ name
      )
    ) %>%

    # Drop unused toxval_qualifier cols
    dplyr::select(!tidyselect::any_of(c("toxval_qualifier_lower", "toxval_qualifier_upper"))) %>%
    # Remove entries with "conc. level" toxval_type or "%" toxval_units
    dplyr::filter(!grepl("conc\\. level", toxval_type),
                  !grepl("%", toxval_units))

  # Check for acute OHTs without a mapped duration field
  if(grepl("acute", subf, ignore.case = TRUE)){
    # Set duration to 1 day if not present in value/units
    if(!all(c("study_duration_value", "study_duration_units") %in% names(res))){
      res$study_duration_value = 1
      res$study_duration_units = "day"
    }
  }

  # Check for media column, or put as blank
  if(!"media" %in% names(res)){
    message("Media field is missing a mapping, defaulting to blank for now...")
    res$media = "-"
  }

  # Drop duplicates
  res = dplyr::distinct(res)

  # Standardize the names
  names(res) <- names(res) %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]|[\\(]|[\\)]", "_", .) %>%
    stringr::str_squish() %>%
    tolower() %>%
    # Truncate field names to abbreviated strings
    textclean::mgsub(.,
                     pattern = c("__", "administrativedata", "materialsandmethods", "administrationexposure", "administration",
                                 "materials", "resultsanddiscussion", "effectlevels", "system", "toxicity", "inhalation",
                                 "developmental", "maternal", "fetuses", "fetal", "results", "abnormalities", "animals",
                                 "fetus", "remarks", "details", "concentration", "observation", "examination", "material",
                                 "background", "publication", "description", "attach", "histopath", "observe", "overall",
                                 "docsforpub", "severity", "dermal", "incidence"
                     ),
                     replace = c("_", "addata", "matmet", "adexp", "ad",
                                 "mat", "resdisc", "efflvs", "sys", "tox", "inh",
                                 "dvmtl", "mtnl", "fts", "ftl", "res", "abnrm", "anim",
                                 "fts", "rmrk", "dtls", "conc", "obs", "exam", "mat",
                                 "bgrd", "pub", "desc", "atch", "hist", "obs", "ovrll",
                                 "pubdocs", "sev", "derm", "inc")) %>%
    gsub("targetsysorgantox_targetsysorgantox", "targetsysorgantox", .) %>%
    gsub("targetsysorgantox", "trgsysorgtox", .)

  # Halt if field names are still too long
  if(any(nchar(names(res)) >= 65)){
    message("Error: field names too long: ", names(res)[nchar(names(res)) >= 65] %>% toString())
    browser()
  }

  # Add version date. Can be converted to a mutate statement as needed
  res$source_version_date <- src_version_date

  # Check for duplicate records early
  res.temp = source_hash_vectorized(res, hashing_cols=hashing_cols)
  res$source_hash = res.temp$source_hash

  # Dedup by collapsing non hashing columns to dedup
  res = res %>%
    dplyr::group_by(source_hash) %>%
    dplyr::mutate(dplyr::across(-dplyr::any_of(c("source_hash", hashing_cols)),
                                ~paste0(., collapse=" |::| ") %>%
                                  na_if("NA"))) %>%
    # dplyr::summarise(linkage_id = toString(linkage_id)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()

  # # Check for immediate duplicate hashes
  # dup_hashes = res %>%
  #   dplyr::group_by(source_hash) %>%
  #   dplyr::summarise(n = dplyr::n()) %>%
  #   dplyr::filter(n > 1)
  #
  # # Stop if duplicate source_hash values present
  # if(nrow(dup_hashes)){
  #   cat("Duplicate source_hash values present in res...\n")
  #   # Export log file to review
  #   writexl::write_xlsx(res %>% dplyr::filter(source_hash %in% dup_hashes$source_hash),
  #                       paste0(toxval.config()$datapath,"iuclid/",subf,"/",
  #                              subf, "_dup_records_check.xlsx"))
  #   browser()
  #   stop("Duplicate source_hash values present in res...")
  # }

  #####################################################################
  cat("Load the data\n")
  #####################################################################
  source_prep_and_load(db=db,
                       source=source,
                       table=source_table,
                       res=res,
                       do.reset=do.reset,
                       do.insert=do.insert,
                       chem.check.halt=chem.check.halt,
                       hashing_cols=hashing_cols)
}
