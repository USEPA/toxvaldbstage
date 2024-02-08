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
#'  \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_extract}}, \code{\link[stringr]{modifiers}}
#'  \code{\link[tidyselect]{starts_with}}, \code{\link[tidyselect]{all_of}}
#'  \code{\link[textclean]{mgsub}}
#' @rdname import_source_iuclid
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr filter group_by mutate row_number n case_when pull rename select
#' @importFrom tidyr separate_rows separate unite pivot_longer starts_with pivot_wider drop_na matches
#' @importFrom stringr str_squish str_extract regex
#' @importFrom tidyselect starts_with any_of all_of
#' @importFrom textclean mgsub
#--------------------------------------------------------------------------------------
import_source_iuclid <- function(db, subf, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)

  # Do not upload BasicToxicokinetics OHT data
  if (subf == "iuclid_basictoxicokinetics") {
    cat("Skipping BasicToxicokinetics OHT\n")
    return(0)
  }

  source = gsub("iuclid", "IUCLID", subf)
  source_table = paste0("source_", subf) %>% tolower()
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

  res <- res0 %>%
    # Rename original "name" column to avoid renaming conflicts
    dplyr::rename(study_name = name) %>%

    # Copy columns and rename new columns
    dplyr::rename(tidyselect::all_of(tmp)) %>%

    # Split columns and name them
    tidyr::separate(study_type_1, c("study_type_1","exposure_route"), sep=": ", fill="right", remove=TRUE) %>%

    # Fix exposure_method column
    dplyr::mutate(
      exposure_method = gsub(".+:", "", exposure_method) %>%
        stringr::str_squish()
    )

  # Unite duplicate columns
  for (field in map$to) {
    if (field %in% names(res) & grepl("_1", field)) {
      core_field = gsub("_1", "", field)
      res = res %>%
        tidyr::unite(
          col = !!core_field,
          tidyselect::starts_with(core_field),
          sep = "|",
          na.rm = TRUE
        )
    }
  }

  # Handle developmental fetus vs. maternal studies
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

  res = res %>%
    # Handle toxval_numeric (use toxval_subtype to set upper or lower)
    tidyr::pivot_longer(
      cols = c("toxval_numeric_lower", "toxval_numeric_upper"),
      names_to = "toxval_subtype",
      values_to = "toxval_numeric"
    ) %>%
    dplyr::mutate(
      # Fill "-" name and casrn with NA
      name = dplyr::case_when(
        name == "-" ~ as.character(NA),
        TRUE ~ name
      ),
      casrn = dplyr::case_when(
        casrn == "-" ~ as.character(NA),
        TRUE ~ casrn
      ),

      # Clean toxval_units/make value substitutions when necessary
      toxval_units = dplyr::case_when(
        toxval_units == "other:" ~ toxval_units_other,
        TRUE ~ toxval_units
      ) %>%
        gsub("diet", "", .) %>%
        gsub("drinking water", "", .) %>%
        gsub("\\(.+\\)", "", .) %>%
        stringr::str_squish()
    ) %>%

    # Filter out entries with inadequate toxval_type
    dplyr::filter(!grepl("dose|other", toxval_type)) %>%
    # Drop entries without necessary toxval columns
    tidyr::drop_na(toxval_numeric, toxval_units, toxval_type) %>%
    # Drop entries without either name or casrn
    dplyr::filter((name != "" | casrn != "")) %>%
    # Combine columns and name them
    dplyr::select(-tidyr::matches("CrossReference.*.uuid|CrossReference.*.RelatedInformation")) %>%

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
                                                   "week|\\bw\\b|[0-9]w\\b|wk|weeek|wwek",
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
        gsub(":", ": ", .) %>%
        stringr::str_squish(),
      # After cleaning, further refine strain
      strain = dplyr::case_when(
        # Filter out entries too long to be a strain (generally study details)
        nchar(strain) > 40 ~ as.character(NA),
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
        gsub("in diet", "", .) %>%
        gsub("diet", "", .) %>%
        gsub("drinking water", "", .) %>%
        stringr::str_squish(),

      # Clean sex field
      sex = dplyr::case_when(
        grepl("no", sex) ~ as.character(NA),
        TRUE ~ sex
      ),

      # Ensure normal range for year
      year = dplyr::case_when(
        is.na(year) ~ NA,
        as.numeric(year) >= 1800 & as.numeric(year) <= 2024 ~ as.numeric(year),
        TRUE ~ NA
      ),

      # Select and clean appropriate toxval_numeric_qualifier
      toxval_numeric_qualifier = dplyr::case_when(
        toxval_subtype == "toxval_numeric_lower" ~ toxval_qualifier_lower,
        toxval_subtype == "toxval_numeric_upper" ~ toxval_qualifier_upper,
        TRUE ~ as.character(NA)
      ) %>% gsub("ca\\.", "~", .),

      # Add toxval_numeric_qualifier when not present in data (per Jira guidelines)
      toxval_numeric_qualifier = dplyr::case_when(
        !is.na(toxval_numeric_qualifier) ~ toxval_numeric_qualifier,
        toxval_subtype == "toxval_numeric_lower" ~ ">",
        toxval_subtype == "toxval_numeric_upper" ~ "<",
        TRUE ~ toxval_numeric_qualifier
      ),

      # Ensure that toxval_numeric is of numeric type
      toxval_numeric = as.numeric(toxval_numeric),

      # Call fix.replace.unicode after previous cleaning operations to improve runtime
      name = fix.replace.unicode(name),
      critical_effect = fix.replace.unicode(critical_effect),
      strain = fix.replace.unicode(strain)
    ) %>%

    # Drop unused toxval_qualifier cols
    dplyr::select(!tidyselect::any_of(c("toxval_qualifier_lower", "toxval_qualifier_upper")))

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
                                 "docsforpub", "severity"
                     ),
                     replace = c("_", "addata", "matmet", "adexp", "ad",
                                 "mat", "resdisc", "efflvs", "sys", "tox", "inh",
                                 "dvmtl", "mtnl", "fts", "ftl", "res", "abnrm", "anim",
                                 "fts", "rmrk", "dtls", "conc", "obs", "exam", "mat",
                                 "bgrd", "pub", "desc", "atch", "hist", "obs", "ovrll",
                                 "pubdocs", "sev")) %>%
    gsub("targetsysorgantox_targetsysorgantox", "targetsysorgantox", .) %>%
    gsub("targetsysorgantox", "trgsysorgtox", .)

  # Halt if field names are still too long
  if(any(nchar(names(res)) >= 65)){
    message("Error: field names too long: ", names(res)[nchar(names(res)) >= 65] %>% toString())
    browser()
  }

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"
  #####################################################################
  cat("Load the data\n")
  #####################################################################
  source_prep_and_load(db,
                       source=source,
                       table=source_table,
                       res=res,
                       do.reset=do.reset,
                       do.insert=do.insert,
                       chem.check.halt=chem.check.halt)
}
