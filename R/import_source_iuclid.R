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
#'  \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_extract}}, \code{\link[stringr]{modifiers}} \code{\link[stringr]{str_detect}} \code{\link[stringr]{str_split}} \code{\link[stringr]{str_unique}}
#'  \code{\link[tidyselect]{starts_with}}, \code{\link[tidyselect]{all_of}}
#'  \code{\link[textclean]{mgsub}}
#' @rdname import_source_iuclid
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr filter group_by mutate row_number n case_when pull rename select
#' @importFrom tidyr separate_rows separate unite pivot_longer starts_with pivot_wider drop_na matches
#' @importFrom stringr str_squish str_extract regex str_detect str_split str_unique
#' @importFrom tidyselect starts_with any_of all_of
#' @importFrom textclean mgsub
#--------------------------------------------------------------------------------------
import_source_iuclid <- function(db, subf, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  message("Import for: ", subf)

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

  # Try to open file (Windows file nesting issue)
  res0 = tryCatch({
    # guess_max used due to large file with some columns guessed as NA/logical when not
    readxl::read_xlsx(file, guess_max=21474836)
  },
  error = function(e) {
    message(e); return(data.frame())
  })

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
    # Handle consistency issues with study_duration mappings
    dplyr::mutate(
      to = dplyr::case_when(
        to == "study_duration : study_duration_units" ~ "study_duration_units",
        TRUE ~ to
      )
    ) %>%
    tidyr::separate_rows(to, sep=" : ") %>%
    # Sort rows to enforce combination order is consistent
    dplyr::arrange(to, from) %>%
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

  oht_field_map = map %>%
    dplyr::pull(from, to)

  # If "name" field exists in source data, rename to avoid conflicts
  if ("name" %in% names(res0)) {
    res0 = res0 %>% dplyr::rename(study_name = name)
  }

  # Special case for ec_number na_if() and report missing cases
  res0 = res0 %>%
    dplyr::mutate(dplyr::across(tidyr::any_of(c("chemical_ECnumber", "reference_substance_ECnumber")),
                                ~na_if(., "-")))

  if(!dir.exists(file.path(toxval.config()$datapath, "iuclid/ec_number_issue"))){
    dir.create(file.path(toxval.config()$datapath, "iuclid/ec_number_issue"))
  }
  # Export case of missing reference EC number but has chemical EC number
  ec_number_report = res0 %>%
    dplyr::filter(is.na(reference_substance_ECnumber), !is.na(chemical_ECnumber))

  if(nrow(ec_number_report)){
    writexl::write_xlsx(ec_number_report,
                        paste0(toxval.config()$datapath,
                               "iuclid/ec_number_issue/ec_number_issue_",
                               source_table, "_", Sys.Date(),".xlsx"))
  }

  # Old approach, did not account for multiple toxval fields mapped to same field
  # res <- res0 %>%
  #   # # Copy columns and rename new columns
  #   # dplyr::rename(dplyr::all_of(tmp)) %>%
  #   # Select only to mapped ToxVal fields
  #   dplyr::select(dplyr::any_of(tmp))

  res = res0
  # https://stackoverflow.com/questions/68959057/using-mutate-to-create-column-copies-using-a-named-vector
  # Required for cases where same field is mapped to multiple toxval fields (e.g., exposure_route and method)
  res[names(oht_field_map)] <- res[oht_field_map]
  res = res %>%
    dplyr::select(dplyr::any_of(c(names(oht_field_map), "endpoint_uuid")))

  # Split columns and name them (handle case where exposure_route is already provided)
  if("exposure_route" %in% names(res) | "exposure_route_1" %in% names(res)) {
    res = res %>% tidyr::separate(study_type_1, c("study_type_1","exposure_route_other"), sep=": ", fill="right", remove=TRUE)
  } else {
    res = res %>% tidyr::separate(study_type_1, c("study_type_1","exposure_route"), sep=": ", fill="right", remove=TRUE)
  }

  # Unite duplicate columns with numbered stems
  for (field in map$to[grepl("_1\\b", map$to) & map$to %in% names(res)]) {
    if(grepl("F1|F2|P0|P1", field)) next
    core_field = gsub("_1", "", field)
    cat("...Combining duplicate column mapping: ", core_field, "\n")
    res = res %>%
      tidyr::unite(
        col = !!core_field,
        tidyselect::starts_with(core_field),
        sep = "|",
        na.rm = TRUE
      )
  }

  # Handle developmental fetus vs. maternal studies as needed
  if(grepl("developmental", subf) && any(grepl("fetus_|maternal_", names(res)))){
    message("Handling developmental OHT fetus vs. maternal field pivots...")
    # Fill default maternal sex
    res$maternal_sex = "female"

    # Handle fetus and maternal fields separately, tracking origin with generation
    res_fetus = res %>%
      dplyr::select(-tidyselect::starts_with("maternal_")) %>%
      dplyr::mutate(generation="fetus") %>%
      dplyr::rename_with(function(x) gsub("fetus_", "", x))
    res_maternal = res %>%
      dplyr::select(-tidyselect::starts_with("fetus_")) %>%
      dplyr::mutate(generation="maternal") %>%
      dplyr::rename_with(function(x) gsub("maternal_", "", x))

    # Recombine fetus and maternal data
    res = dplyr::bind_rows(res_fetus, res_maternal)
  }

  # Handle different generations as needed for ToxicityReproduction OHT
  if(subf == "iuclid_toxicityreproduction"){
    message("Handling reproduction OHT generation field pivots...")
    # Handle generation fields separately, tracking origin with generation_class
    res0_p0 = res %>%
      dplyr::select(-tidyselect::starts_with("P1_")) %>%
      dplyr::select(-tidyselect::starts_with("F1_")) %>%
      dplyr::select(-tidyselect::starts_with("F2_")) %>%
      dplyr::mutate(
        # No generation mapping, so hardcode and set NA generation_details
        generation = "P0",
        generation_details = as.character(NA)
      )
    # Handle different column groupings to extract all data
    max_stem = NULL
    res_p0 = NULL
    for(name in names(res0_p0)) {
      if(!grepl("_[0-9]+", name)) next
      stem = as.numeric(stringr::str_extract(name, "_([0-9]+)", group=1))
      max_stem = max(stem, max_stem)
    }
    for(i in 0:max_stem) {
      curr_res = res0_p0 %>%
        dplyr::select(ends_with(paste0("_", !!i)) | !starts_with("P0")) %>%
        dplyr::rename_with(function(x) gsub("P0_", "", x)) %>%
        dplyr::rename_with(function(x) gsub("_[0-9]+", "", x))
      # Handle potential missing columns for rbind
      if(!("toxval_qualifier_upper") %in% names(curr_res)) curr_res$toxval_qualifier_upper = as.character(NA)
      if(!("toxval_numeric_upper") %in% names(curr_res)) curr_res$toxval_numeric_upper = as.character(NA)
      res_p0 = rbind(res_p0, curr_res)
    }

    res0_p1 = res %>%
      dplyr::select(-tidyselect::starts_with("P0_")) %>%
      dplyr::select(-tidyselect::starts_with("F1_")) %>%
      dplyr::select(-tidyselect::starts_with("F2_")) %>%
      dplyr::mutate(
        # No generation mapping, so hardcode and set NA generation_details
        generation = "P1",
        generation_details = as.character(NA)
      )
    # Handle different column groupings to extract all data
    max_stem = NULL
    res_p1 = NULL
    for(name in names(res0_p1)) {
      if(!grepl("_[0-9]+", name)) next
      stem = as.numeric(stringr::str_extract(name, "_([0-9]+)", group=1))
      max_stem = max(stem, max_stem)
    }
    for(i in 0:max_stem) {
      curr_res = res0_p1 %>%
        dplyr::select(ends_with(paste0("_", !!i)) | !starts_with("P1")) %>%
        dplyr::rename_with(function(x) gsub("P1_", "", x)) %>%
        dplyr::rename_with(function(x) gsub("_[0-9]+", "", x))
      # Handle potential missing columns for rbind
      if(!("toxval_qualifier_upper") %in% names(curr_res)) curr_res$toxval_qualifier_upper = as.character(NA)
      if(!("toxval_numeric_upper") %in% names(curr_res)) curr_res$toxval_numeric_upper = as.character(NA)
      res_p1 = rbind(res_p1, curr_res)
    }

    res0_f1 = res %>%
      dplyr::select(-tidyselect::starts_with("P0_")) %>%
      dplyr::select(-tidyselect::starts_with("P1_")) %>%
      dplyr::select(-tidyselect::starts_with("F2_"))
    # Handle different column groupings to extract all data
    max_stem = NULL
    res_f1 = NULL
    for(name in names(res0_f1)) {
      if(!grepl("_[0-9]+", name)) next
      stem = as.numeric(stringr::str_extract(name, "_([0-9]+)", group=1))
      max_stem = max(stem, max_stem)
    }
    for(i in 0:max_stem) {
      curr_res = res0_f1 %>%
        dplyr::select(ends_with(paste0("_", !!i)) | !starts_with("F1")) %>%
        dplyr::rename_with(function(x) gsub("F1_", "", x)) %>%
        dplyr::rename_with(function(x) gsub("_[0-9]+", "", x))
      # Handle potential missing columns for rbind
      if(!("toxval_qualifier_upper") %in% names(curr_res)) curr_res$toxval_qualifier_upper = as.character(NA)
      if(!("toxval_numeric_upper") %in% names(curr_res)) curr_res$toxval_numeric_upper = as.character(NA)
      res_f1 = rbind(res_f1, curr_res)
    }
    res_f1 = res_f1  %>% dplyr::mutate(
      # Extract generation_details from F1_generation
      generation_details = generation %>%
        gsub("F1|\\(|\\)|other:?", "", .) %>%
        stringr::str_squish(),
      # Set hardcoded generation
      generation = "F1"
    )

    res0_f2 = res %>%
      dplyr::select(-tidyselect::starts_with("P0_")) %>%
      dplyr::select(-tidyselect::starts_with("P1_")) %>%
      dplyr::select(-tidyselect::starts_with("F1_"))
    # Handle different column groupings to extract all data
    max_stem = NULL
    res_f2 = NULL
    for(name in names(res0_f2)) {
      if(!grepl("_[0-9]+", name)) next
      stem = as.numeric(stringr::str_extract(name, "_([0-9]+)", group=1))
      max_stem = max(stem, max_stem)
    }
    for(i in 0:max_stem) {
      curr_res = res0_f2 %>%
        dplyr::select(ends_with(paste0("_", !!i)) | !starts_with("F2")) %>%
        dplyr::rename_with(function(x) gsub("F2_", "", x)) %>%
        dplyr::rename_with(function(x) gsub("_[0-9]+", "", x))
      # Handle potential missing columns for rbind
      if(!("toxval_qualifier_upper") %in% names(curr_res)) curr_res$toxval_qualifier_upper = as.character(NA)
      if(!("toxval_numeric_upper") %in% names(curr_res)) curr_res$toxval_numeric_upper = as.character(NA)
      res_f2 = rbind(res_f2, curr_res)
    }
    res_f2 = res_f2 %>% dplyr::mutate(
      # Extract generation_details from F2_generation
      generation_details = generation %>%
        gsub("F2|\\(|\\)|other:?", "", .) %>%
        stringr::str_squish(),
      # Set hardcoded generation
      generation = "F2"
    )

    # Recombine data from different generations
    res = dplyr::bind_rows(res_p0, res_p1, res_f1, res_f2) %>%
      # Drop entries without generation
      dplyr::filter(!is.na(generation)) %>%
      dplyr::mutate(
        # Add generation_details to generation if necessary
        generation = dplyr::case_when(
          generation_details %in% c(as.character(NA), "", "-") ~ generation,
          TRUE ~ paste0(generation, " (", generation_details, ")")
        )
      ) %>%
      # Drop unused generation_details column
      dplyr::select(-generation_details)
  }

  # Add NA toxval_units_other column if it doesn't exist
  if (!("toxval_units_other" %in% names(res))) {
    res$toxval_units_other = as.character(NA)
  }
  # Add NA strain_other column if it doesn't exist
  if (!("strain_other" %in% names(res))) {
    res$strain_other = as.character(NA)
  }
  # Add NA exposure_route_other column if it doesn't exist
  if (!("exposure_route_other" %in% names(res))) {
    res$exposure_route_other = as.character(NA)
  }

  # Add special toxval_units "score" case for certain OHTs
  if (subf %in% c("iuclid_eyeirritation", "iuclid_skinirritationcorrosion", "iuclid_skinsensitisation")) {
    toxval_units = "score"
  }

  # Handle name assignment (check if secondary name is supplied)
  if (!("name_secondary" %in% names(res))) {
    res$name = res$name_primary
  } else {
    res = res %>% dplyr::mutate(
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
          edge_case_check %in% c("PASSED") ~ "PASSED",
          !is.na(suppressWarnings(as.numeric(edge_case_check))) ~ "PASSED",
          TRUE ~ "FAILED"
        )
      )
    if(nrow(res %>% dplyr::filter(edge_case_check %in% ("FAILED"))) > 0) {
      cat("\nRange relationship edge case identified\n")
      cat("Check for hyphenated range in toxval_numeric_lower/upper\n")
      # Export log file to review
      writexl::write_xlsx(res %>% dplyr::filter(edge_case_check %in% c("FAILED")),
                          paste0(toxval.config()$datapath,"iuclid/",subf,"/",
                                 subf, "_edge_case_check.xlsx"))
      stop()
    }
    res = res %>% dplyr::select(-edge_case_check)

    # Combine toxval_numeric_lower and toxval_numeric_upper for relationship tracking
    res = res %>%
      # Note origin to help with qualifier/relationship assignment
      dplyr::mutate(
        toxval_numeric_lower = dplyr::case_when(
          is.na(toxval_numeric_lower) ~ NA,
          TRUE ~ paste0(toxval_numeric_lower, " (Lower Range)")
        ),

        toxval_numeric_upper = dplyr::case_when(
          is.na(toxval_numeric_upper) ~ NA,
          TRUE ~ paste0(toxval_numeric_upper, " (Upper Range)")
        )
      ) %>%
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
        toxval_subtype = toxval_numeric %>%
          stringr::str_extract("Lower Range|Upper Range")
      ) %>%
      ungroup()
  } else {
    # Empty dataframe with res cols to bind_rows()
    ranged = res[0,] %>%
      dplyr::mutate(toxval_numeric = toxval_numeric)
  }

  # Join back the range split rows and set origin
  res <- res %>%
    dplyr::filter(!grepl("-(?![eE])", toxval_numeric, perl=TRUE),
                  !is.na(suppressWarnings(toxval_numeric))) %>%
    dplyr::bind_rows(ranged) %>%
    dplyr::mutate(toxval_numeric_origin = toxval_numeric %>%
                    stringr::str_extract("Lower Range|Upper Range"),
                  toxval_numeric = toxval_numeric %>%
                    gsub("\\(Lower Range\\)|\\(Upper Range\\)", "", .) %>%
                    stringr::str_squish() %>%
                    as.numeric())

  # Handle case where study_duration_class is not supplied
  if(!("study_duration_class" %in% names(res))) {
    res$study_duration_class = as.character(NA)
  }
  # Handle case where EC number is not supplied
  if(!("chemical.ec_number" %in% names(res))) {
    res$chemical.ec_number = as.character(NA)
  }

  res = res %>%
    dplyr::mutate(
      # Fill "-" casrn with NA and address date-formatted casrn values
      casrn = dplyr::case_when(
        casrn == "-" ~ as.character(NA),
        # Special case where "/" in casrn
        grepl("/", casrn) ~ paste0(stringr::str_split_i(casrn, "/", 3),
                                   "-",
                                   stringr::str_pad(stringr::str_split_i(casrn, "/", 1), width=2, side="left", pad="0"),
                                   "-",
                                   stringr::str_split_i(casrn, "/", 2)),
        TRUE ~ casrn
      ),

      # Clean toxval_units/make value substitutions when necessary
      toxval_units = dplyr::case_when(
        grepl("other:", toxval_units) ~ toxval_units_other,
        TRUE ~ toxval_units
      ) %>%
        gsub("diet", "", .) %>%
        gsub("air", "", .) %>%
        gsub("drinking water", "", .) %>%
        gsub("sediment", "", .) %>%
        gsub("\\(.+\\)", "", .) %>%
        gsub("micro", "u", .) %>%
        gsub(" per ", "/", .) %>%
        stringr::str_squish(),
    ) %>%
    # Collapse duplicate EC numbers
    dplyr::rowwise() %>%
    dplyr::mutate(
      chemical.ec_number = stringr::str_split(string=chemical.ec_number, pattern="\\|") %>%
        .[[1]] %>%
        stringr::str_unique() %>%
        paste0(collapse="|")
    ) %>%
    dplyr::ungroup() %>%

    # Filter out entries with differing EC numbers
    dplyr::filter(!grepl("\\|", chemical.ec_number)) %>%
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
  res[, hashing_cols[!hashing_cols %in% names(res)]] <- "-"

  res = res %>%
    # Conduct most cleaning operations after dropping rows to improve runtime
    dplyr::mutate(
      # Clean critical_effect column
      critical_effect = critical_effect %>%
        gsub("Results:|other:|not specified", "", ., ignore.case=TRUE) %>%
        stringr::str_squish() %>%
        dplyr::na_if("") %>%
        dplyr::na_if(" ") %>%
        dplyr::na_if(":") %>%
        dplyr::na_if("table"),

      # Add fields to track PND/GD units (units removed from string to improve handling)
      pnd_or_gd = dplyr::case_when(
        grepl("PND|post\\-?natal day", study_duration_units, ignore.case=TRUE) ~ "PND",
        grepl("GD|gestation day", study_duration_units, ignore.case=TRUE) ~ "GD",
        TRUE ~ as.character(NA)
      ),

      # Extract study_duration_value and study_duration_units
      study_duration_raw = study_duration_units,
      study_duration = study_duration_raw %>%
        gsub(",", "", .) %>%
        tolower(),
      study_duration = dplyr::case_when(
        grepl("were dosed [0-9]+\\.?[0-9]* days at a minimum",
              study_duration_raw) ~ stringr::str_extract(study_duration_raw,
                                                         "were dosed ([0-9]+\\.?[0-9]* days) at a minimum",
                                                         group=1),
        grepl(paste0("[0-9]+\\.?[0-9]* consecutive ",
                     "(?:hour|\\bh\\b|[0-9\\.]h\\b|",
                     "day|\\bd\\b|[0-9\\.]d\\b|",
                     "week|\\bw\\b|[0-9\\.]w\\b|wk|weeek|wwek|",
                     "month|\\bm\\b|[0-9\\.]m\\b|",
                     "year|\\by\\b|[0-9\\.]y\\b|yr|min\\b)"),
              study_duration_raw,
              ignore.case=TRUE) ~ stringr::str_extract(study_duration_raw,
                                                       paste0("[0-9]+\\.?[0-9]* consecutive ",
                                                              "(?:hour|\\bh\\b|[0-9\\.]h\\b|",
                                                              "day|\\bd\\b|[0-9\\.]d\\b|",
                                                              "week|\\bw\\b|[0-9\\.]w\\b|wk|weeek|wwek|",
                                                              "month|\\bm\\b|[0-9\\.]m\\b|",
                                                              "year|\\by\\b|[0-9\\.]y\\b|yr|min\\b)")),
        grepl("observations then continued for", study_duration_raw) ~ gsub("observations then continued for.+", "", study_duration_raw),
        grepl("for a minimum of", study_duration_raw) ~ stringr::str_extract(study_duration_raw,
                                                                                  paste0("for a minimum of [0-9]+\\.?[0-9]*.+",
                                                                                         "(?:hour|\\bh\\b|[0-9\\.]h\\b|",
                                                                                         "day|\\bd\\b|[0-9\\.]d\\b|",
                                                                                         "week|\\bw\\b|[0-9\\.]w\\b|wk|weeek|wwek|",
                                                                                         "month|\\bm\\b|[0-9\\.]m\\b|",
                                                                                         "year|\\by\\b|[0-9\\.]y\\b|yr|min\\b)")),
        grepl(paste0("\\bfor up to\\b.*[0-9]+\\.?[0-9]*.*(?:hour|\\bh\\b|[0-9\\.]h\\b|",
                     "day|\\bd\\b|[0-9\\.]d\\b|",
                     "week|\\bw\\b|[0-9\\.]w\\b|wk|weeek|wwek|",
                     "month|\\bm\\b|[0-9\\.]m\\b|",
                     "year|\\by\\b|[0-9\\.]y\\b|yr|min\\b)"),
              study_duration_raw,
              ignore.case=TRUE) ~ gsub(paste0(".*\\bfor up to\\b(.*[0-9]+\\.?[0-9]*.*(?:hour|\\bh\\b|[0-9\\.]h\\b|",
                                              "day|\\bd\\b|[0-9\\.]d\\b|",
                                              "week|\\bw\\b|[0-9\\.]w\\b|wk|weeek|wwek|",
                                              "month|\\bm\\b|[0-9\\.]m\\b|",
                                              "year|\\by\\b|[0-9\\.]y\\b|yr|min\\b))"), "\\1", study_duration_raw),
        grepl(paste0("\\bfor\\b.*[0-9]+\\.?[0-9]*.*(?:hour|\\bh\\b|[0-9\\.]h\\b|",
                     "day|\\bd\\b|[0-9\\.]d\\b|",
                     "week|\\bw\\b|[0-9\\.]w\\b|wk|weeek|wwek|",
                     "month|\\bm\\b|[0-9\\.]m\\b|",
                     "year|\\by\\b|[0-9\\.]y\\b|yr|min\\b)"),
              study_duration_raw,
              ignore.case=TRUE) ~ gsub(paste0(".*\\bfor\\b(.*[0-9]+\\.?[0-9]*.*(?:hour|\\bh\\b|[0-9\\.]h\\b|",
                                              "day|\\bd\\b|[0-9\\.]d\\b|",
                                              "week|\\bw\\b|[0-9\\.]w\\b|wk|weeek|wwek|",
                                              "month|\\bm\\b|[0-9\\.]m\\b|",
                                              "year|\\by\\b|[0-9\\.]y\\b|yr|min\\b))"), "\\1", study_duration_raw),
        grepl(paste0("\\bin\\b.*[0-9]+\\.?[0-9]*.*(?:hour|\\bh\\b|[0-9\\.]h\\b|",
                     "day|\\bd\\b|[0-9\\.]d\\b|",
                     "week|\\bw\\b|[0-9\\.]w\\b|wk|weeek|wwek|",
                     "month|\\bm\\b|[0-9\\.]m\\b|",
                     "year|\\by\\b|[0-9\\.]y\\b|yr|min\\b)"),
              study_duration_raw,
              ignore.case=TRUE) ~ gsub(paste0(".*\\bin\\b(.*[0-9]+\\.?[0-9]*.*(?:hour|\\bh\\b|[0-9\\.]h\\b|",
                                              "day|\\bd\\b|[0-9\\.]d\\b|",
                                              "week|\\bw\\b|[0-9\\.]w\\b|wk|weeek|wwek|",
                                              "month|\\bm\\b|[0-9\\.]m\\b|",
                                              "year|\\by\\b|[0-9\\.]y\\b|yr|min\\b))"), "\\1", study_duration_raw),
        grepl(paste0("\\bover a period of\\b.*[0-9]+\\.?[0-9]*.*(?:hour|\\bh\\b|[0-9\\.]h\\b|",
                     "day|\\bd\\b|[0-9\\.]d\\b|",
                     "week|\\bw\\b|[0-9\\.]w\\b|wk|weeek|wwek|",
                     "month|\\bm\\b|[0-9\\.]m\\b|",
                     "year|\\by\\b|[0-9\\.]y\\b|yr|min\\b)"),
              study_duration_raw,
              ignore.case=TRUE) ~ gsub(paste0(".*\\bover a period of\\b(.*[0-9]+\\.?[0-9]*.*(?:hour|\\bh\\b|[0-9\\.]h\\b|",
                                              "day|\\bd\\b|[0-9\\.]d\\b|",
                                              "week|\\bw\\b|[0-9\\.]w\\b|wk|weeek|wwek|",
                                              "month|\\bm\\b|[0-9\\.]m\\b|",
                                              "year|\\by\\b|[0-9\\.]y\\b|yr|min\\b))"), "\\1", study_duration_raw),
        TRUE ~ study_duration_raw
      ) %>%
        gsub("0,5 h\\b", "0.5 h", .) %>%
        gsub("through", "-", .) %>%
        gsub("birth to\\b", "", .) %>%
        gsub("([0-9]+\\.?[0-9]*)(h|d|w|m|y)", "\\1 \\2", ., ignore.case=TRUE) %>%
        gsub("\\s*\\-\\s*", "-", .) %>%
        gsub("\\bseveral\\b", "3", ., ignore.case=TRUE) %>%
        gsub("\\btwenty\\s?\\-?\\s?four\\b", "24", ., ignore.case=TRUE) %>%
        gsub("\\beleven\\b", "11", ., ignore.case=TRUE) %>%
        gsub("\\btwelve\\b", "12", ., ignore.case=TRUE) %>%
        gsub("\\bthirteen\\b", "13", ., ignore.case=TRUE) %>%
        gsub("\\bfourteen\\b", "14", ., ignore.case=TRUE) %>%
        gsub("\\bfifteen\\b", "15", ., ignore.case=TRUE) %>%
        gsub("\\bsixteen\\b", "16", ., ignore.case=TRUE) %>%
        gsub("\\bseventeen\\b", "17", ., ignore.case=TRUE) %>%
        gsub("\\beighteen\\b", "18", ., ignore.case=TRUE) %>%
        gsub("\\bnineteen\\b", "19", ., ignore.case=TRUE) %>%
        gsub("\\btwenty\\b", "20", ., ignore.case=TRUE) %>%
        gsub("\\bone\\b", "1", ., ignore.case=TRUE) %>%
        gsub("\\btwo\\b", "2", ., ignore.case=TRUE) %>%
        gsub("\\bthree\\b", "3", ., ignore.case=TRUE) %>%
        gsub("\\bfour\\b", "4", ., ignore.case=TRUE) %>%
        gsub("\\bfive\\b", "5", ., ignore.case=TRUE) %>%
        gsub("\\bsix\\b", "6", ., ignore.case=TRUE) %>%
        gsub("\\bseven\\b", "7", ., ignore.case=TRUE) %>%
        gsub("\\beight\\b", "8", ., ignore.case=TRUE) %>%
        gsub("\\bnine\\b", "9", ., ignore.case=TRUE) %>%
        gsub("\\bten\\b", "10", ., ignore.case=TRUE) %>%
        gsub("\\-\\s*week", " week", ., ignore.case=TRUE) %>%
        gsub("\\-\\s*hour", " hour", ., ignore.case=TRUE) %>%
        gsub("\\-\\s*day", " day", ., ignore.case=TRUE) %>%
        gsub("\\-\\s*month", " month", ., ignore.case=TRUE) %>%
        gsub("\\-\\s*year", " year", ., ignore.case=TRUE) %>%
        gsub("\\bto\\b", "-", ., ignore.case=TRUE) %>%
        gsub("PND|GD", "", ., ignore.case=TRUE) %>%
        gsub("[0-9]+\\.?[0-9]*\\s*\\(?(?:mg|kg|ppm|mg\\kg)\\)?,?", "", .) %>%
        gsub("\\s*\\-\\s*", "-", .) %>%
        gsub("\\bhr|hopur|Hr|\\bhs", "hour", .) %>%
        gsub("FR\\-513", "", .) %>%
        gsub("1 single exposure", "", .) %>%
        stringr::str_squish(),
      # Use first number appearance (range possible) as study_duration_value
      study_duration_value = study_duration %>%
        stringr::str_extract(stringr::regex(paste0("([0-9]+\\.?[0-9]*(?:\\-[0-9]+\\.?[0-9]*)?).*?",
                                                   "(?:hour|\\bh\\b|[0-9\\.]h\\b|",
                                                   "day|\\bd\\b|[0-9\\.]d\\b|",
                                                   "week|\\bw\\b|[0-9\\.]w\\b|wk|weeek|wwek|",
                                                   "month|\\bm\\b|[0-9\\.]m\\b|",
                                                   "year|\\by\\b|[0-9\\.]y\\b|yr|min\\b)"),
                                            ignore_case = TRUE), group=1) %>%
        c() %>% stringr::str_squish(),
      # Use first "timeframe" appearance as study_duration_units
      study_duration_units = study_duration %>%
        stringr::str_extract(stringr::regex(paste0("[0-9]+\\.?[0-9]*(?:\\-[0-9]+\\.?[0-9]*)?.*?",
                                                   "(hour|\\bh\\b|[0-9\\.]h\\b|",
                                                   "day|\\bd\\b|[0-9\\.]d\\b|",
                                                   "week|\\bw\\b|[0-9\\.]w\\b|wk|weeek|wwek|",
                                                   "month|\\bm\\b|[0-9\\.]m\\b|",
                                                   "year|\\by\\b|[0-9\\.]y\\b|yr|min\\b)"),
                                            ignore_case = TRUE), group=1) %>%
        c(),
      # Perform final processing
      study_duration_units = dplyr::case_when(
        grepl("min", study_duration_units, ignore.case=TRUE) ~ "minutes",
        grepl("m", study_duration_units, ignore.case=TRUE) ~ "months",
        grepl("h", study_duration_units, ignore.case=TRUE) ~ "hours",
        grepl("d", study_duration_units, ignore.case=TRUE) ~ "days",
        grepl("w", study_duration_units, ignore.case=TRUE) ~ "weeks",
        grepl("y", study_duration_units, ignore.case=TRUE) ~ "years",
        TRUE ~ as.character(NA)
      ),

      # If study_duration still not extracted, check for PND/GD
      study_duration_value = dplyr::case_when(
        !is.na(study_duration_value) | is.na(pnd_or_gd) ~ study_duration_value,
        TRUE ~ stringr::str_extract(study_duration, "[0-9]+\\.?[0-9]*\\s*\\-?\\s*[0-9\\.]*") %>% c() %>% stringr::str_squish()
      ),
      study_duration_units = dplyr::case_when(
        !is.na(study_duration_units) | is.na(pnd_or_gd) ~ study_duration_units,
        TRUE ~ pnd_or_gd
      ),

      # Set both cols to NA if only one value is present, study_duration_value is 0, or PND edge case
      study_duration_units = dplyr::case_when(
        study_duration_value == as.character(NA) ~ as.character(NA),
        study_duration_value == "0" ~ as.character(NA),
        grepl("mating to PND", study_duration_raw) ~ as.character(NA),
        TRUE ~ study_duration_units
      ),
      study_duration_value = dplyr::case_when(
        study_duration_units == as.character(NA) ~ as.character(NA),
        study_duration_value == "0" ~ as.character(NA),
        grepl("mating to PND", study_duration_raw) ~ as.character(NA),
        TRUE ~ study_duration_value
      ) %>% gsub("\\s*\\-\\s*", "-", .),

      # Select only first study_type value when pipe is used
      study_type = gsub("\\|.+", "", study_type),

      # Add "chronic" study_duration_class for T25 toxval_type
      study_duration_class = dplyr::case_when(
        toxval_type %in% c("T25") ~ "chronic",
        TRUE ~ study_duration_class
      ),

      # Clean strain column
      strain = dplyr::case_when(
        grepl("hamster,", species, ignore.case=TRUE) ~ gsub("hamster,", "", species, ignore.case=TRUE),
        grepl("\\bother:?", strain) ~ strain_other,
        TRUE ~ strain
      ) %>%
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
        grepl("\\bage\\b|lot #|weeks old|substance", strain, ignore.case=TRUE) ~ as.character(NA),
        TRUE ~ strain
      ),

      # Clean species column
      species = dplyr::case_when(
        grepl("hamster,", species, ignore.case=TRUE) ~ gsub(",.+", "", species, ignore.case=TRUE),
        TRUE ~ species
      ) %>%
        tolower() %>%
        gsub(":", "", .) %>%
        stringr::str_squish(),

      # Clean toxval_type
      toxval_type = toxval_type %>%
        gsub(":", "", .),

      # Extract and clean toxval_units
      toxval_units = dplyr::case_when(
        toxval_units %in% c("other:") ~ toxval_units_other,
        TRUE ~ toxval_units
      ) %>%
        gsub("\\(.+\\)", "", .) %>%
        gsub("diet", "", .) %>%
        gsub("\\bin\\b", "", .) %>%
        gsub("drinking water", "", .) %>%
        stringr::str_squish(),

      # Clean exposure_method
      exposure_method =  dplyr::case_when(
        !(grepl(":", exposure_method)) ~ gsub(".+\\|", "", exposure_method),
        TRUE ~ gsub(".+:", "", exposure_method)
      ) %>%
        stringr::str_replace("^\\|", "") %>%
        gsub("\\|?\\bother\\b\\|?", "", .) %>%
        gsub("\\|?\\bunspecified\\b\\|?", "", .) %>%
        gsub("\\|?not specified|not specified\\|?", "", .) %>%
        stringr::str_squish(),
      exposure_method = dplyr::case_when(
        !(exposure_method %in% c("-", as.character(NA), "")) ~ exposure_method,
        grepl("gavage", exposure_route) ~ "gavage",
        grepl("drinking water", exposure_route) ~ "drinking water",
        grepl("feed", exposure_route) ~ "feed",
        grepl("capsule", exposure_route) ~ "capsule",
        TRUE ~ exposure_method
      ) %>%
        gsub("mixture of.+\\|", "", .) %>%
        gsub("\\|?(?:gas|vapour|aerosol|dust)\\|?", "", ., ignore.case=TRUE) %>%
        stringr::str_replace("^\\||\\|$", "") %>%
        stringr::str_squish(),

      # Extract exposure_form
      exposure_form = dplyr::case_when(
        grepl("mixture", exposure_route) ~ stringr::str_extract(exposure_route, "mixture.+") %>% c(),
        grepl("gas", exposure_route) ~ "gas",
        grepl("vapour", exposure_route) ~ "vapour",
        grepl("aerosol", exposure_route) ~ "aerosol",
        grepl("dust", exposure_route) ~ "dust",
        TRUE ~ exposure_form
      ),

      # Clean exposure_route
      exposure_route = dplyr::case_when(
        grepl("\\bother|\\bunspecified|\\bnot specified", gsub(":.+", "", exposure_route)) ~ exposure_route_other,
        exposure_route %in% c(as.character(NA), "-", "", "other route", "other routes") ~ exposure_route_other,
        TRUE ~ exposure_route
      ) %>%
        gsub(":.+", "", .) %>%
        gsub("gavage|gas|vapour|drinking water|feed|aerosol|capsule|mixture.+|dust|other routes?|acute|(?:sub)?chronic", "", ., ignore.case=TRUE) %>%
        stringr::str_squish(),

      # Handle edge case where exposure_method equals exposure_route
      exposure_method = dplyr::case_when(
        exposure_method == exposure_route ~ as.character(NA),
        TRUE ~ exposure_method
      ),

      # Clean sex field
      sex = dplyr::case_when(
        !grepl("male", sex) ~ as.character(NA),
        TRUE ~ sex
      ) %>% gsub("not specified", "", .),

      # Ensure normal range for year
      year = dplyr::case_when(
        is.na(year) ~ NA,
        # Between 1800 and current year
        as.numeric(year) >= 1800 & as.numeric(year) <= as.numeric(format(Sys.Date(), "%Y")) ~ as.numeric(year),
        TRUE ~ NA
      ),

      # Add temp column to decide which rows to drop relating to experimental_flag/data_purpose_category
      temp_to_drop = dplyr::case_when(
        data_purpose_category %in% c("disregarded due to major methodological deficiencies") ~ 1,
        experimental_flag %in% c("experimental study") ~ 0,
        data_purpose_category %in% c("key study", "supporting study", "weight of evidence") ~ 0,
        TRUE ~ 1
      ),

      # Set appropriate experimental flag
      experimental_flag = dplyr::case_when(
        experimental_flag %in% c("experimental study") ~ 1,
        TRUE ~ NA
      ) %>% as.numeric(),

      # Select and clean appropriate toxval_numeric_qualifier
      toxval_numeric_qualifier = dplyr::case_when(
        !is.na(toxval_qualifier_lower) & toxval_numeric_origin == "Lower Range" ~ toxval_qualifier_lower,
        !is.na(toxval_qualifier_upper) & toxval_numeric_origin == "Upper Range" ~ toxval_qualifier_upper,
        TRUE ~ toxval_numeric_qualifier
      ) %>% gsub("ca\\.", "~", .),

      # Add toxval_numeric_qualifier for range toxval_subtypes
      toxval_numeric_qualifier = dplyr::case_when(
        !is.na(toxval_numeric_qualifier) ~ toxval_numeric_qualifier,
        toxval_subtype %in% c("Lower Range") ~ ">=",
        toxval_subtype %in% c("Upper Range") ~ "<=",
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
        grepl("NDA[0-9]+\\-", name) ~ gsub("NDA[0-9]+\\-", "", name),
        TRUE ~ name
      ),

      # Set critical_effect to mortality when toxval_type starts with LD
      critical_effect = dplyr::case_when(
        startsWith(toupper(toxval_type), "LD") ~ "mortality",
        TRUE ~ critical_effect
      )
    ) %>%

    # Remove entries that should be dropped due to experimental_flag/data_purpose_category
    dplyr::filter(temp_to_drop == 0) %>%
    dplyr::select(!temp_to_drop) %>%

    # Drop unused helper cols
    dplyr::select(!tidyselect::any_of(c("toxval_qualifier_lower", "toxval_qualifier_upper", "toxval_numeric_origin", "pnd_or_gd"))) %>%
    # Remove entries with "conc. level" toxval_type or "%" toxval_units
    dplyr::filter(!grepl("conc\\. level", toxval_type),
                  !grepl("%", toxval_units))  %>%
    # Filter out entries with "other" species
    dplyr::filter(!grepl("\\bother\\b", species, ignore.case=TRUE)) %>%
    # Filter out entries with NA exposure_route
    tidyr::drop_na(exposure_route)

  # Account for exposure_route/method/form edge case
  if("exposure_method_other" %in% names(res)) {
    res = res %>%
      dplyr::mutate(
        # Set exposure_form when necessary
        exposure_form = dplyr::case_when(
          grepl("inhalation", exposure_route) ~ exposure_method,
          TRUE ~ as.character(NA)
        ),

        # Set correct exposure_method where appropriate
        exposure_method = dplyr::case_when(
          grepl("inhalation", exposure_route) ~ exposure_method_other,
          TRUE ~ exposure_method
        ) %>%
          gsub("not specified|other:", "", .) %>%
          stringr::str_squish()
      ) %>%
      # Drop exposure_method_other column
      dplyr::select(-exposure_method_other)
  }

  # Handle sex column duplicates
  res = res %>%
    dplyr::rowwise() %>%
    # Create unique list of entries, then collapse
    dplyr::mutate(sex = sex %>%
                    gsub("/|\\|", ",", .) %>%
                    strsplit(",") %>%
                    unlist() %>%
                    unique() %>%
                    paste0(collapse="/")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(sex = sex %>%
                    stringr::str_replace("^\\/", "") %>%
                    dplyr::na_if("") %>%
                    dplyr::na_if("NA") %>%
                    # Standardize "both" order
                    ifelse(. == "female/male", "male/female", .))

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

  # Select down columns due to MySQL row size constrictions
  res = res %>%
    dplyr::select(
      -dplyr::any_of(c(
        "substance_uuid_entity_uuid_",
        "datasource_reference_i6:key",
        "resdisc_efflvs_efflevel_@i6:uuid"
      )
      ),
      -dplyr::matches(paste0("resdisc_trgsysorgtox_entry_[0-9]+_@i6:uuid|",
                             "matmet_adexp_dosesconcs_entry_[0-9]+_@|",
                             "matmet_guideline_entry_[0-9]+_@")))

  # Apply str_squish to all character columns
  res = res %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), stringr::str_squish))

  # Replace with NA
  res[res == ""] = NA

  # Remove duplicates and drop NA species
  res = res %>%
    dplyr::distinct() %>%
    tidyr::drop_na(species)

  # Perform deduping
  res = toxval.source.import.dedup(res, hashing_cols=hashing_cols)

  # Add version date. Can be converted to a mutate statement as needed
  res$source_version_date <- src_version_date

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
