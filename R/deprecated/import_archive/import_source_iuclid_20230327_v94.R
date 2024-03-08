#--------------------------------------------------------------------------------------
#' @description A generic template for adding data to toxval_source for a new source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param subf The subfolder containing the IUCLID subsource
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
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
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{select}}
#'  \code{\link[writexl]{write_xlsx}}
#'  \code{\link[tidyr]{reexports}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{pivot_wider}}, \code{\link[tidyr]{separate_rows}}, \code{\link[tidyr]{unite}}
#'  \code{\link[stringr]{str_trim}}
#'  \code{\link[textclean]{mgsub}}
#' @rdname import_source_iuclid
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr filter rename mutate across select
#' @importFrom writexl write_xlsx
#' @importFrom tidyr all_of separate pivot_longer starts_with pivot_wider separate_rows unite matches ends_with
#' @importFrom stringr str_squish
#' @importFrom textclean mgsub
#--------------------------------------------------------------------------------------
import_source_iuclid <- function(db, subf, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = paste0("IUCLID_", subf)
  source_table = paste0("source_iuclid_", subf) %>% tolower()
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

  # Check for field mapping issues
  map_check = map$from[!map$from %in% names(res0)]
  if(length(map_check)){
    map_orig$map_confirmed[(map_orig$oht == subf) & (map_orig$from %in% map_check)] <- 0
    # Update the map to help with curation of field names
    writexl::write_xlsx(map_orig, paste0(toxval.config()$datapath,"iuclid/field_maps/iuclid_field_map.xlsx"))
    return(cat(paste0("Need to remap the following fields: ", toString(map_check)), "\n"))
  }

  # Create a named vector to handle renaming from the map
  tmp = map$from %T>% {
    names(.) <- map$to
  }

  res <- res0 %>%
  # Copy columns and rename new columns
    dplyr::rename(tidyr::all_of(tmp)) %>%
    # Split columns and name them
    tidyr::separate(., study_type, c("study_type","exposure_route"), sep=": ", fill="right", remove=FALSE)

  # Handle developmental fetus vs. maternal studies
  if(grepl("developmental", subf) && any(grepl("fetus_|maternal_", names(res)))){
    message("Handling developmental OHT fetus vs. maternal field pivots...")
    # Fill default maternal sex
    res$maternal_sex = "female"
    res = res %>%
      # Get all maternal and fetus fields in one field
      tidyr::pivot_longer(cols=tidyr::starts_with("fetus_") | tidyr::starts_with("maternal_"),
                          names_to = "dev_field",
                          values_transform = list(value=as.character)) %>%
      # Split by maternal vs. fetus fields with "generation_type"
      tidyr::separate(dev_field, into=c("generation_type", "field"), sep="_", extra="merge") %>%
      # Spread out fields again, now without theif "fetus_" or "maternal_" prefixes (now stored in "generation_type")
      tidyr::pivot_wider(names_from = field, values_from=value)
  }

  # Removed since we only want to use the reference_* chemical information fields
  # The other chemical_* fields come from ECHA
  # ## Chemical cleaning
  # # Handle chemical name reassignment ("-" or NA values)
  # res$name[res$name == "-" | is.na(res$name)] <- res$chemical_name[res$name == "-" | is.na(res$name)]
  # # Replace 'not available', 'no iupac name', etc.
  # res$name[grepl("available|no iupac name|Not allocated|see remarks|confidential", res$name, ignore.case = TRUE)] <-
  #   res$chemical_name[grepl("available|no iupac name|Not allocated|see remarks|confidential", res$name, ignore.case = TRUE)]
  # # Handle casrn reassignment ("-" or NA values)
  # res$casrn[res$casrn == "-" | is.na(res$casrn)] <- res$chemical_CASnumber[res$casrn == "-" | is.na(res$casrn)]
  # # Replace "to be assigned", "Not assigned", "not yet assigned"
  # res$casrn[grepl("assigned", res$casrn, ignore.case = TRUE)] <- res$chemical_CASnumber[grepl("assigned", res$casrn, ignore.case = TRUE)]

  # Split chemical mixtures/lists
  res = res %>%
    dplyr::mutate(casrn = gsub(" and", ",", casrn)) %>%
    tidyr::separate_rows(name, sep=";") %>%
    tidyr::separate_rows(casrn, sep=";") %>%
    tidyr::separate_rows(casrn, sep=",") %>%
    # Squish extra whitespace
    dplyr::mutate(dplyr::across(c("name", "casrn"), ~stringr::str_squish(.)))

  # Fill "-" name and casrn with NA
  res$name[res$name == "-" | res$name == ""] = NA
  res$casrn[res$casrn == "-" | res$casrn == ""] = NA
  # Filter out incomplete cases, keep partial cases (has something for name or casrn)
  res = res %>%
    dplyr::filter(!(is.na(name) & is.na(casrn)))
  # View(res %>% filter(is.na(name)) %>% select(name, casrn) %>% distinct())
  # View(res %>% filter(is.na(casrn)) %>% select(name, casrn) %>% distinct())

  # Handle case where exposure was mapped to exposure_form and exposure_method in the map
  if("exposure" %in% names(res)){
    res = res %>%
      tidyr::separate(., exposure, c(NA,"exposure_method"), sep=": ", fill="right", remove=FALSE)
  }
  # Continue transformations
  res = res %>%
    # Combine columns and name them
    tidyr::unite(toxval_numeric, toxval_numeric_lower, toxval_numeric_upper, na.rm = TRUE, sep='-') %>%
    tidyr::unite(toxval_qualifier, toxval_qualifier_lower, toxval_qualifier_upper, na.rm = TRUE, sep=' ') %>%
    #select(-matches("CrossReference.*.uuid")) %>%
    dplyr::select(-tidyr::matches("CrossReference.*.uuid|CrossReference.*.RelatedInformation"))

  # Replace column value with another column value based on a condition ("other:")
  if(all(c("toxval_units", "toxval_units_other") %in% names(res))){
    res$toxval_units[res$toxval_units == 'other:' & !is.na(res$toxval_units)] <- res$toxval_units_other[res$toxval_units == 'other:' & !is.na(res$toxval_units)]
  }
  if(all(c("toxval_type", "toxval_type_other") %in% names(res))){
    res$toxval_type[res$toxval_type == 'other:' & !is.na(res$toxval_type)] <- res$toxval_type_other[res$toxval_type == 'other:' & !is.na(res$toxval_type)]
  }
  if(all(c("species", "species_other") %in% names(res))){
    res$species[res$species == 'other:' & !is.na(res$species)] <- res$species_other[res$species == 'other:' & !is.na(res$species)]
  }
  if(all(c("strain", "strain_other") %in% names(res))){
    res$strain[res$strain == 'other:' & !is.na(res$strain)] <- res$strain_other[res$strain == 'other:' & !is.na(res$strain)]
  }
  if(all(c("guideline", "guideline_other") %in% names(res))){
    res$guideline[res$guideline == 'other:' & !is.na(res$guideline)] <- res$guideline_other[res$guideline == 'other:' & !is.na(res$guideline)]
  }
  if(all(c("exposure", "exposure_other") %in% names(res))){
    res$exposure[res$exposure == 'other:' & !is.na(res$exposure)] <- res$exposure_other[res$exposure == 'other:' & !is.na(res$exposure)]
  }

  # Fix: effect_level_basis TBD
  # Fix: media TBD
  # Fix: reference_type TBD
  # Fix: dose_units TBD

  # Check for acute OHTs without a mapped duration field
  if(grepl("acute", subf, ignore.case = TRUE)){
    # Set duration to 1 day if not present in value/units
    if(!all(c("study_duration_value", "study_duration_units") %in% names(res))){
      res$study_duration_value = 1
      res$study_duration_units = "day"
    }
  }

  # Perform study duration split if needed
  if("study_duration_original" %in% names(res)){
    # Fix study duration with various regex
    res = fix_numeric_units_split(df = res,
                                  to_split = "study_duration_original",
                                  value_to = "study_duration_value",
                                  units_to = "study_duration_units")
  }

  # Check for media column, or put as blank
  if(!"media" %in% names(res)){
    message("Media field is missing a mapping, defaulting to blank for now...")
    # Pause for user to see warning message
    Sys.sleep(5)
    res$media = "-"
  }

  # Fix unicode symbols in units
  res <- res %>%
    dplyr::mutate(dplyr::across(tidyr::ends_with("_units"), ~fix.replace.unicode(.)))


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
                                 "fetus", "remarks"
                     ),
                     replace = c("_", "admindata", "matnmet", "adminexposure", "admin",
                                 "mat", "resndisc", "efflvs", "sys", "tox", "inhale",
                                 "devmtl", "mtnl", "fts", "ftl", "res", "abnorm", "anim",
                                 "fts", "remrk")) %>%
    gsub("targetsysorgantox_targetsysorgantox", "targetsysorgantox", .)

  # Halt if field names are still too long
  if(any(nchar(names(res)) >= 65)){
    message("Error: fieldnames too long: ", names(res)[nchar(names(res)) >= 65] %>% toString())
    browser()
  }

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
