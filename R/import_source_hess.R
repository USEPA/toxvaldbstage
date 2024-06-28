#--------------------------------------------------------------------------------------
#' @title import_source_hess
#' @description Load HESS data into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
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
#'  \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{unite}}, \code{\link[tidyr]{drop_na}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{na_if}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{tidyeval-compat}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{mutate-joins}}
#'  \code{\link[stringr]{str_replace}}, \code{\link[stringr]{str_trim}}, \code{\link[stringr]{case}}, \code{\link[stringr]{str_extract}}
#' @rdname import_source_hess
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom tidyr pivot_longer unite drop_na
#' @importFrom dplyr mutate across where na_if case_when select rename left_join
#' @importFrom stringr str_replace_all str_squish str_to_title str_extract str_replace
#--------------------------------------------------------------------------------------
import_source_hess <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "HESS"
  source_table = "source_hess"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2021-06-16")
  dir = paste0(toxval.config()$datapath,"hess/hess_files/")
  file = paste0(dir,"hess_6_16_21.xlsx")
  res0 = readxl::read_xlsx(file) %>%
    # Remove unnamed columns
    dplyr::select(-tidyr::starts_with("..."))
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  res = res0 %>%
    # Extract toxval_numeric/units/type with pivot
    tidyr::pivot_longer(
      cols=c("reported_noel", "reported_noael", "reported_loel", "reported_loael"),
      names_to="toxval_type",
      values_to="toxval_numeric_units"
    ) %>%
    dplyr::rename(src_document_name = document_name) %>%
    dplyr::mutate(
      name = chemical_name,
      source_url = "https://www.nite.go.jp/en/chem/qsar/hess_update-e.html",
      subsource_url = source_url,

      # Set document name extension to PDF
      src_document_name = gsub("docx", "pdf", src_document_name),

      # Initial cleaning of character columns
      dplyr::across(dplyr::where(is.character),
                    # Remove start/end quotation
                    ~ trimws(., whitespace = "'") %>%
                      stringr::str_squish() %>%
                      fix.replace.unicode() %>%
                      stringr::str_replace_all("male s\\b", "males") %>%
                      stringr::str_replace_all("\\*", "") %>%
                      dplyr::na_if("N/A") %>%
                      dplyr::na_if("N/I") %>%
                      dplyr::na_if("None") %>%
                      dplyr::na_if("No description") %>%
                      dplyr::na_if("Non used") %>%
                      dplyr::na_if("No vehicle") %>%
                      dplyr::na_if("-") %>%
                      dplyr::na_if("")),

      # Clean toxval_type
      toxval_type = toxval_type %>%
        gsub("reported_", "", .) %>%
        toupper() %>%
        stringr::str_squish()
    )

  # Names of all critical_effect categories
  crit_effect_fields <- c("deaths","clinical_observation","functional_observational_battery","body_weight_changes","food_consumption",
                          "water_consumption","urinalysis","hematology","blood_chemistry","thyroid_hormone","absolute_organ_weight",
                          "relative_organ_weight","organ_weight","necropsy","macroscopic_finding","histopathology","reproductive_tissue_evaluation",
                          "estrous_cycle_characterization","bone_marrow_cellularity_counts","liver_biochemistry","reproductive_endpoint","other_findings")

  # Add field names to critical_effect field values
  for(field in crit_effect_fields) {
    field_cleaned = stringr::str_to_title(gsub("_", " ", field))

    res = res %>% dplyr::mutate(
      !!field := dplyr::case_when(
        is.na(!!sym(field)) ~ as.character(NA),
        TRUE ~ paste0(!!field_cleaned, ": ", !!sym(field))
      ) %>%
        stringr::str_squish()
    )
  }

  res = res %>%
    # Combine critical_effect fields
    tidyr::unite("critical_effect", !!crit_effect_fields, sep="|", na.rm=TRUE, remove=FALSE) %>%

    dplyr::mutate(
      long_ref = associated_publication,
      # Extract sex information from subject_type and critical_effect fields
      sex = dplyr::case_when(
        grepl("male", subject_type) ~ stringr::str_extract(subject_type, "(?:fe)?male") %>% c(),
        grepl("\\bmale", critical_effect) & grepl("female", critical_effect) ~ "male/female",
        grepl("female", critical_effect) ~ "female",
        grepl("\\bmale", critical_effect) ~ "male",
        TRUE ~ as.character(NA)
      ),

      # Extract strain from subject_type field (species is "rat" for all entries)
      species = "rat",
      strain = subject_type %>%
        gsub("(IGS|\\)),.+", "\\1", .) %>%
        gsub("Rat|Ratr|Ra t|rats", "", .) %>%
        gsub("(?:fe)?male only|\\(\\s*only male\\s*\\)", "", .) %>%
        stringr::str_squish() %>%
        stringr::str_replace("^\\((.+)\\)$", "\\1") %>%
        stringr::str_squish() %>%
        dplyr::na_if("-") %>%
        dplyr::na_if("Unknon") %>%
        dplyr::na_if("Unknown") %>%
        dplyr::na_if("()") %>%
        dplyr::na_if("not stated") %>%
        dplyr::na_if(""),

      # Extract exposure_method from route (all exposure_route values are "Oral")
      exposure_route = "Oral",
      exposure_method = route %>%
        gsub("Oral|containing|\\*|[0-9]+days\\/week", "", .) %>%
        gsub("Gavege|Gvage", "Gavage", .) %>%
        gsub("\\(\\)", "", .) %>%
        stringr::str_squish() %>%
        stringr::str_replace("^\\((.+)\\)$", "\\1") %>%
        stringr::str_squish() %>%
        stringr::str_replace("^d", "D") %>%
        stringr::str_replace("^g", "G") %>%
        stringr::str_replace("^w", "W") %>%
        stringr::str_replace("^f", "F") %>%
        stringr::str_replace("Feeding", "Feed") %>%
        stringr::str_replace("Drinking$", "Drinking water"),

      # Extract exposure_form from vehicle field
      exposure_form = vehicle %>%
        gsub("oill", "oil", .) %>%
        stringr::str_squish() %>%
        stringr::str_replace("^\\((.+)\\)$", "\\1") %>%
        stringr::str_squish(),

      # Extract toxval_subtype from study_type
      toxval_subtype = dplyr::case_when(
        grepl("^TG.*\\:", study_type) ~ gsub("(.*)(\\:.*)", "\\1", study_type),
        grepl("\\:|\\;", study_type) ~ gsub("(.*)(\\:|\\;\\s+)(.*)(\\)$)", "\\3", study_type),
        grepl("TG.*\\)+", study_type) ~ gsub("(.*\\s*)(TG.*)(\\))", "\\2", study_type),
        grepl("TG.*\\)+\\s*\\(+", study_type) ~ gsub("(TG.*)(\\).*\\(.*)", "\\1", study_type),
        grepl("^Repeated.*TG.*", study_type) ~ gsub("(.*)(TG.*$)", "\\2", study_type),
        grepl("OECD.*combined", study_type, ignore.case = T) ~ gsub("(.*OECD\\s*\\d+)(\\s*.*)", "\\1", study_type),
        TRUE ~ as.character(NA)
      ) %>%
        gsub("\\).+", "", .) %>%
        gsub("\\/ ", "/", .) %>%
        gsub("\\-", " ", .) %>%
        gsub("(TG)([0-9])", "\\1 \\2", .) %>%
        stringr::str_squish(),

      # Extract study_duration_value/units from study_type
      study_duration = study_type %>%
        gsub("([0-9])([A-Za-z])", "\\1 \\2", .) %>%
        gsub(" days, female ", "-", .) %>%
        gsub("\\( female \\)", "", .) %>%
        gsub("\\( male \\)", "", .) %>%
        gsub("\\-(day|week)", " \\1", .) %>%
        gsub(" \\- ", "-", .) %>%
        gsub("eee", "ee", .) %>%
        stringr::str_squish(),
      study_duration_value = study_duration %>%
        stringr::str_extract("([0-9\\-]+) (day|week|W\\b|M\\b)", group=1),
      study_duration_units = study_duration %>%
        stringr::str_extract("([0-9\\-]+) (day|week|W\\b|M\\b)", group=2) %>%
        stringr::str_replace("\\bW\\b", "week") %>%
        stringr::str_replace("\\bM\\b", "month"),

      # Clean critical_effect
      critical_effect = critical_effect %>%
        # Transformations from previous load script
        stringr::str_replace_all("\\?", "") %>%
        gsub("(^Deaths)(: [0-9]+)([^|]+)(.*)", "\\1\\4", .) %>%
        stringr::str_replace_all("(;|:)+\\s*[^[:alnum:]]*[0-9]+\\s*(male|female)*\\s*(female|male)*", "") %>%
        stringr::str_replace_all(",+\\s*[^[:alnum:]]*[0-9]+\\s*(male|female)*", "") %>%
        gsub("(\\s*[a-zA-Z]{1}[a-z]+)([A-Z]{1}[a-z]*\\s*)", "\\1 \\2", .) %>%
        gsub("(decrease|increase)(\\s+)", "\\1,\\2", .) %>%
        gsub("(,\\s+\\|)", "|", .) %>%
        gsub(",$", "", .) %>%
        gsub("(\\.|\\s+)[0-9]+\\s+(male|female)+", "", .) %>%
        gsub("(male|female)+\\s*(male|female)*\\s*", "", .) %>%
        gsub("(male|female)+\\s*", "", .) %>%
        stringr::str_squish(),

      # Fix casrn values
      casrn = dplyr::case_when(
        grepl("82832\\-73\\-3, 87625\\-09\\-0", casrn) ~ "82832-73-3",
        grepl("82657\\-04\\-399267\\-18\\-2", casrn) ~ "99267-18-2",
        TRUE ~ casrn
      ),

      # Clean study_type
      study_type = dplyr::case_when(
        grepl("Repeat(?:ed)?\\s*\\-*dose", study_type, ignore.case=TRUE) ~ "subchronic",
        grepl("Carcinogenicity", study_type, ignore.case=TRUE) ~ "carcinogenicity",
        TRUE ~ as.character(NA)
      ),

      # Extract toxval_numeric_units_units based on sex
      toxval_numeric_units = toxval_numeric_units %>%
        gsub("ca.", "~", .),
      toxval_numeric_units_both = dplyr::case_when(
        grepl("male female\\s*:", toxval_numeric_units) ~ stringr::str_extract(toxval_numeric_units,
                                                                               "male female\\s*:\\s*[<>~=]*\\s*[0-9\\.]+\\s*(?:ppm|mg\\/kg(?:\\/day)?|%|L)(?:\\s*(?:diet|day))?"),
        TRUE ~ stringr::str_extract(toxval_numeric_units,
                                    "^[<>~=]*\\s*[0-9\\.]+\\s*(?:ppm|mg\\/kg(?:\\/day)?|%|L)(?:\\s*(?:diet|day))?")
      ) %>% c(),
      toxval_numeric_units_male = toxval_numeric_units %>%
        stringr::str_extract("\\bmale\\s*:\\s*[<>~=]*\\s*[0-9\\.]+\\s*(?:ppm|mg\\/kg(?:\\/day)?|%|L)(?:\\s*(?:diet|day))?") %>%
        c(),
      toxval_numeric_units_female = toxval_numeric_units %>%
        gsub("male female.+", "", .) %>%
        stringr::str_extract("female\\s*:\\s*[<>~=]*\\s*[0-9\\.]+\\s*(?:ppm|mg\\/kg(?:\\/day)?|%|L)(?:\\s*(?:diet|day))?") %>%
        c()
    ) %>%
    dplyr::select(-toxval_numeric_units)

  # Separate out observations by sex
  base_res = res %>%
    dplyr::select(-toxval_numeric_units_male, -toxval_numeric_units_female) %>%
    dplyr::rename(toxval_numeric_units = toxval_numeric_units_both)
  male_res = res %>%
    dplyr::select(-toxval_numeric_units_both, -toxval_numeric_units_female) %>%
    dplyr::rename(toxval_numeric_units = toxval_numeric_units_male) %>%
    dplyr::mutate(sex = "male")
  female_res = res %>%
    dplyr::select(-toxval_numeric_units_male, -toxval_numeric_units_both) %>%
    dplyr::rename(toxval_numeric_units = toxval_numeric_units_female) %>%
    dplyr::mutate(sex = "female")

  # Recombine data and conduct further extraction operations
  res = rbind(base_res, male_res, female_res) %>%
    dplyr::mutate(
      # Extract final toxval_numeric/units/qualifier
      toxval_numeric = toxval_numeric_units %>%
        stringr::str_extract("[0-9\\.]+") %>% c() %>%
        dplyr::na_if(""),
      toxval_units = toxval_numeric_units %>%
        stringr::str_extract("(?:ppm|mg\\/kg(?:\\/day)?|%|L)(?:\\s*(?:diet|day))?") %>% c() %>%
        stringr::str_squish() %>%
        dplyr::na_if(""),
      toxval_numeric_qualifier = toxval_numeric_units %>%
        stringr::str_extract("[<>~=]") %>% c() %>%
        dplyr::na_if(""),
    ) %>%
    dplyr::select(-study_duration, -toxval_numeric_units) %>%
    tidyr::drop_na(toxval_numeric, toxval_units, toxval_type)

  # Remove casrn from name
  res = res %>%
    dplyr::rowwise() %>%
    dplyr::mutate(name = name %>%
                    gsub(paste0(casrn, ", "), "", .) %>%
                    stringr::str_squish()) %>%
    dplyr::ungroup()

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"

  # Perform deduping
  res = toxval.source.import.dedup(res)

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
