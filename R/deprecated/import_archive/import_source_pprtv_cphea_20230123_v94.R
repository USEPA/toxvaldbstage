#--------------------------------------------------------------------------------------
#' @description Import of PPRTV CPHEA source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
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
import_source_pprtv_cphea <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "PPRTV CPHEA"
  source_table = "source_pprtv_cphea"
  dir = paste0(toxval.config()$datapath,"pprtv_cphea/pprtv_cphea_files/")
  tmp = readxl::read_xlsx(paste0(dir, "pprtv_cphea_full.xlsx"), guess_max=21474836)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  #
  # the final file should have column names that include "name" and "casrn"
  # additionally, the names in res need to match names in the source
  # database table. You do not need to add any of the generic columns
  # described in the SOP - they will get added in source_prep_and_load

  # Define helper function for cleaning up scientific notation
  parse_scientific <- function(s) {
    # Handle scientific notation conversion (either 10's or e notation)
    if(grepl("[Xx]", s) && grepl("^[0-9]+\\.?[0-9]* ?[Xx] ?10\\^.*$", s)){
      mantissa <- as.numeric(gsub(" ?[Xx].*", "", s))
      exponent <- as.numeric(gsub(".*\\^", "", s))
      return(mantissa * 10^exponent)
    } else if(grepl("[eE]", s) && grepl("^(-?[0-9]*)\\.?[0-9]+[eE]?[-\\+]?[0-9]+$", s)){
      mantissa <- as.numeric(gsub(" ?[eE].*", "", s))
      exponent <- as.numeric(gsub(".*?[eE]", "", s))
      return(mantissa * 10^exponent)
    }
    return(suppressWarnings(as.numeric(s)))
  }

  # Handle specific toxval_type fixes
  # Empty dataframe to collect handled cases
  res_parsed = data.frame()
  # Add temp ID to help filter out handled cases
  res = tmp %>%
    dplyr::mutate(temp_id = 1:n())

  # Collect all the records that lack a toxval
  res_parsed = res %>%
    dplyr::filter(is.na(`RfD (mg/kg-day)`) & (is.na(PoD) | PoD == ":") & is.na(`RfC (mg/m^3)`)
           & is.na(`Oral Slope Factor`) & is.na(`Unit Risk Factor`)) %>%
    dplyr::select(-c(`RfD (mg/kg-day)`, PoD, `RfC (mg/m^3)`, `Oral Slope Factor`, `Unit Risk Factor`, `RfC (mg/kg-day)`)) %>%
    dplyr::mutate(toxval_type = NA, toxval_numeric = NA, toxval_units = NA) %>%
    rbind(res_parsed, .)
  res = res %>% dplyr::filter(!temp_id %in% res_parsed$temp_id)
  # Resetting before the pivoting so we don't lose fields in the pivot
  res_parsed$temp_id = NA

  # Get list of fields to pivot
  toxval_type_list = c("RfD (mg/kg-day)", "PoD", "RfC (mg/m^3)",
                       "Oral Slope Factor", "Unit Risk Factor", "RfC (mg/kg-day)")
  # Apply general pivot longer fixes for all toxval_type fields
  res = res %>%
    tidyr::pivot_longer(cols = tidyr::all_of(toxval_type_list),
                        names_to="toxval_type",
                        values_to="toxval_numeric",
                        values_transform = list(toxval_numeric=as.character)) %>%
    # Split out units for RfD and RfC variants
    tidyr::separate(toxval_type, c("toxval_type", "toxval_units"), sep="\\(",
                    extra="merge", fill="right", remove=FALSE) %>%
    # Remove extra parentheses from units
    dplyr::mutate(toxval_units = gsub("\\(|\\)", "", toxval_units),
                  temp_id = 1:n()) %>%
    dplyr::mutate(dplyr::across(c("toxval_type", "toxval_units", "toxval_numeric"),
                         ~stringr::str_squish(.)))

  # Per splitting
  res_parsed = res %>%
    dplyr::filter(grepl(" per ", toxval_numeric)) %>%
    tidyr::separate(toxval_numeric, c("toxval_numeric", "toxval_units"), sep=" per ") %>%
    rbind(res_parsed, .)
  res = res %>% dplyr::filter(!temp_id %in% res_parsed$temp_id)
  # PoD splitting
  res_parsed = res %>%
    dplyr::filter(grepl(" : ", toxval_numeric)) %>%
    dplyr::mutate(toxval_numeric = stringr::str_squish(toxval_numeric)) %>%
    tidyr::separate(toxval_numeric, c("toxval_type", "toxval_numeric"), sep=" : ",
                    fill="right") %>%
    tidyr::separate(toxval_numeric, c("toxval_numeric", "toxval_units"), sep=" ") %>%
    rbind(res_parsed, .)
  res = res %>% dplyr::filter(!temp_id %in% res_parsed$temp_id)
########################################################################
  # Straggler PoD without a PoD listed (just ":")
  res_parsed = res %>%
    dplyr::filter(grepl("^:", toxval_numeric)) %>%
    dplyr::mutate(toxval_numeric = gsub(":", "", toxval_numeric) %>%
             stringr::str_squish()) %>%
    tidyr::separate(toxval_numeric, c("toxval_numeric", "toxval_units"), sep=" ",
                    extra="merge", fill="left") %>%
    rbind(res_parsed, .)
  res = res %>% dplyr::filter(!temp_id %in% res_parsed$temp_id)
  # Recombine
  res = res %>%
    rbind(res_parsed) %>%
    dplyr::select(-temp_id) %>%
    # Filter out unnecssarily created toxval_numeric-types during pivoting
    dplyr::filter(!(is.na(toxval_numeric) & !is.na(toxval_type)))

  # Standardize the names
  res0 <- res %>%
    dplyr::rename(name = chemical, endpoint = System, critical_effect = Basis, uncertainty_factor = UF,
                  species = `Species Studied`, study_reference = `Principal Study`,
                  tumor_site = `Tumor site(s)`, cancer_type = Cancer) %>%
    # Clean up punctuation/spacing for endpoint column
    dplyr::mutate(endpoint = gsub(" ,", ", ", endpoint)) %>%
    dplyr::rowwise() %>%
    # Determine study_type
    dplyr::mutate(study_type = ifelse(grepl("Cancer|Carcinogenic", table_title),
                                      "Cancer", ifelse(grepl("Subchronic", table_title),
                                                       "Subchronic", "Chronic")))
  # Hardcode species for several records
  res0$species[res0$study_reference == "Biodynamics 1988"
               & res0$name == "Butyltin Compounds, mono-"] <- "Rat"
  res0$species[res0$study_reference == "Kawakami et al., 2015"
               & res0$name == "2-Nitropropane"] <- "Rat"
  res0$species[res0$study_reference == "Lewis et al., (1979), Ulrich et al. (1977)"
               & res0$name == "2-Nitropropane"] <- "Rat"
  res0$species[res0$study_reference == "Lewis et al. (1979) and Ulrich et al. (1977)"
               & res0$name == "2-Nitropropane"] <- "Rat"

  # Fix unicode symbols in character fields
  res0 <- dplyr::mutate(res0, dplyr::across(where(is.character), fix.greek.symbols))

  # Add missing toxval_units (imputed from RfC/RfD) to NOAEL cases
  # Narrow down the search first
  noael_fix = res0 %>%
    dplyr::filter(toxval_type == "NOAEL", is.na(toxval_units)) %>%
    dplyr::select(name, study_type, table_title) %>%
    dplyr::distinct()

  # Fix each individual case
  for(i in seq_len(nrow(noael_fix))){
    # Pull case of chemical from same table and same type
    n_fix = res0 %>%
      dplyr::filter(name == noael_fix$name[i], study_type == noael_fix$study_type[i],
             table_title == noael_fix$table_title[i],
             !is.na(toxval_units)) %>%
      # Select available units
      dplyr::select(toxval_units)

    # Skip if multiple units
    if(nrow(n_fix) > 1){
      message("Issue with NOAEL chemical: ", noael_fix$name[i])
      next
    } else {
      # Make replacement of NA
      res0$toxval_units[is.na(res0$toxval_units) &
                          res0$name == noael_fix$name[i] &
                          res0$study_type == noael_fix$study_type[i] &
                          res0$table_title == noael_fix$table_title[i]] = n_fix$toxval_units
    }
  }

  # Update units for toxval_types Oral Slope/Unit Risk Factor
  cases <- which(res0$toxval_type %in% c("Oral Slope Factor", "Unit Risk Factor"))
  res0$toxval_units[cases] <- paste0("(", res0$toxval_units[cases], ")^-1")

  # Fix names
  names(res0) <- names(res0) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  # Split duration column into study_duration_value and _units
  res0 <- res0 %>%
    dplyr::mutate(# Remove leading "N hr/d, N d/wk, for/on "
      duration_original = duration,
      duration = gsub("^([0-9]+\\s?hr?/d,?\\s)?([0-9]+\\s?d(ay)?/wk?)?,?\\s?(for\\s|on\\s)?",
                      "", duration),
      # Remove "postweaning" and "gavage study" from duration
      duration = gsub(", postweaning|gavage study", "", duration),
      duration = gsub("weejs", "weeks", duration)) %>%
    # Clean up and split remaining numeric/unit pairs
    fix_numeric_units_split(to_split = "duration",value_to="study_duration_value",
                            units_to="study_duration_units") %>%
    # Now that they've served as tags for adding units, remove these
    dplyr::mutate(dplyr::across(c(study_duration_value, study_duration_units),
                  ~ stringr::str_squish(
                    stringr::str_replace_all(., "(reproductive: )?gds?|gestation days|pnds?", "")
                    )
                  )
           ) %>%
    dplyr::select(-duration)

  # Combine two separate notes columns from the extraction into one
  # We don't need to worry about having values in both fields for one record, as they're exclusionary by construction
  res0$notes <- paste0(tidyr::replace_na(res0$note, ""), tidyr::replace_na(res0$note_in_body, ""))
  res0 <- res0 %>% dplyr::select(-c(note, note_in_body))

  # Handle cases like https://cfpub.epa.gov/ncea/pprtv/chemicalLanding.cfm?pprtv_sub_id=1815
  # toxval_numeric remained as the units, toxval_numeric is in the notes section
  # Not a piped, fully tidy solution, but cleaner than using a conditional mutate.
  # NB that this only grabs the first number/unit pair from the notes.
  # These all seem to be correct for this version of CPHEA, but check again if a re-extraction is performed.
  missing_numeric <- which(is.na(res0$toxval_numeric) & !is.na(res0$toxval_type))
  toxval_with_units <- stringr::str_extract(res0$notes[missing_numeric], "[0-9]+\\.?[0-9]* [^ ]* ")
  res0$toxval_numeric[missing_numeric] <- gsub(" .*$", "", toxval_with_units)
  res0$toxval_units[missing_numeric] <- gsub("^[^ ]* ", "", toxval_with_units)

  # Fix scientific notation issue
  res0 = res0 %>%
    # Add rowwise so mutate can vectorize the parse_scientific function
    dplyr::rowwise() %>%
    dplyr::mutate(toxval_numeric = parse_scientific(toxval_numeric)) %>%
    ## Add blank manual curation fields
    dplyr::mutate(strain = "-",
           sex = "-",
           exposure_route = "-",
           exposure_method = "-")
  # Replacing empty notes with "-"
  res0$notes[res0$notes == ""] <- "-"

  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db=db,
                       source=source,
                       table=source_table,
                       res=res0,
                       do.reset=do.reset,
                       do.insert=do.insert,
                       chem.check.halt=chem.check.halt)

}
