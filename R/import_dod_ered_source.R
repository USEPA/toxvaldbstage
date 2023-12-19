#--------------------------------------------------------------------------------------
#' @title import_dod_ered_source
#' @description Load DOD ERED data into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @return None; the script sends data to toxval_source
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{distinct}}
#'  \code{\link[stringr]{str_extract}}, \code{\link[stringr]{str_trim}}
#'  \code{\link[tidyr]{replace_na}}, \code{\link[tidyr]{drop_na}}
#' @rdname import_dod_ered_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate across case_when rename distinct
#' @importFrom stringr str_extract str_squish
#' @importFrom tidyr replace_na drop_na
#--------------------------------------------------------------------------------------
import_dod_ered_source <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "DOD ERED"
  source_table = "source_dod_ered"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2019-10-25")
  dir = paste0(toxval.config()$datapath,"dod_ered/dod_ered_files/")
  file = paste0(dir,"USACE_ERDC_ERED_database_10_25_2019.xlsx")
  res0 = readxl::read_xlsx(file, sheet="ERED Data", skip=3)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  res0 = res0 %>%
    # Handle unicode symbols
    dplyr::mutate(dplyr::across(where(is.character), fix.replace.unicode),
                  # Remove any instances of N/A, N/I, N/R, and N/S
                  dplyr::across(.fns = ~replace(., . %in% c("N/A", "N/I", "N/R", "N/S", "(N/I)"),
                                                NA))
                  )

  res = res0 %>%
    # Mutate new columns as needed
    dplyr::mutate(
      lifestage = LifeStage,
      source_url = "https://ered.el.erdc.dren.mil/",
      subsource = "USACE_ERDC_ERED_database_10_25_2019",
      name = ChemName,
      casrn = CAS %>%
        gsub("N/A", "", .),
      toxval_type = Risk %>%
        gsub("N/A|N/R|<\\s?", "", .),
      species = tolower(GenusSpecies),
      exposure_route = Route %>%
        gsub("N/I", "", .),
      long_ref = paste(source, Origin) %>%
        # Remove NA Origin
        gsub(" NA$", "", .),
      year = as.numeric(Time),
      habitat = environment,
      # Extract and translate study_type
      study_type = dplyr::case_when(
        ST == "L" ~ "Lab",
        ST == "F" ~ "Field",
        ST == "Meso" ~ "Mesocosm",
        ST == "Micro" ~ "Microcosm",
        TRUE ~ ST
      ),
      # Handle toxval_numeric noise
      toxval_numeric = expConc %>%
        stringr::str_extract("[0-9\\.]+") %>%
        gsub("N/A|N/I|N/R", "", .) %>%
        as.numeric(),

      # Extract and clean toxval_units
      toxval_units = ExpConcUnits %>%
        gsub("N/A|N/I|N/R|N/S", "", .),

      # Extract toxval_numeric_qualifier
      toxval_numeric_qualifier = expConc %>%
        gsub("[\\s0-9\\.]+", "", .) %>%
        dplyr::na_if("") %>%
        dplyr::na_if("-") %>%
        tidyr::replace_na("="),

      # Get media
      Media = dplyr::case_when(
          Media == "FW" ~ "freshwater",
          Media == "SW" ~ "saltwater",
          TRUE ~ Media
        ),

      # Get animal_source
      animal_source = dplyr::case_when(
        Coll == "W" ~ "wild",
        Coll == "C" ~ "cultured",
        TRUE ~ Coll
      ),

      # Get critical_effect and remove N/R
      critical_effect = paste0(Effect, ", ", fraction) %>%
        gsub(", N/R|N/R, ", "", .),

      # Address typo in source data
      DUR = DUR %>%
        gsub("96114\\)", "96\\(114\\)", .) %>%
        # Fix consistency of unit spacing
        gsub(" d", "d", .) %>%
        # Removing "+1 hr"
        gsub("+1 hr", "", ., fixed = TRUE) %>%
        stringr::str_squish(),

      # Get study_duration_value and study_duration_units
      # To handle edge cases, only take the leftmost value and hr/d/mo/yr
      # Rationale: earliest time when effects are observed is most relevant
      study_duration_value = DUR %>%
        sub('.*;', '', .) %>%
        sub(" - ", "-", .) %>%
        # Pull out ranges or first numeric value
        stringr::str_extract("[0-9\\.]+-[0-9\\.]+|[0-9\\.]+") %>%
        # Select max of range
        sub('.*-', '', .) %>%
        as.numeric(),
      study_duration_units = stringr::str_extract(DUR, "d p\\.h\\.|d posthatch|hr|rr|d|wk|mo|m\\b|yr|generations") %>%
        gsub("hr|rr", "hours", .) %>%
        gsub("d", "days", .) %>%
        gsub("wk", "weeks", .) %>%
        gsub("mo", "months", .) %>%
        gsub("m\\b", "minutes", .) %>%
        gsub("yr", "years", .) %>%
        gsub("posthatch|p\\.h\\.", "post-hatch", .),

      # Assign missing units
      # Duration units for long term studies (NOEC,LOEC) as d (days) - "NOEC|LOEC"
      # Duration units for short term studies (ED,LC) as d (days) if duration value > 4 and as h (hours) if <= 4 - "ED|LC"
      # Duration units for the rest of the short term studies as h (hours) - "NOEC|LOEC|ED|LC"
      study_duration_units = ifelse(!is.na(study_duration_units), study_duration_units,
                                    ifelse(!grepl("NOEC|LOEC|ED|LC", toxval_type), study_duration_units,
                                           ifelse(grepl("NOEC|LOEC", toxval_type), "days",
                                                  ifelse(study_duration_value > 4, "days", "hours")))),

      # Size column has sex information for some entries
      sex = dplyr::case_when(
        `size (cm)` == "female" ~ "F",
        `size (cm)` == "male" ~ "M",
        TRUE ~ NA_character_
      )
    ) %>%

    # Drop rows without toxval_numeric
    tidyr::drop_na(toxval_numeric) %>%

    # Rename columns as specified in previous import script
    dplyr::rename(ered_id="EREDid", ref_id="REFid", common_name="CommonName",
                  name_categories="CommonDesc", life_stage="LifeStage", chemical_group="chem_group",
                  mixed_chemical="Mix", spiked_chemical="Spiked", exposure_conc="expConc",
                  exposure_units="ExpConcUnits", study_duration="DUR", tissue_residue_conc="TissueConc",
                  tissue_residue_conc_units="tissue wet \r\nweight units", test_tissue_type="fraction",
                  effect_trend="trend", percentage_effect="%effect", effect_significance="Signif",
                  data_source="source", data_year="Time", ered_date_modified="Date Modified",
                  moisture_percentage="%Moisture") %>%

    # Remove columns causing duplicates with hashed cols
    dplyr::select(-ered_id, -tissue_residue_conc, -tissue_residue_conc_units,
                  -percentage_effect, -Control_result, -comments,
                  -effect_trend, -effect_significance, -Depuration, -ered_date_modified,
                  -exposure_conc, -Origin, -animal_source, -Coll, -p_value, -mixed_chemical,
                  -Lipid, -moisture_percentage, -`weight (g)`, -`size (cm)`, -study_duration,
                  -`Exp Type`) %>%

    # Uncomment this block and comment the above block if mutation is preferred to renaming
    # dplyr::mutate(
    #   ered_id = EREDid,
    #   ref_id = REFid,
    #   common_name = CommonName,
    #   name_categories = CommonDesc,
    #   life_stage = LifeStage,
    #   chemical_group = chem_group,
    #   mixed_chemical = Mix,
    #   spiked_chemical = Spiked,
    #   exposure_conc = expConc,
    #   exposure_units = ExpConcUnits,
    #   study_duration = DUR,
    #   tissue_residue_conc = TissueConc,
    #   tissue_residue_conc_units = `tissue wet \r\nweight units`,
    #   test_tissue_type = fraction,
    #   effect_trend = trend,
    #   percentage_effect = `%effect`,
    #   effect_significance = Signif,
    #   data_source = source,
    #   data_year = Time,
    #   ered_date_modified = `Date Modified`,
    #   moisture_percentage = `%Moisture`
    # ) %>%

    dplyr::distinct()

  # Check study_duration splitting
  # View(res %>% select(study_duration, study_duration_value, study_duration_units) %>% unique())

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"

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




