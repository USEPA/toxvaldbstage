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
#' @rdname import_generic_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate across where case_when rename distinct
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

  res = res0 %>%
    # Handle unicode symbols
    dplyr::mutate(dplyr::across(dplyr::where(is.character), fix.replace.unicode)) %>%

    # Mutate new columns as needed
    dplyr::mutate(
      name = ChemName,
      casrn = CAS %>%
        gsub("N/A", "", .),
      toxval_type = Risk %>%
        gsub("N/A|N/R|<\\s?", "", .),
      species = tolower(GenusSpecies),
      exposure_route = Route %>%
        gsub("N/I", "", .),
      long_ref = source,
      year = as.numeric(Time),
      habitat = environment,

      # Extract and translate study_type
      study_type = dplyr::case_when(
        ST == "L" ~ "Lab",
        ST == "F" ~ "Field",
        ST == "Meso" ~ "Mesocosm",
        ST == "Micro" ~ "Microcosm",
        .default = ST
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
        dplyr::case_when(
          . == "" | . == "-" ~ NA,
          .default = .
        ) %>%
        tidyr::replace_na("="),

      # Get media
      media = dplyr::case_when(
          grepl("FW", Media) ~ "freshwater",
          grepl("SW", Media) ~ "saltwater",
          .default = ""
        ),

      # Get animal_source
      animal_source = dplyr::case_when(
        Coll == "W" ~ "wild",
        Coll == "C" ~ "cultured",
        .default = Coll
      ),

      # Get critical_effect and remove N/R
      critical_effect = paste0(Effect, ", ", fraction) %>%
        gsub(", N/R|N/R, ", "", .),

      # Address typo in source data
      DUR = gsub("96114\\)", "96\\(114\\)", DUR),

      # Get study_duration_value and study_duration_units
      # To handle edge cases, only take the leftmost value and hr/d/mo/yr
      # Rationale: earliest time when effects are observed is most relevant
      study_duration_value = stringr::str_extract(DUR, "[0-9\\.]+"),
      study_duration_units = stringr::str_extract(DUR, "hr|rr|d|wk|mo| m|yr|generations") %>%
        dplyr::case_when(
          grepl("hr|rr", .) ~ "hours",
          grepl("d", .) ~ "days",
          grepl("wk", .) ~ "weeks",
          grepl(" m|mo", .) ~ "months",
          grepl("yr", .) ~ "years",
          .default = .
        ),
    ) %>%

    # Remove any instances of N/A, N/I, N/R, and N/S that remain
    dplyr::mutate(dplyr::across(.fns = ~replace(., . %in% c("N/A", "N/I", "N/R", "N/S", "(N/I)"),
                                                NA))) %>%

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




