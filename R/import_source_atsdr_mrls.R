#--------------------------------------------------------------------------------------
#' @title import_source_astdr_mrls
#' @description Send ASTDR MRLs data to toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @param do.toxicological_profile If TRUE, add toxicological profile data to table before insertion
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
#'  \code{\link[tidyr]{separate}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{case_when}}
#'  \code{\link[stringr]{str_trim}}
#' @rdname import_source_atsdr_mrls
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom tidyr separate
#' @importFrom dplyr mutate case_when
#' @importFrom stringr str_squish
#--------------------------------------------------------------------------------------
import_source_atsdr_mrls <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE, do.toxicological_profile=FALSE) {
  printCurrentFunction(db)
  source = "ATSDR MRLs"
  source_table = "source_atsdr_mrls"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2024-01-01")
  dir = paste0(toxval.config()$datapath,"atsdr_mrls/atsdr_mrls_files/")
  file = paste0(dir,"atsdr_mrls_extraction_20240101.xlsx")
  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  res = res0 %>%
    # Get toxval_numeric and toxval_units
    tidyr::separate(
      col = "MRL",
      into = c("toxval_numeric", "toxval_units"),
      sep = " ",
      remove = TRUE
    ) %>%
    dplyr::rename(name = `Name`) %>%
    dplyr::mutate(
      # Basic renaming/add general information
      casrn = `CAS Number`,
      doc_status = Status,
      doc_cover_date = `Cover Date`,
      toxval_type = "MRL",
      source_url = "https://wwwn.cdc.gov/TSP/MRLS/mrlsListing.aspx",

      # Get year
      year = doc_cover_date %>%
        gsub("[0-9]+\\/", "", .) %>%
        as.Date(., format = "%y") %>%
        format(., "%Y"),

      # Remove symbols from name and units
      name = fix.replace.unicode(name),
      toxval_units = fix.replace.unicode(toxval_units),

      # Ensure toxval_numeric is of numeric type
      toxval_numeric = as.numeric(toxval_numeric),

      # Expand abbreviated terms
      exposure_route = dplyr::case_when(
        Route == "Inh." ~ "Inhalation",
        Route == "Rad." ~ "External Radiation",
        TRUE ~ Route
      ),
      study_type = dplyr::case_when(
        Duration == "Chr." ~ "chronic",
        Duration == "Int." ~ "intermediate",
        TRUE ~ tolower(Duration)
      ),
      critical_effect = Endpoint %>%
        gsub("Body Wt.", "body weight", .) %>%
        gsub("Develop.", "developmental", .) %>%
        gsub("Endocr.", "endocrine", .) %>%
        gsub("Gastro.", "gastrointestinal", .) %>%
        gsub("Hemato.", "hematological", .) %>%
        gsub("Immuno.", "immunological", .) %>%
        gsub("Lymphor.", "lymphatic", .) %>%
        gsub("Metab.", "metabolic", .) %>%
        gsub("Musculo.", "musculoskeletal", .) %>%
        gsub("Neurol.", "neurological", .) %>%
        gsub("Reprod.", "reproductive", .) %>%
        gsub("Resp.", "respiratory", .) %>%
        gsub(",", ", ", .) %>%
        stringr::str_squish() %>%
        tolower(),

      # Get study duration values based on study_type
      study_duration_value = dplyr::case_when(
        study_type == "acute" ~ "1-14",
        study_type == "intermediate" ~ "15-364",
        study_type == "chronic" ~ "1",
        TRUE ~ as.character(NA)
      ),
      study_duration_units = dplyr::case_when(
        study_type == "acute" ~ "days",
        study_type == "intermediate" ~ "days",
        study_type == "chronic" ~ "year",
        TRUE ~ as.character(NA)
      ),
      study_duration_qualifier = dplyr::case_when(
        study_type == "chronic" ~ ">",
        TRUE ~ "="
      )
    )

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  # Add summary data to df before prep and load
  if(do.toxicological_profile){
    # Import manually curated IRIS Summary information
    summary_file = paste0(dir,"source_atsdr_mrls_manual_pod_awebb01_20240307.xlsx")
    res1 <- readxl::read_xlsx(summary_file) %>%
      dplyr::filter(toxval_type != "MRL") %>%
      dplyr::mutate(document_type = "ATSDR MRLs Toxicological Profile") %>%
      dplyr::rename(long_ref = full_study_reference,
                    source_url = document_url,
                    species = species_original) %>%
      # Squish all character fields
      dplyr::mutate(dplyr::across(where(is.character), stringr::str_squish))
    res <- res %>%
      dplyr::mutate(document_type = "ATSDR MRLs")
  } else {
    # If no manual curation of PODs, provisionally calculate NOAEL with UF
    res1 <- res %>%
      dplyr::mutate(toxval_numeric = as.numeric(toxval_numeric) * as.numeric(`Total Factors`),
                    toxval_type = "NOAEL",
                    toxval_subtype = 'Provisional: MRL multiplied by UF')
  }
  # Combine with manual or provisional
  res = res %>%
    dplyr::bind_rows(res1) %>%
    dplyr::distinct()

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"

  # Check for duplicate records early
  res.temp = source_hash_vectorized(res, hashing_cols=toxval.config()$hashing_cols)
  res$source_hash = res.temp$source_hash

  # Dedup by collapsing non hashing columns to dedup
  res = res %>%
    dplyr::group_by(source_hash) %>%
    dplyr::mutate(dplyr::across(-dplyr::any_of(c("source_hash", toxval.config()$hashing_cols)),
                                ~paste0(.[!is.na(.)], collapse=" |::| ") %>%
                                  na_if("NA") %>%
                                  na_if("")
    )) %>%
    # dplyr::summarise(linkage_id = toString(linkage_id)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()

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




