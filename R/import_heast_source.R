#--------------------------------------------------------------------------------------
#' @title import_heast_source
#' @description Load HEAST data into toxval_source
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
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{c("rowwise", "rowwise")}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{na_if}}, \code{\link[dplyr]{row_number}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{case_when}}
#'  \code{\link[tidyselect]{all_of}}, \code{\link[tidyselect]{starts_with}}
#'  \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_extract}}
#'  \code{\link[tidyr]{drop_na}}
#' @rdname import_heast_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr filter select rowwise mutate ungroup na_if row_number rename rename_with bind_rows case_when
#' @importFrom tidyselect all_of starts_with
#' @importFrom stringr str_squish str_extract
#' @importFrom tidyr drop_na
#--------------------------------------------------------------------------------------
import_heast_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "HEAST"
  source_table = "source_heast"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("1997-07-31")
  dir = paste0(toxval.config()$datapath,"heast/heast_files/")
  file = paste0(dir,"EPA_HEAST_Table1_ORNL for loading.xlsx")
  res0 = readxl::read_xlsx(file)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  # Combine critical_effect entries (logic adapted from previous import script version)
  res = res0[order(res0$row_id),]
  res$keep = 0
  start = 0
  pointer = -1
  for(i in 1:nrow(res)) {
    if(!is.na(res[i,"name"])) {
      if(i>1) {
        res[pointer,"target"] = target
        res[pointer,"critical_effect"] = ce
      }
      pointer = i
      start = 1
      ce = paste0(res[pointer, "target"], ": ", res[pointer,"critical_effect"])
      if(!is.na(res[pointer, "target"])) {
        target = res[pointer, "target"]
        ce = paste0(res[pointer, "target"], ": ", res[pointer,"critical_effect"])
      }
      else if(!(res[pointer,"critical_effect"] %in% c("None observed", as.character(NA)))) {
        target = as.character(NA)
        ce = res[pointer,"critical_effect"]
      }
      else {
        target = as.character(NA)
        ce = as.character(NA)
      }
      res[i,"keep"] = 1
    }
    else {
      if(!is.na(res[i, "target"])) {
        ce = paste0(ce, "|", res[i, "target"], ": ", res[i,"critical_effect"])
        target = paste0(target, "; ", res[i, "target"])
      } else if(!(res[pointer,"critical_effect"] %in% c("None observed", as.character(NA)))) {
        ce = ce = paste0(ce, "|", res[i,"critical_effect"])
      } else next
    }
  }

  res = res %>%
    # Drop unused rows/columns after getting extra critical_effect information
    dplyr::filter(keep==1, toxval_type!="Critial") %>%
    dplyr::select(-tidyselect::all_of(c("keep", "row_id"))) %>%
    # Add original row as relationship ID before split
    dplyr::mutate(relationship_id = dplyr::row_number())

  # Below is logic to further collapse critical_effect, comment, ornl_table, and target
  # Uncomment if additional collapsing is desired
  # # Use deduping function to improve collapse behavior for critical_effect
  # hashing_cols = toxval.config()$hashing_cols[!(toxval.config()$hashing_cols %in% c("critical_effect"))]
  # dedup_cols = c("critical_effect", "ornl_table", "comment", "target")
  # res = toxval.source.import.dedup(res, hashing_cols=hashing_cols) %>%
  #   dplyr::rowwise() %>%
  #   # Create unique list of entries, then collapse for each deduped col
  #   dplyr::mutate(
  #     critical_effect = critical_effect %>%
  #       strsplit(" \\|::\\| ") %>%
  #       unlist() %>%
  #       unique() %>%
  #       paste0(collapse=" |::| "),
  #     ornl_table = ornl_table %>%
  #       strsplit(" \\|::\\| ") %>%
  #       unlist() %>%
  #       unique() %>%
  #       paste0(collapse=" |::| "),
  #     comment = comment %>%
  #       strsplit(" \\|::\\| ") %>%
  #       unlist() %>%
  #       unique() %>%
  #       paste0(collapse=" |::| "),
  #     target = target %>%
  #       gsub(" \\|::\\| ", "; ", .) %>%
  #       strsplit("; ") %>%
  #       unlist() %>%
  #       unique() %>%
  #       paste0(collapse=" |::| ")
  #   ) %>%
  #   dplyr::ungroup() %>%
  #
  #   # Replace "|::|" in deduped fields with appropriate delimiter
  #   dplyr::mutate(
  #     critical_effect = critical_effect %>%
  #       gsub(" \\|::\\| ", "|", .) %>%
  #       gsub(": NA", "", .) %>%
  #       dplyr::na_if("NA"),
  #     ornl_table = ornl_table %>%
  #       gsub(" \\|::\\| ", "; ", .) %>%
  #       dplyr::na_if("NA"),
  #     comment = comment %>%
  #       gsub(" \\|::\\| ", ". ", .) %>%
  #       gsub("\\.\\.| \\.", ".", .) %>%
  #       stringr::str_squish() %>%
  #       dplyr::na_if("NA"),
  #     target = target %>%
  #       gsub(" \\|::\\| ", "; ", .) %>%
  #       dplyr::na_if("NA"),
  #
  #     # Add original row as relationship ID before split
  #     relationship_id = dplyr::row_number()
  #   )

  # Separate and handle NOEL/LOEL, RfC, and RfD fields
  res_orig = res %>%
    dplyr::select(-tidyselect::starts_with("rfc")) %>%
    dplyr::select(-tidyselect::starts_with("rfd")) %>%
    dplyr::mutate(toxval_uf=as.character(NA),
                  toxval_numeric=as.character(toxval_numeric))

  res_rfc_chronic = res %>%
    dplyr::select(-c(tidyselect::starts_with("rfd"), "toxval_type", "toxval_numeric", "toxval_units")) %>%
    dplyr::select(-tidyselect::starts_with("rfc_subchronic")) %>%
    dplyr::mutate(toxval_type="RfC",
                  study_duration_class="chronic",
                  study_duration_value=NA,
                  study_duration_units=as.character(NA)) %>%
    dplyr::rename(toxval_numeric=rfc_chronic) %>%
    dplyr::rename_with(function(x) gsub("rfc_chronic_?", "toxval_", x)) %>%
    dplyr::mutate(toxval_uf=as.character(toxval_uf),
                  toxval_numeric=as.character(toxval_numeric))

  res_rfc_subchronic = res %>%
    dplyr::select(-c(tidyselect::starts_with("rfd"), "toxval_type", "toxval_numeric", "toxval_units")) %>%
    dplyr::select(-tidyselect::starts_with("rfc_chronic")) %>%
    dplyr::mutate(toxval_type="RfC",
                  study_duration_class="subchronic",
                  study_duration_value=NA,
                  study_duration_units=as.character(NA)) %>%
    dplyr::rename(toxval_numeric=rfc_subchronic) %>%
    dplyr::rename_with(function(x) gsub("rfc_subchronic_?", "toxval_", x)) %>%
    dplyr::mutate(toxval_uf=as.character(toxval_uf),
                  toxval_numeric=as.character(toxval_numeric))

  res_rfd_chronic = res %>%
    dplyr::select(-c(tidyselect::starts_with("rfc_"), "toxval_type", "toxval_numeric", "toxval_units")) %>%
    dplyr::select(-tidyselect::starts_with("rfd_subchronic")) %>%
    dplyr::mutate(toxval_type="RfD",
                  study_duration_class="chronic",
                  study_duration_value=NA,
                  study_duration_units=as.character(NA)) %>%
    dplyr::rename(toxval_numeric=rfd_chronic) %>%
    dplyr::rename_with(function(x) gsub("rfd_chronic_?", "toxval_", x)) %>%
    dplyr::mutate(toxval_uf=as.character(toxval_uf),
                  toxval_numeric=as.character(toxval_numeric))

  res_rfd_subchronic = res %>%
    dplyr::select(-c(tidyselect::starts_with("rfc_"), "toxval_type", "toxval_numeric", "toxval_units")) %>%
    dplyr::select(-tidyselect::starts_with("rfd_chronic")) %>%
    dplyr::mutate(toxval_type="RfD",
                  study_duration_class="subchronic",
                  study_duration_value=NA,
                  study_duration_units=as.character(NA)) %>%
    dplyr::rename(toxval_numeric=rfd_subchronic) %>%
    dplyr::rename_with(function(x) gsub("rfd_subchronic_?", "toxval_", x)) %>%
    dplyr::mutate(toxval_uf=as.character(toxval_uf),
                  toxval_numeric=as.character(toxval_numeric))

  # Recombine original and RfD/RfC data
  res = dplyr::bind_rows(res_orig, res_rfc_chronic, res_rfc_subchronic,
                         res_rfd_chronic, res_rfd_subchronic) %>%
    dplyr::rename(uf=toxval_uf) %>%
    tidyr::drop_na(toxval_numeric, toxval_type, toxval_units) %>%

    dplyr::mutate(
      # Set values based on toxval_type
      human_ra = dplyr::case_when(
        toxval_type %in% c("RfD", "RfC") ~ "Y",
        TRUE ~ "N"
      ),
      target_species = dplyr::case_when(
        toxval_type %in% c("RfD", "RfC") ~ "Human",
        TRUE ~ as.character(NA)
      ),
      species = dplyr::case_when(
        toxval_type %in% c("RfD", "RfC") ~ as.character(NA),
        TRUE ~ species
      ),

      # Add hardcoded columns
      human_eco = "human_health",
      source_url = "https://cfpub.epa.gov/ncea/risk/recordisplay.cfm?deid=2877",
      study_type = study_duration_class,

      # Perform other transformations as necessary
      toxval_numeric = as.numeric(toxval_numeric),
      casrn = sapply(casrn, FUN=fix.casrn) %>% dplyr::na_if("NOCAS"),

      # Extract study_duration
      study_duration_qualifier = dplyr::case_when(
        grepl("<|>", study_duration_value) ~ stringr::str_extract(study_duration_value, "<|>"),
        grepl("up to", study_duration_value) ~ "<=",
        grepl("\\+\\/\\-", study_duration_value) ~ "~",
        TRUE ~ "="
      ),
      study_duration_value = dplyr::case_when(
        study_duration_value %in% c("Acute", "Chronic", "Single dose",
                                    "Multi-generation", "Subchronic") ~ as.character(NA),
        TRUE ~ study_duration_value
      ) %>%
        gsub("Lifetime", "1", .) %>%
        gsub("<|>|generations|42409|\\+.+|Up to|\\bor.+", "", .) %>%
        gsub("\\s?to\\s?", "-", .) %>%
        stringr::str_squish(),
      study_duration_units = dplyr::case_when(
        study_duration_units %in% c("Acute", "Chronic", "Single dose",
                                    "Multi-generation", "Subchronic") ~ as.character(NA),
        grepl("Inhalation: intermittent", study_duration_units) ~ "weeks",
        study_duration_units == "3 generations, 2 years" ~ "years",
        grepl("generation", study_duration_units) ~ "generations",
        TRUE ~ study_duration_units
      ),

      # Set study_duration to NA if value or units is missing
      study_duration_qualifier = dplyr::case_when(
        is.na(study_duration_value) | is.na(study_duration_units) ~ as.character(NA),
        TRUE ~ study_duration_qualifier
      ),
      study_duration_units = dplyr::case_when(
        is.na(study_duration_units) ~ as.character(NA),
        TRUE ~ study_duration_units
      ) %>% tolower(),
      study_duration_value = dplyr::case_when(
        is.na(study_duration_units) ~ as.character(NA),
        TRUE ~ study_duration_value
      ),

      # Clean species and extract lifestage where possible
      lifestage = stringr::str_extract(species, "young|infant"),
      species = species %>%
        gsub("occupational|infants|, young", "", .) %>%
        tolower() %>%
        stringr::str_squish()
    )

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"

  # Call deduping function
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




