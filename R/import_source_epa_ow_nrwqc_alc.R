#--------------------------------------------------------------------------------------
#' Import of EPA OW NRWQC-ALC source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#--------------------------------------------------------------------------------------
import_generic_source <- function(db,chem.check.halt=F) {
  printCurrentFunction(db)
  source = "EPA OW NRWQC-ALC"
  source_table = "source_epa_ow_nrwqc_alc"
  dir = paste0(toxval.config()$datapath,"epa_ow_nrwqc_alc/epa_ow_nrwqc_alc_files/")

  files = list.files(dir)
  res0 = readxl::read_xlsx(paste0(dir,files))
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
  #
  # the final file should have column names that include "name" and "casrn"
  # additionally, the names in res need to match names in the source
  # database table. You do not need to add any of the generic columns
  # described in the SOP - they will get added in source_prep_and_load
  #
  # Standardize the names
  names(res0) <- names(res0) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .)


  #res = source.specific.transformations(res0)

  res <- res0 %>%
    # Renaming columns
    dplyr::rename(name="Pollutant_(P_=_Priority_Pollutant)",
                  casrn="CAS_Number") %>%
    rowwise() %>%
    # Making priority_pollutant column based on (P) in name column
    mutate(priority_pollutant = ifelse(endsWith(name, "(P)"), "yes", "no")) %>%
    ungroup() %>%
    # wide to long based on toxval_type
    tidyr::pivot_longer(
      cols= c("Freshwater_CMC1_(acute)_(µg/L)",
              "Freshwater_CCC2_(chronic)_(µg/L)",
              "Saltwater_CMC1_(acute)_(µg/L)",
              "Saltwater_CCC2_(chronic)_(µg/L)"),
      names_to= "toxval_type",
      values_to= "toxval_numeric") %>%
    # splitting toxval_type into other toxval columns
    tidyr::separate(toxval_type, c("media", "toxval_type", "study_type", "toxval_units"),
                    sep="_", fill="right", remove=TRUE) %>%

    dplyr::mutate(
      # replacing greek letters
      toxval_units = gsub("µ", "u", toxval_units),
      # getting rid of units still in toxval_numeric column
      toxval_numeric = gsub("ug/L", "", toxval_numeric),
      # getting rid of parenthesis around values in certain columns
      toxval_units = gsub("[()]", "", toxval_units),
      study_type = gsub("[()]", "", study_type)) %>%
    # replacing multiple dashes with single dash for empty columns
    dplyr::mutate(across(matches("name|casrn|Publication_Year|toxval_numeric"),
                         .fns = ~ case_when(
                           . == "---" ~ "-",
                           . == "--" ~ "-",
                           . == "—" ~ "-",
                           . == "—" ~ "-",
                           TRUE ~ . )),
                  # getting rid of (P) at end of 'name' column values
                  name = gsub("\\(P[)]$", "", name) %>%
                    # Remove asterisk
                    gsub("*", "", ., fixed=TRUE),
                  across(c("name", "casrn", "toxval_numeric"), ~stringr::str_squish(.))
    ) %>%
    # Split CASRN lists into unique rows
    # https://stackoverflow.com/questions/15347282/split-delimited-strings-in-a-column-and-insert-as-new-rows
    tidyr::separate_rows(casrn, sep=" ")

  # Fix publication year split by media for select chemicals
  res$Publication_Year[res$name == "Ammonia" & res$media == "Freshwater"] = 2013
  res$Publication_Year[res$name == "Ammonia" & res$media == "Saltwater"] = 1989
  res$Publication_Year[res$name == "Selenium" & res$media == "Freshwater"] = 2016
  res$Publication_Year[res$name == "Selenium" & res$media == "Saltwater"] = 1999

  # make column names lowercase now (didn't earlier to keep liter in toxval_units uppercase)
  names(res) <- tolower(names(res))

  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source=source,table=source_table,res=res,F,T,T)
}
