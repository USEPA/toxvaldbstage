#--------------------------------------------------------------------------------------
#' @description Import of IRIS 2023-05-09 source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @title import_epa_hawc_source
#' @return None; data is pushed to toxval_source
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[stringr]{str_trim}}
#' @rdname import_generic_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_squish
#' @importFrom dplyr mutate across where
#' @importFrom tidyr replace_na
#--------------------------------------------------------------------------------------
import_epa_hawc_source_orchestrate <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "EPA HAWC"
  source_table = "source_epa_hawc"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2025-06-25")
  dir = paste0(toxval.config()$datapath,"epa_hawc/epa_hawc_files/")

  file_list = list.files(dir, pattern = "xlsx", full.names = TRUE)

  hawc_list = lapply(file_list, function(f){
    sheet_list = readxl::excel_sheets(f)
    df_list = lapply(sheet_list, function(s){
      tmp = readxl::read_xlsx(f, sheet=s)
      # If no data, set to NULL
      if(!nrow(tmp)) tmp = NULL
      return(tmp)
    }) %T>% {
      names(.) <- sheet_list
    } %>%
      # Remove NULL values
      purrr::compact()
  }) %T>% {
    names(.) <- basename(file_list) %>% gsub("hawc_|\\.xlsx", "", .)
  } %>%
    # Remove NULL values
    purrr::compact()

  # Loop through all assessment IDs
  res0 = lapply(names(hawc_list), function(a_id){
    message("Processing: ", a_id)
    import_epa_hawc_source(hawc_list[[a_id]]) %>%
      dplyr::mutate(assessment_id = a_id)
  }) %>%
    dplyr::bind_rows()

  # Add source specific transformations
  res = res0 %>%
    dplyr::mutate(toxval_units = dplyr::case_when(
      toxval_units == "mg/kg-day t4" ~ "mg/kg-day (4-weeks dosed feed)",
      TRUE ~ toxval_units
    ))

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  res = res %>%
    # Generic cleanup of strings before dedup check
    dplyr::mutate(
      dplyr::across(dplyr::where(is.character), ~tidyr::replace_na(., "-") %>%
                      fix.replace.unicode() %>%
                      stringr::str_squish()),
      dplyr::across(dplyr::where(is.character), ~gsub("\\r|\\n|\\\\r|\\\\n", "", .)),
      dplyr::across(dplyr::where(is.character), ~gsub("\\\\'", "'", .)),
      dplyr::across(dplyr::where(is.character), ~gsub('\\\\\\"', '"', .))
    )

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"

  # Perform deduping
  res = toxval.source.import.dedup(res, hashing_cols=toxval.config()$hashing_cols)

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
