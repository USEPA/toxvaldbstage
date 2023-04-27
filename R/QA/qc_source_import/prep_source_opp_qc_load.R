#' @title prep_source_opp_qc_load
#' @description Function to match QC'd file data to source_hash values before QC loading.
#' Special case where all reviewed passed without edits, so just join to get source_hash
#' and push QC'd records.
#' @param in_file Filepath to QC'd data (from Jira ticket TOXVAL-309)
#' @param db The version of toxval_source into which the source is loaded.
#' @return DataFrame of QC'd data with matched source_hash field
prep_source_opp_qc_load <- function(in_file, db){

  # Pull source_opp data
  db_data <- runQuery("SELECT * FROM source_opp", db) %>%
    dplyr::select("source_hash", "casrn", "name", "toxval_type", "toxval_numeric", "toxval_units",
             "risk_assessment_class", "sensitive_lifestage", "url")
  # Pull QC'd data (from Jira ticket TOXVAL-309)
  qc_data <- readxl::read_xlsx(in_file) %>%
    dplyr::select(-random_sample)
  # Set missing to "-"
  qc_data[is.na(qc_data)] <- "-"

  # Prep QC'd data
  out <- qc_data %>%
    # Filter to only QC'd data
    dplyr::filter(qc_status == "PASS") %>%
    # Join to get source_hash values
    dplyr::left_join(db_data,
              by=c("casrn", "name", "toxval_type", "toxval_numeric", "toxval_units",
                   "risk_assessment_class", "sensitive_lifestage", "url")) %>%
    # Filter only to those that matched
    dplyr::filter(!is.na(source_hash)) %>%
    dplyr::select(source_hash, qc_status) %>%
    # Add QC information
    dplyr::mutate(qc_notes = "PASS without changes. 5% record review due to autoextraction of document",
           created_by = "erowan")

  # Query to join and make updates
  updateQuery = paste0("UPDATE source_opp a INNER JOIN z_updated_df b ",
                       "ON (a.source_hash = b.source_hash) SET ",
                       paste0("a.", names(out)[!names(out) %in% c("source_hash")],
                              " = b.", names(out)[!names(out) %in% c("source_hash")],
                              collapse = ", ")
  )
  runUpdate(table="source_opp", updateQuery=updateQuery, updated_df=out, db=db, trigger_check=FALSE)
}

# writexl::write_xlsx(out2, "qc_source_opp_push.xlsx")
