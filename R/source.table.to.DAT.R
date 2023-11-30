# Script to process toxval source table into DAT application input format.
# By: Jonathan Taylor Wall
# Created: 2022-06-2
# R version 4.1.0 (2021-05-18)
# dplyr_1.0.8; RMySQL_0.10.23; DBI_1.1.2; readxl_1.3.1; magrittr_2.0.2;
# tidyr 1.2.0; writexl 1.4.0

#-------------------------------------------------------------------------------------
#' @title source.table.to.DAT
#' @description Convert toxval source table to DAT format for loading to DAT
#' application
#' @param source.db The version of toxval source to use.
#' @param source The name of toxval source to use.
#' @param source_table The name of toxval source table to use. If a DataFrame, input data will be #' processing and returned without saving to file.
#' @param limit Excel file grouping limit (default is max XLSX row limit)
#' @param sample_p Percentage of records to sample down to
#' @return Processed source table to DAT format cached and returned.
#' @import dplyr RMySQL DBI readxl magrittr tidyr writexl
#' @export
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{rename}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{slice}}, \code{\link[dplyr]{select}}
#'  \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{reexports}}
#'  \code{\link[writexl]{write_xlsx}}
#' @rdname source.table.to.DAT
#' @importFrom dplyr rename filter slice_sample select
#' @importFrom tidyr pivot_longer all_of
#' @importFrom writexl write_xlsx
#--------------------------------------------------------------------------------------
source.table.to.DAT <- function(source.db, source_table, limit = 1000000, sample_p = NA){
  # Option to inject data directly to turn into DAT format
  if(!is.data.frame(source_table)){
    # Skip if does not have the required ID field
    name_check = runQuery(paste("SELECT * FROM ", source_table, " LIMIT 1"),
                          db=source.db) %>%
      names()
    if(!"source_hash" %in% name_check) {
      message("...", source_table, " does not have required source_hash field...skipping...")
      return()
    }
    # Pull source table data
    src_data = runQuery(paste("SELECT * FROM ", source_table),
                        db=source.db) %>%
      # Set record_id for DAT template from source_hash
      dplyr::rename(record_id = source_hash) %T>% {
        # Get sample count based on sample_p
        sample_nrec <<- ceiling(nrow(.) * sample_p)
      } %>%
      dplyr::filter(clowder_id != "-")

    # Sample down if sample_p parameter used
    if(!is.na(sample_nrec)){
      src_data = src_data %>%
        #group_by(clowder_id) %>%
        dplyr::slice_sample(n=sample_nrec)
    }
  } else {
    src_data = source_table %>%
      dplyr::rename(record_id = source_hash) %>%
      dplyr::filter(clowder_id != "-")
  }

  # Remove columns
  rm_list = c(toxval.config()$non_hash_cols,
              c("source", "subsource"))
  src_data[, rm_list] <- NULL
  # Set ID column for pivot
  id_cols = c("record_id")
  # Set list of template columns to add
  template_cols = c("dataset_name", "domain_name", "source_name", "document_id",
                    "document_name", "document_path")

  # Get Clowder document fields
  rec_docs <- runQuery(paste0("SELECT a.clowder_id as source_name, b.source_hash as record_id FROM documents a ",
                              "LEFT JOIN documents_records b ",
                              "ON b.fk_doc_id = a.id ",
                              "WHERE source_hash in ('",
                              paste0(src_data$record_id, collapse="', '")
                              ,"')"),
                       source.db)


  # Load and transform data
  in_dat = src_data %>%
    dplyr::left_join(rec_docs,
                     by="record_id") %>%
    tidyr::pivot_longer(cols=-c(record_id, tidyr::all_of(id_cols)),
                        names_to="field_name",
                        values_to="value",
                        # Convert column values to character
                        values_transform = list(value = as.character))
  # Add additional template fields not already present
  in_dat[template_cols[!template_cols %in% names(in_dat)]] <- ""
  # Replace missing value entries with empty string
  in_dat$value[is.na(in_dat$value)] = ""
  # Reorder columns to fit template order
  in_dat = in_dat %>%
    dplyr::select(dataset_name, domain_name, source_name,
                  record_id, field_name, value,
                  document_id, document_name, document_path)
  # Prep export location (check and create if not present)
  if(!dir.exists("Repo/DAT Input")) dir.create("Repo/DAT Input")

  if(!is.data.frame(source_table)){
    # Export transformation in groups based on limit input
    nr <- nrow(in_dat)
    out_dat = split(in_dat, rep(1:ceiling(nr/limit), each=limit, length.out=nr))
    # Write output files
    for(i in seq_len(length(out_dat))){
      writexl::write_xlsx(x=list(data=out_dat[[i]]),
                          path = paste0("Repo/DAT Input/", source_table, "_DAT_input_",i,".xlsx"))
    }
  } else {
    out_dat = in_dat
  }

  return(out_dat)
}
