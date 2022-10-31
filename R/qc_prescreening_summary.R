#--------------------------------------------------------------------------------------
#' Runs a database query and returns a result set
#'
#' @param src_tbl a toxval source table name
#' @param outputDir optional directory path to save output file in
#' @param db the name of the database
#' @export
#--------------------------------------------------------------------------------------
qc_prescreening_summary <- function(src_tbl=NULL, outputDir=NULL, db=NULL) {
  if(is.null(src_tbl)) stop("No source table name provided...")
  if(is.null(db)) stop("No database name provided...")
  if(is.null(outputDir)) outputDir = getwd()
  # Use the runQuery function to extract a source table "src_tbl" from a database "db"
  cat(paste0("Retrieving ",src_tbl," data from database", "\n"))
  in_data = runQuery(paste0("select * from ",src_tbl),db)

  # Use tidyr pivot longer function to put data in long form
  # These columns are not necessary and are just unique identification numbers for the document
  id_columns <- c("source_hash", "parent_hash", "version", "data_record_annotation",
               "failure_reason", "src_tbl_name", "qc_status", "status_name",
               "create_by", "create_time", "end_time", "chemical_id",
               "source_id", "clowder_id", "document_name", "modify_time", "created_by")
  # So remove them and work with that dataframe
  data <- in_data[, !(names(in_data) %in% id_columns)]
  cat("Turning data into long form","\n")
  # With ID columns removed, we pivot all columns
  in_data_long <- tidyr::pivot_longer(data,
                                      cols = dplyr::everything(),
                                      names_to = "field",
                                      values_to = "value",
                                      values_transform= list(value=as.character)
                                      )
  # Dataframe has 2 columns, one for all other fields, and another for their values
  # ex: author | John Doe
  # Want unique values using group_by, and summarise from dplyr
  cat("Creating summary","\n")
  # To create a frequency table, we will use dplry's group_by and summarise functions
  # Start by copying the long form data into a new dataframe "df"
  freq_df = in_data_long %>%
    # group_by creates groups in the dataframe, in our case the field,value pairs are the groups
    group_by(field,value) %>%
    # used in conjunction with the summarise funcion, we created a new column called "frequency"
    # This column holds the number of occurrences of each field,value pair.
    dplyr::summarise(frequency = n())
    # ex: author | John Doe | 2  means that the (author,John Doe) pair appears twice in our long form data

  # Export the data as an xlsx (writexl::write_xlsx) to preserve the CASRN values (no CSV date conversion)
  # Find the location where the excel sheet will be out from
  # This will help the user find the output file if they forget where there working directory is
  out_file = paste0(outputDir, "/qc_prescreening_", src_tbl, ".xlsx")
  cat("Outputing excel file as: ", out_file)
  # This saves the raw data and summary table to different sheets in the same file
    writexl::write_xlsx(x = list('raw_data' = in_data,
                                 'summary' = freq_df),
                        # The file name we will save as, using the inpu parameters
                        path = out_file)

  cat("All done! Have fun!","\n")
  return(list('raw_data' = in_data,
       'summary' = freq_df))
}

# qc_prescreening_summary(src_tbl = "source_pfas_150_sem", db = "database")
