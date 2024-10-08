#--------------------------------------------------------------------------------------
#' @description Runs a database query and returns a result set
#'
#' @param src_tbl a toxval source table name.
#' @param source_name a toxval source name (used for direct load types).
#' @param outputDir optional directory path to save output file in.
#' @param db the name of the database.
#' @export
#' @title qc_prescreening_summary
#' @return Result set of QC prescreenig information
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[tidyr]{pivot_longer}}
#'  \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}
#'  \code{\link[writexl]{write_xlsx}}
#' @rdname qc_prescreening_summary
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr everything group_by summarise
#' @importFrom writexl write_xlsx
#--------------------------------------------------------------------------------------
qc_prescreening_summary <- function(src_tbl=NULL, source_name=NULL, outputDir=NULL, db=NULL) {
  if(is.null(src_tbl)) stop("No source table name provided...")
  if(is.null(db)) stop("No database name provided...")
  if(is.null(outputDir)) outputDir = getwd()
  if(src_tbl == "direct load" & is.null(source_name)) stop("Direct load 'src_tbl' must have 'source_name'")

  if(src_tbl == "direct load"){
    # Use the runQuery function to extract a source table "src_tbl" from a database "db"
    cat(paste0("Retrieving ", source_name," data from database", "\n"))
    in_data = runQuery(paste0(
      "SELECT b.raw_name, b.raw_casrn, a.* from toxval a ",
      "LEFT JOIN source_chemical b on a.chemical_id = b.chemical_id ",
      "WHERE b.source = '", source_name, "'"), db)
  } else {
    # Use the runQuery function to extract a source table "src_tbl" from a database "db"
    cat(paste0("Retrieving ", src_tbl," data from database", "\n"))
    in_data = runQuery(paste0("select * from ",src_tbl),db)
  }

  # Use tidyr pivot longer function to put data in long form
  # These columns are not necessary and are just unique identification numbers for the document
  id_columns <- c("data_record_annotation", "failure_reason", "src_tbl_name",
                  "status_name", "create_by", "end_time", toxval.config()$non_hash_cols)

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
    dplyr::group_by(field,value) %>%
    # used in conjunction with the summarise funcion, we created a new column called "frequency"
    # This column holds the number of occurrences of each field,value pair.
    dplyr::summarise(frequency = n())
    # ex: author | John Doe | 2  means that the (author,John Doe) pair appears twice in our long form data

  # Export the data as an xlsx (writexl::write_xlsx) to preserve the CASRN values (no CSV date conversion)
  # Find the location where the excel sheet will be out from
  # This will help the user find the output file if they forget where there working directory is

  out_file = paste0(outputDir, "/qc_prescreening_",
                    ifelse(src_tbl == "direct load", source_name, src_tbl),
                    "_", Sys.Date(),".xlsx")
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
