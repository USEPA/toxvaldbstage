#--------------------------------------------------------------------------------------
#' Runs a database query and returns a result set
#'
#' @param src_tbl a toxval source table name
#' @param db the name of the database
#' @export
#--------------------------------------------------------------------------------------
qc_prescreening_summary <- function(src_tbl=NULL,db) {
#Use the runQuery function to extract a source table "src_tbl" from a database "db"
  in_data = runQuery(paste0("select * from ",src_tbl),db)
  cat("Retrieving file from database", "\n")


  #Use line below until database credential are in.
  #in_data = readxl::read_xlsx("C:\\Users\\mmille16\\OneDrive - Environmental Protection Agency (EPA)\\Profile\\Downloads\\source_pfas_150_test.xlsx")
  #Use tidyr pivot longer function to put data in long form

#These columns are not necessary and are just unique identification numbers for the document
  columns <- c("source_hash", "parent_hash", "version", "data_record_annotation",
               "failure_reason", "src_tbl_name", "qc_status", "status_name",
               "create_by", "create_time", "end_time", "chemical_id",
               "source_id", "clowder_id", "document_name", "modify_time", "created_by")
#So lets remove them and work with that dataframe
  data <- in_data[,!(names(in_data)%in%columns)]
  cat("Turning data into long form","\n")


#Begin putting data in long form based on study titles

#Find the column index that holds the study titles
  index <- grep("title",colnames(data))

#tidyr::pivot_longer puts the data "data" into long form based on the columns indicated in "cols"
#In our case, cols only needs the title column. -index tells  pivot_longer to pivot all columns
#EXCEPT the one located at the index "index"
  in_data_long <- tidyr::pivot_longer(data,
                                      cols = -index,
                                      names_to = "field",
                                      values_to = "value",
                                      values_transform= list(value=as.character))
#Now our dataframe has 3 columns, one for the titiles, one for all other fields, and another for their values
  #ex: (title) | author | John Doe
  #Want unique values using group_by, and summarise from dplyr
  cat("Creating summary","\n")

#To create a frequency table, we will use dplry's group_by and summarise functions
#Start by copying the long form data into a new dataframe "df"
  df = in_data_long

#group_by creates groups in the dataframe, in our case the field,value pairs are the groups
#used in conjunction with the summarise funcion, we created a new column called "frequency"
#This column holds the number of occurrences of each field,value pair.
#ex: author | John Doe | 2  means that the (author,John Doe) pair appears twice in our long form data
  df = df %>% group_by(field,value) %>% summarise(frequency = n())


  #Export the data as an xlsx (writexl::write_xlsx)
#Find the location where the excel sheet will be out from
#This will help the user find the output file if they forget where there working directory is
  WD<- getwd() #The working directory of the R instance
  name <- paste("/qc_prescreening_",src_tbl,".xlsx", sep = "") #The file name we will save as
  file <- paste(WD,name,sep = "")

  cat("Outputing excel file as ", WD, name,"\n", sep = "")

#This saves the raw data and summary table to different sheets in the same file
  dataset_names <- list('raw_data' = in_data,'summary' = df)
  write.xlsx(dataset_names,file = file)

  cat("All done! Have fun!","\n")
  return(x)

  #Need to export with multiple sheets
    #One with raw data, one with new data
  #Return both raw and summary data

  #Add some messages as steps are complete with cat(\n *message*)

}

qc_prescreening_summary(src_tbl = "pfas_150_sem", db = "database")
