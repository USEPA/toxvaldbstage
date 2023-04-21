#-------------------------------------------------------------------------------------
#' @#' 
#'
#' @description Function to combine output log with output message
#' @title FUNCTION_TITLE
#' @param log_df PARAM_DESCRIPTION
#' @param message_df_col PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname output_log
#' @export 
#-------------------------------------------------------------------------------------
log_message <- function(log_df,message_df_col){
  printCurrentFunction()
  # start logging message after 2nd occurance of line breaker(===)
  start_log <-  log_df[1:grep("^\\=",log_df[,1])[2],]
  # stop log after the occurance of last line breaker(===)
  stop_log <- log_df[grep("^\\=",log_df[,1])[2] : grep("^Log Elapsed Time|^NOTE\\: Elapsed Time",log_df[,1])+1,]
  return(c(start_log, message_df_col, stop_log))
}


