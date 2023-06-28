# #library(RPostgreSQL)
# library(DBI)
#--------------------------------------------------------------------------------------
#' @description set PSQL connection to the database
#' @param server SQL server on which relevant database lives
#' @param user SQL username to access database
#' @param password SQL password corresponding to username
#' @param api_auth API Key for CCTE API's
#' @title FUNCTION_TITLE
#' @param port PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname setPSQLDBConn
#' @export
#--------------------------------------------------------------------------------------
setPSQLDBConn <- function(server,port,user,password,api_auth) {
  printCurrentFunction()
  DB.PSQLSERVER <<- server
  DB.PSQLPORT <<- port
  DB.PSQLUSER <<- user
  DB.PSQLPASSWORD <<- password
  API_AUTH <<- api_auth
}
