# #library(RPostgreSQL)
# library(DBI)
#--------------------------------------------------------------------------------------
#' @description Get the names the database server, user, and pass or returns error message
#' @return print the database connection information
#' @title getPSQLDBConn
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname getPSQLDBConn
#--------------------------------------------------------------------------------------
getPSQLDBConn <- function() {
  printCurrentFunction()
  if(!exists("DB.PSQLSERVER")) cat("DB.PSQLSERVER not defined\n")
  else cat("DB.PSQLSERVER: ",DB.PSQLSERVER,"\n")
  if(!exists("DB.PSQLPORT")) cat("DB.PSQLPORT not defined\n")
  else cat("DB.PSQLPORT: ",DB.PSQLPORT,"\n")
  if(!exists("DB.PSQLUSER")) cat("DB.PSQLUSER not defined\n")
  else cat("DB.PSQLUSER: ",DB.PSQLUSER,"\n")
  if(!exists("DB.PSQLPASSWORD")) cat("DB.PSQLPASSWORD not defined\n")
  else cat("DB.PSQLPASSWORD: ",DB.PSQLPASSWORD,"\n")
}
