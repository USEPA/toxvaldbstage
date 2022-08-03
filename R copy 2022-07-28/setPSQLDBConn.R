#library(RPostgreSQL)
library(DBI)
#--------------------------------------------------------------------------------------
#' set PSQL connection to the database
#' @param server SQL server on which relevant database lives
#' @param user SQL username to access database
#' @param password SQL password corresponding to username
#--------------------------------------------------------------------------------------
setPSQLDBConn <- function(server,port,user,password) {
  printCurrentFunction()
  DB.PSQLSERVER <<- server
  DB.PSQLPORT <<- port
  DB.PSQLUSER <<- user
  DB.PSQLPASSWORD <<- password
}
