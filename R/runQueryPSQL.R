#library(RPostgreSQL)
library(DBI)
#--------------------------------------------------------------------------------------
#' Runs a PSQL database query and returns a result set
#'
#' @param query a properly formatted SQL query as a string
#' @param db the name of the database
#' @param do.halt if TRUE, halt on errors or warnings
#' @param verbose if TRUE, print diagnostic information
#' @export
#--------------------------------------------------------------------------------------
runQuery_psql <- function(query,db,do.halt=T,verbose=T) {

  if(!exists("DB.PSQLSERVER")) {
    cat("DB.PSQLSERVER not defined\n")
    return(NULL)
  }
  if(!exists("DB.PSQLPORT")) {
    cat("DB.PSQLPORT not defined\n")
    return(NULL)
  }
  if(!exists("DB.PSQLUSER")) {
    cat("DB.PSQLUSER not defined\n")
    return(NULL)
  }
  if(!exists("DB.PSQLPASSWORD")) {
    cat("DB.PSQLPASSWORD not defined\n")
    return(NULL)
  }
  if(verbose) {
    printCurrentFunction()
    cat("query: ",query,"\n")
    cat("db: ",db,"\n")
  }
  tryCatch({
    con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),user=DB.PSQLUSER,port=DB.PSQLPORT,password=DB.PSQLPASSWORD,host=DB.PSQLSERVER,dbname=db)
    d1 <- dbGetQuery(con,query)

    dbDisconnect(con)

    return(d1)
  })
}

