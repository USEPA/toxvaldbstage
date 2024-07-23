#' @description Runs a PSQL database query and returns a result set
#'
#' @param query a properly formatted SQL query as a string
#' @param db the name of the database
#' @param do.halt if TRUE, halt on errors or warnings
#' @param verbose if TRUE, print diagnostic information
#' @title runQuery_psql
#' @return Query results
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[RMySQL]{character(0)}}
#'  \code{\link[RPostgreSQL]{PostgreSQL}}
#'  \code{\link[DBI]{dbGetQuery}}
#' @rdname runQuery_psql
#' @importFrom RMySQL dbConnect dbDisconnect
#' @importFrom RPostgreSQL PostgreSQL
#' @importFrom DBI dbGetQuery
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
    con <- RMySQL::dbConnect(drv=RPostgreSQL::PostgreSQL(),user=DB.PSQLUSER,port=DB.PSQLPORT,password=DB.PSQLPASSWORD,host=DB.PSQLSERVER,dbname=db)
    d1 <- DBI::dbGetQuery(con,query)
    RMySQL::dbDisconnect(con)
    return(d1)
  })
}

