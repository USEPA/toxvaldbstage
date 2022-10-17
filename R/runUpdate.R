#--------------------------------------------------------------------------------------
#' Runs a database query and returns a result set
#'
#' @param updateQuery a properly formatted SQL query as a string in the form of an UPDATE INNER JOIN
#' @param updated_df a dataframe of updated data to temporarily write to database for INNER JOIN
#' @param db the name of the database
#' @param do.halt if TRUE, halt on errors or warnings
#' @param verbose if TRUE, print diagnostic information
#' @export
#--------------------------------------------------------------------------------------
runUpdate <- function(updateQuery=NULL, updated_df=NULL, db, do.halt=TRUE,verbose=FALSE){
  if(is.null(query)){
    cat("No query provided...\n")
    return(NULL)
  }

  if(!exists("DB.SERVER")) {
    cat("DB.SERVER not defined\n")
    return(NULL)
  }
  if(!exists("DB.USER")) {
    cat("DB.USER not defined\n")
    return(NULL)
  }
  if(!exists("DB.PASSWORD")) {
    cat("DB.PASSWORD not defined\n")
    return(NULL)
  }

  if(verbose) {
    printCurrentFunction()
    cat("query: ",query,"\n")
    cat("db: ",db,"\n")
  }
  tryCatch({
    # Push temp table of updates
    con <- dbConnect(drv=RMySQL::MySQL(),user=DB.USER,password=DB.PASSWORD,host=DB.SERVER,dbname=db)
    res = dbWriteTable(con,
                       name="z_updated_df",
                       value=updated_df,
                       row.names=FALSE,
                       overwrite=TRUE)
    dbDisconnect(con)
    # Push updates
    runStatement(query=update_query, db=db)
    # Drop temp table
    runStatement(query="DROP TABLE IF EXISTS z_updated_df", db=db)
  }, warning = function(w) {
    cat("WARNING:",query,"\n")
    if(do.halt) browser()
  }, error = function(e) {
    #cat("ERROR:",query,"\n")
    cat("Error messge: ",paste0(e, collapse=" | "), "\n")
    if(do.halt) browser()
  }, finally = { dbDisconnect(con) })
}
