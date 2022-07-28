#-------------------------------------------------------------------------------------
#' Extract ECOTOX from the datahub to a file
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param verbose Whether the loaded rows should be printed to the console.
#' @param do.load If TRUE, load the data from the input file and put into a global variable
#' @export
#--------------------------------------------------------------------------------------
ecotox.datahub.to.file <- function(toxval.db,verbose=T,do.load=F) {
  printCurrentFunction()


  user = "rjudson"
  password = "Rat64?xYzW!e2w^Z"
  setPSQLDBConn(user=user,password=password,server="ccte-pgsql-prod.epa.gov",port=9999)
  runQuery_psql(query,db)


  }
