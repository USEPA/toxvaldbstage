#--------------------------------------------------------------------------------------
#' Prep the source data aand load
#' @param db The version of toxval_source into which the source is loaded.
#' @param source Name of the source
#' @param table Name of the database table
#' @param res The data frame to be processed
#' @param do.reset If TRUE, delete data from the database for this source before
#' inserting new data. Default FALSE
#' @param do.insert If TRUE, insert data into the database, default TRUE
#' @param chem.check.halt If TRUE, stop the execution if there are errors in the
#' chemical  mapping
#--------------------------------------------------------------------------------------
source_prep_and_load <- function(db,source,table,res,
                                 do.reset=FALSE,do.insert=FALSE,chem.check.halt=FALSE){
  printCurrentFunction(paste(db,"\n",source,":",table))

  chem.check.halt = F

  #####################################################################
  cat("Generating source table in database\n")
  # create_source_table_SQL(source=source, res=res, db=db)
  #####################################################################

  #####################################################################
  cat("Do the chemical checking\n")
  #####################################################################
  res = as.data.frame(res)
  res$source = source
  res$clowder_id = "-"
  if(!is.element(source,c("HESS"))) res$document_name = "-"
  res$qc_status = "-"
  res = fix.non_ascii.v2(res,source)
  res = source_chemical.process(db,res,source,chem.check.halt,casrn.col="casrn",name.col="name")

  #####################################################################
  cat("Set the default values for missing data\n")
  #####################################################################
  res = source_set_defaults(res,source)

  #####################################################################
  cat("Set the clowder_id and document name\n")
  #####################################################################
  res = set_clowder_id(res,source)

  #####################################################################
  cat("Build the hash key and load the data \n")
  #####################################################################
  toxval_source.hash.and.load(db,source,table,F,T,res)
}
