#--------------------------------------------------------------------------------------
#' @description Prep the source data aand load
#' @param db The version of toxval_source into which the source is loaded.
#' @param source Name of the source
#' @param table Name of the database table
#' @param res The data frame to be processed
#' @param do.reset If TRUE, delete data from the database for this source before #' inserting new data. Default FALSE
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @param chem.check.halt If TRUE, stop the execution if there are errors in the #' chemical mapping
#' @title FUNCTION_TITLE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[tidyr]{reexports}}
#' @rdname source_prep_and_load
#' @export
#--------------------------------------------------------------------------------------
source_prep_and_load <- function(db,source,table,res,
                                 do.reset=FALSE, do.insert=FALSE,
                                 chem.check.halt=FALSE){
  printCurrentFunction(paste(db,"\n",source,":",table))

  chem.check.halt = FALSE

  #####################################################################
  cat("Generating source table in database\n")
  res = create_source_table_SQL(source=table, res=res,
                                src_version = res$source_version_date[1], db=db)
  #####################################################################

  #####################################################################
  cat("Setting default columns \n")
  #####################################################################
  res = as.data.frame(res)
  res$source = source
  # res$clowder_id = "-"
  res$parent_chemical_id = "-"
  # if(!generics::is.element(source,c("HESS"))) res$document_name = "-"
  res$qc_status = "not determined"

  # #####################################################################
  # cat("Set the clowder_id and document name\n")
  # #####################################################################
  # Removed due to document lineage schema change
  # res = set_clowder_id(res=res,source=source)

  cat("General fixes to non-ascii and encoding \n")
  # Handle character fixes
  res = fix.non_ascii.v2(res,source)

  #
  # make sure all characters are in UTF8 - moved from runInsertTable.R
  # so it is applied BEFORE hashing and loading
  #
  desc <- runQuery(paste0("desc ",table),db)
  desc <- desc[generics::is.element(desc[,"Field"],names(res)),]
  for(i in 1:dim(desc)[1]) {
    col <- desc[i,"Field"]
    type <- desc[i,"Type"]
    if(grepl("varchar|text", type)) {
      # if(verbose) cat("   enc2utf8:",col,"\n")
      x <- as.character(res[,col])
      x[is.na(x)] <- "-"
      x <- enc2native(x)
      x <- iconv(x,from="latin1",to="UTF-8")
      x <- iconv(x,from="LATIN1",to="UTF-8")
      x <- iconv(x,from="LATIN2",to="UTF-8")
      x <- iconv(x,from="latin-9",to="UTF-8")
      res[,col] <- enc2utf8(x)
    }
  }

  #####################################################################
  cat("Do the chemical checking\n")
  #####################################################################
  res = source_chemical.process(db,res,source,table,chem.check.halt,casrn.col="casrn",name.col="name")

  #####################################################################
  cat("Set the default values for missing data\n")
  #####################################################################
  res = source_set_defaults(res,source)

  #####################################################################
  cat("Build the hash key and load the data \n")
  #####################################################################
  toxval_source.hash.and.load(db=db, source=source,table=table,
                              do.reset=do.reset, do.insert=do.insert,
                              res=res)
}
