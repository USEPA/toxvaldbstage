#--------------------------------------------------------------------------------------
#' @description Prep the source data aand load
#' @param db The version of toxval_source into which the source is loaded.
#' @param source Name of the source
#' @param table Name of the database table
#' @param res The data frame to be processed
#' @param do.reset If TRUE, delete data from the database for this source before #' inserting new data. Default FALSE
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @param chem.check.halt If TRUE, stop the execution if there are errors in the #' chemical mapping
#' @param verbose If TRUE, write out diagnostic messages #'
#' @param hashing_cols Optional list of columns to use for generating source_hash
#' @title source_prep_and_load
#' @return None
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
#' @importFrom generics is.element
#' @importFrom dplyr select distinct left_join
#--------------------------------------------------------------------------------------
source_prep_and_load <- function(db,source,table,res,
                                 do.reset=FALSE, do.insert=FALSE,
                                 chem.check.halt=FALSE, verbose=FALSE,
                                 hashing_cols = NULL){
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
  # Set default qc_status if not already set
  if(!"qc_status" %in% names(res)){
    res$qc_status = "not determined"
  }

  # #####################################################################
  # cat("Set the clowder_id and document name\n")
  # #####################################################################
  # Removed due to document lineage schema change
  # res = set_clowder_id(res=res,source=source)

  cat("General fixes to non-ascii and encoding \n")
  # Get list of character fields from table
  char_fields = runQuery(paste0("DESC ", table), db) %>%
    dplyr::filter(grepl("varchar|text", Type, ignore.case=TRUE)) %>%
    dplyr::pull(Field)

  # Handle character fixes
  res = fix.non_ascii.v2(res,source) %>%
    dplyr::mutate(
      # make sure all characters are in UTF8 - moved from runInsertTable.R
      # so it is applied BEFORE hashing and loading
      dplyr::across(
        tidyselect::any_of(!!char_fields),
        ~as.character(.) %>%
          tidyr::replace_na("-") %>%
          enc2native() %>%
          iconv(from="latin1", to="UTF-8") %>%
          iconv(from="LATIN1", to="UTF-8") %>%
          iconv(from="LATIN2", to="UTF-8") %>%
          iconv(from="latin-9", to="UTF-8") %>%
          enc2utf8()
      )
    )

  #####################################################################
  cat("Do the chemical checking\n")
  #####################################################################
  # res = source_chemical.process(db,res,source,table,chem.check.halt,casrn.col="casrn",name.col="name")

  # Process and curate the distinct chemical entries (saves processing time and resources)
  chem_map = source_chemical.process(db=db,
                                    res=res %>%
                                      dplyr::select(casrn, name) %>%
                                      dplyr::distinct(),
                                    source=source,
                                    table=table,
                                    chem.check.halt=chem.check.halt,
                                    casrn.col="casrn",name.col="name",
                                    verbose=verbose)

  # Map back chemical information to all records
  res <- res %>%
    dplyr::left_join(chem_map %>%
                dplyr::select(-chemical_index),
              by = c("name", "casrn"))

  # Remove intermediate
  rm(chem_map)

  #####################################################################
  cat("Set the default values for missing data\n")
  #####################################################################
  res = source_set_defaults(res,source)

  #####################################################################
  cat("Build the hash key and load the data \n")
  #####################################################################
  toxval_source.hash.and.load(db=db, source=source,table=table,
                              do.reset=do.reset, do.insert=do.insert,
                              res=res, hashing_cols = hashing_cols)
}
