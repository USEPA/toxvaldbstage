#-------------------------------------------------------------------------------------
#' Do all of the fixes to units
#'
#' @param toxval.db The version of toxvaldb to use.
#' @param do.convert If TRUE, so unit conversions, as opposed to just cleaning
#' @export
#-------------------------------------------------------------------------------------
fix.units.testmw <- function(toxval.db="dev_toxval_v9", do.convert=T) {
  printCurrentFunction(toxval.db)
  dsstox.db <- toxval.config()$dsstox.db

  #
  # Run conversions from molar to mg units, using MW
  #
  if(do.convert) {
    cat(">>> Run conversions from molar to mg units, using MW\n")
    convos <- read.xlsx(paste0(toxval.config()$datapath,"dictionary/MW conversions.xlsx"))
    nrows <- dim(convos)[1]
    for (i in 1:nrows){
      query <- paste0("update ",toxval.db,".toxval
                      inner join ",dsstox.db,".generic_substances on ",dsstox.db,".generic_substances.dsstox_substance_id=",toxval.db,".toxval.dtxsid
                      inner join ",dsstox.db,".generic_substance_compounds on ",dsstox.db,".generic_substance_compounds.fk_generic_substance_id = ",dsstox.db,".generic_substances.id
                      inner join ",dsstox.db,".compounds on ",dsstox.db,".compounds.id = ",dsstox.db,".generic_substance_compounds.fk_compound_id
                      set toxval_units = '",convos[i,2],"', toxval_numeric = toxval_numeric*mol_weight
                      where toxval_units = '",convos[i,1],"'")
      browser()
      runInsert(query,toxval.db,T,F,T)
    }
  }
  #
  # Convert ppm to mg/m3 for inhalation studies
  #
  # https://cfpub.epa.gov/ncer_abstracts/index.cfm/fuseaction/display.files/fileID/14285
  if(do.convert) {
    cat("   Convert ppm to mg/m3 for inhalation studies\n")
    query <- paste0("update ",toxval.db,".toxval
                     inner join ",dsstox.db,".generic_substances on ",dsstox.db,".generic_substances.dsstox_substance_id=",toxval.db,".toxval.dtxsid
                     inner join ",dsstox.db,".generic_substance_compounds on ",dsstox.db,".generic_substance_compounds.fk_generic_substance_id = ",dsstox.db,".generic_substances.id
                     inner join ",dsstox.db,".compounds on ",dsstox.db,".compounds.id = ",dsstox.db,".generic_substance_compounds.fk_compound_id
                     set toxval_units = 'mg/m3', toxval_numeric = toxval_numeric*mol_weight*0.0409
                     where toxval_units like 'ppm%' and exposure_route = 'inhalation'
           ")
    browser()
    runInsert(query,toxval.db)
  }
}
