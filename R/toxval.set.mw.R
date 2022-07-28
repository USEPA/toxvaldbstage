#--------------------------------------------------------------------------------------
#' Set the molecular weight in the toxval table, for use in unit conversions
#' @param toxval.db The database version to use
#' @param source The source
#--------------------------------------------------------------------------------------
toxval.set.mw <- function(toxval.db, source){
  printCurrentFunction(toxval.db)
  dsstox.db <- toxval.config()$dsstox.db
  dlist = runQuery(paste0("select distinct dtxsid from toxval where source='",source,"'"),toxval.db)[,1]
  for(dtxsid in dlist) {
    query = paste0(
      "select ",dsstox.db,".compounds.mol_weight
      from ",
      dsstox.db,".compounds, ",
      dsstox.db,".generic_substance_compounds, ",
      dsstox.db,".generic_substances
      where ",
      dsstox.db,".compounds.id = ",dsstox.db,".generic_substance_compounds.fk_compound_id
      and ",
      dsstox.db,".generic_substance_compounds.fk_generic_substance_id = ",dsstox.db,".generic_substances.id
      and ",dsstox.db,".generic_substances.dsstox_substance_id='",dtxsid,"'"
    )
    mw = runQuery(query,toxval.db)[1,1]
    if(!is.na(mw)) {
      query = paste0("update toxval set mw=",mw," where dtxsid='",dtxsid,"' and source='",source,"'")
      if(source!='ECHA eChemPortal') runQuery(query,toxval.db)
    }
  }
}
