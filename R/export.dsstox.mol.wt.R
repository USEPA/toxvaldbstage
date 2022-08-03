#-------------------------------------------------------------------------------------
#'
#' Export mol weight from DSSTox
#-------------------------------------------------------------------------------------
export.dsstox.mol.wt <- function() {
  printCurrentFunction()
  dsstox.db <- toxval.config()$dsstox.db
  DSSTOX_Mol_Weight <- runQuery("select c.mol_weight, ss.external_id  from 
                                source_generic_substance_mappings sgsm 
                                inner join source_substances ss on sgsm.fk_source_substance_id = ss.id 
                                inner join generic_substance_compounds gsc on gsc.fk_generic_substance_id = sgsm.fk_generic_substance_id 
                                inner join compounds c on c.id = gsc.fk_compound_id 
                                where ss.external_id is not null and trim(ss.external_id) <> '' and ss.external_id not like '-';",dsstox.db)
  file <- paste0("./DSSTox/DSSTox_mol_weight",Sys.Date(),".RData")
  save(DSSTOX_Mol_Weight,file=file)
}
