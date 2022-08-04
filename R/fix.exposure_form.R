#-------------------------------------------------------------------------------------
#
#' Exposure Method temporary fix to add Exposure Form
#' @param toxval.db The database version to use
#-------------------------------------------------------------------------------------
fix.exposure_form = function(toxval.db){
  printCurrentFunction(toxval.db)
  conv = read.xlsx(paste0(toxval.config()$datapath,"dictionary/exposure_method_temp_fix.xlsx"))
  for (i in 1: nrow(conv)){
    query = paste0("
                   update toxval
                   set exposure_method = '",conv[i,2],"',
                   exposure_form = '",conv[i,3],"'
                   where exposure_method='",conv[i,1],"' and source not like 'ECOTOX'")
    runInsert(query,toxval.db,T,F,T)
  }
}
