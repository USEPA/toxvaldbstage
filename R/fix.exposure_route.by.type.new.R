#-------------------------------------------------------------------------------------
#' Alter the exposure route of toxval according to an excel dictionary
#' @param toxval.db The version of toxval in which the data is altered.
#' @return The database will be altered
#' @export
#-------------------------------------------------------------------------------------
fix.exposure_route.by.type.new = function(toxval.db){
  printCurrentFunction(toxval.db)
  query = "update toxval
  set exposure_route = 'inhalation'
  where toxval_type in ('RFCi', 'Inhalation Unit Risk', 'IUR', 'Inhalation UR', 'Inhalation TC', 'Inhalation SF') and source not like 'ECOTOX'"
  runInsert(query,toxval.db,T,F,T)
  
  query = "update toxval
  set exposure_route = 'oral'
  where toxval_type in ('RFDo', 'Oral Slope Factor', 'oral TDI', 'oral SF', 'oral ADI', 'LDD50 (Lethal Dietary Dose)') and source not like 'ECOTOX'"
  runInsert(query,toxval.db,T,F,T)
  
  query = "update toxval
  set study_duration_class = 'acute'
  where toxval_type in ('ARFD', 'ARFD (group)', 'AAOEL') and source not like 'ECOTOX'"
  
  query = "update toxval
  set study_duration_class = 'chronic'
  where toxval_type like 'Chronic%' and source not like 'ECOTOX'"
  runInsert(query,toxval.db,T,F,T)
}
