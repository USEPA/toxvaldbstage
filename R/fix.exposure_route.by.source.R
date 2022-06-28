#-------------------------------------------------------------------------------------
#' Alter the exposure route and study_duration_class of toxval based on toxval_type
#' @param toxval.db The version of toxval in which the data is altered.
#' @return The database will be altered
#' @export
#-------------------------------------------------------------------------------------
fix.exposure_route.by.source <- function(toxval.db, source){
  printCurrentFunction(paste(toxval.db,":", source))

  query <- paste0("update toxval
  set exposure_route = 'inhalation'
  where toxval_type in ('RFCi', 'Inhalation Unit Risk', 'IUR', 'Inhalation UR', 'Inhalation TC', 'Inhalation SF') and source = '",source,"'")
  runInsert(query,toxval.db,T,F,T)

  query = paste0("update toxval
  set exposure_route = 'oral'
  where toxval_type in ('RFDo', 'Oral Slope Factor', 'oral TDI', 'oral SF', 'oral ADI', 'LDD50 (Lethal Dietary Dose)') and source = '",source,"'")
  runInsert(query,toxval.db,T,F,T)

  query = paste0("update toxval
  set study_duration_class = 'acute'
  where toxval_type in ('ARFD', 'ARFD (group)', 'AAOEL') and source = '",source,"'")

  query = paste0("update toxval
  set study_duration_class = 'chronic'
  where toxval_type like 'Chronic%' and source = '",source,"'")
  runInsert(query,toxval.db,T,F,T)
}
