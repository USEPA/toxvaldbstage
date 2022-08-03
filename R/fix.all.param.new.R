#-------------------------------------------------------------------------------------
#' Alter the contents of toxval according to an excel dictionary file with fields -
#' exposure_method, exposure_route, sex,strain, study_duration_class, study_duration_units, study_type,
#' toxval_type, exposure_form, media, toxval_subtype, generation
#' @param toxval.db The version of toxval in which the data is altered.
#' @return The database will be altered
#' @export
#-------------------------------------------------------------------------------------
fix.all.param.new <- function(toxval.db) {
  printCurrentFunction()
  
  filenames <- list.files(path = paste0(toxval.config()$datapath,"dictionary/2021_dictionaries/"), pattern="_5.xlsx", full.names = T)
  full_dict <- lapply(filenames, function(x) read.xlsx(x ,colNames = T))
  for (i in 1:length(full_dict)){
    names(full_dict)[i] <- gsub("(.*dictionary/2021_dictionaries/)(.*)(_5.*)","\\2",filenames)[i]
    
  }
  field <- names(full_dict)
  full_dict <- mapply(cbind, full_dict, "field"=field, SIMPLIFY=F)
  colnames <- c("term_final","term_original","field")
  #print(View(full_dict))
  full_dict <- lapply(full_dict, setNames, colnames)
  full_dict <- do.call(rbind,full_dict)
  rownames(full_dict) <- NULL
  full_dict <- lapply(full_dict, function(x) type.convert(as.character(x), as.is = T))
  full_dict <- data.frame(full_dict, stringsAsFactors = F)
  full_dict["toxval_fix_id"] <- c(1:length(full_dict[,1]))
  full_dict <- full_dict[c("toxval_fix_id",names(full_dict[-4]))]
  full_dict$field <- gsub("^[^[:alnum:]]","",full_dict$field)
  runInsertTable(full_dict, "toxval_fix", toxval.db, verbose)
  #print(View(full_dict))
  runQuery(paste0("update toxval SET exposure_method"," = ", "REPLACE", "( exposure_method",  ",\'\"\',", " \"'\" ) WHERE exposure_method"," LIKE \'%\"%\'" ),toxval.db)
  runQuery(paste0("update toxval SET strain"," = ", "REPLACE", "( strain",  ",\'\"\',", " \"'\" ) WHERE strain"," LIKE \'%\"%\'" ),toxval.db)
  runQuery(paste0("update toxval SET exposure_route"," = ", "REPLACE", "( exposure_route",  ",\'\"\',", " \"'\" ) WHERE exposure_route"," LIKE \'%\"%\'" ),toxval.db)
  runQuery(paste0("update toxval SET media"," = ", "REPLACE", "( media",  ",\'\"\',", " \"'\" ) WHERE media"," LIKE \'%\"%\'" ),toxval.db)
  runQuery(paste0("update toxval SET study_type"," = ", "REPLACE", "( study_type",  ",\'\"\',", " \"'\" ) WHERE study_type"," LIKE \'%\"%\'" ),toxval.db)
  runQuery(paste0("update toxval SET toxval_type"," = ", "REPLACE", "( toxval_type",  ",\'\"\',", " \"'\" ) WHERE toxval_type"," LIKE \'%\"%\'" ),toxval.db)
  
  for(i in 1:nrow(full_dict)) {
    original <- full_dict[i,3]
    final <- full_dict[i,2]
    field <- full_dict[i,4]
    query <- paste0("update toxval set ",field,"=\"",final,"\" where ",field,"=\"",original,"\" and source not like 'ECOTOX'")
    runInsert(query,toxval.db,T,F,T)
  }
  query <- paste0("update toxval set ",field,"='-' where ",field,"_original is NULL")
  runInsert(query,toxval.db,T,F,T)
}  

