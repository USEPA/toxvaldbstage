#-------------------------------------------------------------------------------------
#' Alter the contents of toxval according to an excel dictionary file with fields -
#' exposure_method, exposure_route, sex,strain, study_duration_class, study_duration_units, study_type,
#' toxval_type, exposure_form, media, toxval_subtype
#' @param toxval.db The version of toxval in which the data is altered.
#' @return The database will be altered
#' @export
#-------------------------------------------------------------------------------------
fix.all.param.by.source <- function(toxval.db, source, fill.toxval_fix=F) {
  printCurrentFunction(paste(toxval.db,":", source))

  n = runQuery("select count(*) from toxval_fix",toxval.db)[1,1]
  if(n==0) fill.toxval_fix = T
  if(fill.toxval_fix) {
    runInsert("delete from toxval_fix",toxval.db)

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
  }
  else {
    full_dict = runQuery("select * from toxval_fix",toxval.db)
  }
  #print(View(full_dict))
  cat("   exposure_method\n")
  runQuery(paste0("update toxval SET exposure_method"," = ", "REPLACE", "( exposure_method",  ",\'\"\',", " \"'\" ) WHERE exposure_method"," LIKE \'%\"%\' and source = '",source,"'"),toxval.db)
  cat("   strain\n")
  runQuery(paste0("update toxval SET strain"," = ", "REPLACE", "( strain",  ",\'\"\',", " \"'\" ) WHERE strain"," LIKE \'%\"%\' and source = '",source,"'"),toxval.db)
  cat("   exposure_route\n")
  runQuery(paste0("update toxval SET exposure_route"," = ", "REPLACE", "( exposure_route",  ",\'\"\',", " \"'\" ) WHERE exposure_route"," LIKE \'%\"%\' and source = '",source,"'"),toxval.db)
  cat("   media\n")
  runQuery(paste0("update toxval SET media"," = ", "REPLACE", "( media",  ",\'\"\',", " \"'\" ) WHERE media"," LIKE \'%\"%\' and source = '",source,"'"),toxval.db)
  cat("   study_type\n")
  runQuery(paste0("update toxval SET study_type"," = ", "REPLACE", "( study_type",  ",\'\"\',", " \"'\" ) WHERE study_type"," LIKE \'%\"%\' and source = '",source,"'"),toxval.db)
  cat("   toxval_type\n")
  runQuery(paste0("update toxval SET toxval_type"," = ", "REPLACE", "( toxval_type",  ",\'\"\',", " \"'\" ) WHERE toxval_type"," LIKE \'%\"%\' and source = '",source,"'"),toxval.db)

  cat("   iterate through the full_dict\n")
  flist = unique(full_dict$field)
  for(field in flist) {
    cat("   ",field,"\n")
    sdict = full_dict[full_dict$field==field,]
    query = paste0("select distinct ",field," from toxval where source='",source,"'")
    terms = runQuery(query,toxval.db)[,1]
    if(length(terms)>0) {
      sdict = sdict[is.element(sdict$term_original,terms),]
      if(nrow(sdict)>0) {
        for(i in 1:nrow(sdict)) {
          original = sdict[i,"term_original"]
          final = sdict[i,"term_final"]
          query <- paste0("update toxval set ",field,"=\"",final,"\" where ",field,"_original=\"",original,"\" and source = '",source,"'")
          runInsert(query,toxval.db,T,F,T)
        }
      }
    }
  }

  query <- paste0("update toxval set ",field,"='-' where ",field,"_original is NULL and source = '",source,"'")
  runInsert(query,toxval.db,T,F,T)
}
