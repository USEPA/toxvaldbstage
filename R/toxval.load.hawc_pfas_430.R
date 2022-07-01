#--------------------------------------------------------------------------------------
#' Load HAWC PFAS 430 from toxval_source to toxval
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param source.db The version of toxval_source from which the tables are loaded.
#' @param verbose If TRUE, output extra debug information
#' @export
#--------------------------------------------------------------------------------------
toxval.load.hawc_pfas_430 <- function(toxval.db, source.db,log=F){
  printCurrentFunction(toxval.db)
  source <- "HAWC PFAS 430"
  source_table = "source_hawc_pfas_430"
  verbose=F
  #####################################################################
  cat("start output log, log files for each source can be accessed from output_log folder\n")
  #####################################################################
  if(log) {
    con1 = file.path(toxval.config()$datapath,paste0(source,"_",Sys.Date(),".log"))
    con1 = log_open(con1)
    con = file(paste0(toxval.config()$datapath,source,"_",Sys.Date(),".log"))
    sink(con, append=TRUE)
    sink(con, append=TRUE, type="message")
  }
  #####################################################################
  cat("clean source_info by source\n")
  #####################################################################
  import.source.info.by.source(toxval.db, source)

  #####################################################################
  cat("clean by source\n")
  #####################################################################
  clean.toxval.by.source(toxval.db,source)

  #####################################################################
  cat("load data to res\n")
  #####################################################################
  query = paste0("select * from ",source_table)
  res = runQuery(query,source.db,T,F)
  res = res[ , !(names(res) %in% c("source_id","clowder_id","parent_hash","create_time","modify_time","created_by"))]
  res$source = source
  res$details_text = paste(source,"Details")
  print(dim(res))

  #####################################################################
  cat("Add the code from the original version from Aswani\n")
  #####################################################################
  res$casrn <-  gsub("([a-zA-Z]+\\s+[a-zA-Z]*\\:*\\s*)(.*)","\\2",res$casrn)
  res$exposure_route <- gsub("(^[a-zA-Z]+)(\\s*.*)","\\1", res$route_of_exposure)
  res$exposure_method <- gsub("(^[a-zA-Z]+\\s*)(.*)","\\2", res$route_of_exposure)
  res$exposure_method <- gsub("^\\-\\s+","", res$exposure_method)
  res$study_duration_value <- gsub("(^\\d+)(\\s+.*)","\\1",res$exposure_duration_text)
  res$study_duration_value <- gsub("(^\\d+)(.*)","\\1",res$study_duration_value)
  range_vals <- grep("\\-", res$study_duration_value)
  res[range_vals,"study_duration_value"] <- res[range_vals,"exposure_duration_value_original"]
  res$study_duration_units <- gsub("(^GD)(\\s+.*)","\\1",res$exposure_duration_text)
  res$study_duration_units <- gsub("(^\\d+\\s+)(\\w+)(\\s*.*)","\\2",res$study_duration_units)
  res$study_duration_units <- gsub("(.*)(\\d+\\s+)(\\w+)(\\s*.*)","\\3",res$study_duration_units)
  res$study_duration_units <- gsub("(\\d+\\s*)(\\w+)(\\s*.*)","\\2",res$study_duration_units)
  res[is.element(res$study_duration_units,"d"),"study_duration_units"] <- "day"
  res[is.element(res$study_duration_units,"GD"),"study_duration_units"] <- "day"
  res[is.element(res$study_duration_units,"wk"),"study_duration_units"] <- "week"
  res[is.element(res$study_duration_units,"yr"),"study_duration_units"] <- "year"

  res$study_type <- gsub("(^\\w+\\-*\\w*)(\\s*.*)","\\1",res$study_type_original)
  res$study_duration_value <- as.numeric(res$study_duration_value)

  cremove=c("target","noel_original","loel_original",
            "fel_original","endpoint_url_original","bmd",
            "study_id","authors_short","full_text_url",
            "study_url_original","experiment_name","chemical_source",
            "guideline_compliance","dosing_regime_id","route_of_exposure",
            "exposure_duration_value_original","exposure_duration_text","doses")
  res = res[ , !(names(res) %in% cremove)]

  #####################################################################
  cat("find columns in res that do not map to toxval or record_source\n")
  #####################################################################
  cols1 = runQuery("desc record_source",toxval.db)[,1]
  cols2 = runQuery("desc toxval",toxval.db)[,1]
  cols = unique(c(cols1,cols2))
  colnames(res)[which(names(res) == "species")] = "species_original"
  res = res[ , !(names(res) %in% c("record_url","short_ref"))]
  nlist = names(res)
  nlist = nlist[!is.element(nlist,c("casrn","name"))]
  nlist = nlist[!is.element(nlist,cols)]
  if(length(nlist)>0) {
    cat("columns to be dealt with\n")
    print(nlist)
    browser()
  }
  print(dim(res))

  # examples ...
  # names(res)[names(res) == "source_url"] = "url"
  # colnames(res)[which(names(res) == "phenotype")] = "critical_effect"

  #####################################################################
  cat("Generic steps \n")
  #####################################################################
  res = unique(res)
  res = fill.toxval.defaults(toxval.db,res)
  res = generate.originals(toxval.db,res)
  if(is.element("species_original",names(res))) res[,"species_original"] = tolower(res[,"species_original"])
  res$toxval_numeric = as.numeric(res$toxval_numeric)
  print(dim(res))
  res=fix.non_ascii.v2(res,source)
  res = data.frame(lapply(res, function(x) if(class(x)=="character") trimws(x) else(x)), stringsAsFactors=F, check.names=F)
  res = unique(res)
  res = res[,!is.element(names(res),c("casrn","name"))]
  print(dim(res))

  #####################################################################
  cat("add toxval_id to res\n")
  #####################################################################
  count = runQuery("select count(*) from toxval",toxval.db)[1,1]
  if(count==0) tid0 = 1
  else tid0 = runQuery("select max(toxval_id) from toxval",toxval.db)[1,1] + 1
  tids = seq(from=tid0,to=tid0+nrow(res)-1)
  res$toxval_id = tids
  print(dim(res))

  #####################################################################
  cat("pull out record source to refs\n")
  #####################################################################
  cols = runQuery("desc record_source",toxval.db)[,1]
  nlist = names(res)
  keep = nlist[is.element(nlist,cols)]
  refs = res[,keep]
  cols = runQuery("desc toxval",toxval.db)[,1]
  nlist = names(res)
  remove = nlist[!is.element(nlist,cols)]
  res = res[ , !(names(res) %in% c(remove))]
  print(dim(res))

  #####################################################################
  cat("add extra columns to refs\n")
  #####################################################################
  refs$record_source_type = ""
  refs$record_source_note = ""
  refs$record_source_level = ""
  print(dim(res))

  #####################################################################
  cat("load res and refs to the database\n")
  #####################################################################
  res = unique(res)
  refs = unique(refs)
  res$datestamp = Sys.Date()
  res$source_table = source_table
  res$source_url = "-"
  res$subsource_url = "-"
  res$details_text = paste(source,"Details")
  #for(i in 1:nrow(res)) res[i,"toxval_uuid"] = UUIDgenerate()
  #for(i in 1:nrow(refs)) refs[i,"record_source_uuid"] = UUIDgenerate()
  runInsertTable(res, "toxval", toxval.db, verbose)
  runInsertTable(refs, "record_source", toxval.db, verbose)
  print(dim(res))

  #####################################################################
  cat("do the post processing\n")
  #####################################################################
  toxval.load.postprocess(toxval.db,source.db,source)

  if(log) {
    #####################################################################
    cat("stop output log \n")
    #####################################################################
    closeAllConnections()
    log_close()
    output_message = read.delim(paste0(toxval.config()$datapath,source,"_",Sys.Date(),".log"), stringsAsFactors = F, header = F)
    names(output_message) = "message"
    output_log = read.delim(paste0(toxval.config()$datapath,"log/",source,"_",Sys.Date(),".log"), stringsAsFactors = F, header = F)
    names(output_log) = "log"
    new_log = log_message(output_log, output_message[,1])
    writeLines(new_log, paste0(toxval.config()$datapath,"output_log/",source,"_",Sys.Date(),".txt"))
  }
  #####################################################################
  cat("finish\n")
  #####################################################################
  return(0)

  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #####################################################################
  cat("start output log, log files for each source can be accessed from output_log folder\n")
  #####################################################################
  source <- "HAWC PFAS 430"

  con1 <- file.path(toxval.config()$datapath,paste0(source,"_",Sys.Date(),".log"))
  con1 <- log_open(con1)

  con <- file(paste0(toxval.config()$datapath,source,"_",Sys.Date(),".log"))
  sink(con, append=TRUE)
  sink(con, append=TRUE, type="message")

  #####################################################################
  cat("clean source_info by source\n")
  #####################################################################
  import.source.info.by.source(toxval.db, source)
  #####################################################################
  cat("clean by source\n")
  #####################################################################
  clean.toxval.by.source(toxval.db,source)

  #####################################################################
  cat("load data to res\n")
  #####################################################################

  query <- "select * from original_hawc_pfas_430"

  res <- runQuery(query,source.db,T,F)
  res <- res[ , !(names(res) %in% c("source_id","clowder_id"))]
  print(dim(res))
  res <- generate.originals(toxval.db,res)

  res$casrn <-  gsub("([a-zA-Z]+\\s+[a-zA-Z]*\\:*\\s*)(.*)","\\2",res$casrn)

  res$exposure_route <- gsub("(^[a-zA-Z]+)(\\s*.*)","\\1", res$route_of_exposure)
  res$exposure_method <- gsub("(^[a-zA-Z]+\\s*)(.*)","\\2", res$route_of_exposure)

  res$exposure_method <- gsub("^\\-\\s+","", res$exposure_method)

  res$study_duration_value <- gsub("(^\\d+)(\\s+.*)","\\1",res$exposure_duration_text)
  res$study_duration_value <- gsub("(^\\d+)(.*)","\\1",res$study_duration_value)
  range_vals <- grep("\\-", res$study_duration_value)

  res[range_vals,"study_duration_value"] <- res[range_vals,"exposure_duration_value_original"]

  res$study_duration_units <- gsub("(^GD)(\\s+.*)","\\1",res$exposure_duration_text)
  res$study_duration_units <- gsub("(^\\d+\\s+)(\\w+)(\\s*.*)","\\2",res$study_duration_units)
  res$study_duration_units <- gsub("(.*)(\\d+\\s+)(\\w+)(\\s*.*)","\\3",res$study_duration_units)
  res$study_duration_units <- gsub("(\\d+\\s*)(\\w+)(\\s*.*)","\\2",res$study_duration_units)

  res[is.element(res$study_duration_units,"d"),"study_duration_units"] <- "day"
  res[is.element(res$study_duration_units,"GD"),"study_duration_units"] <- "day"
  res[is.element(res$study_duration_units,"wk"),"study_duration_units"] <- "week"
  res[is.element(res$study_duration_units,"yr"),"study_duration_units"] <- "year"

  res$study_type <- gsub("(^\\w+\\-*\\w*)(\\s*.*)","\\1",res$study_type_original)

  res$study_duration_value <- as.numeric(res$study_duration_value)



  # assign appropriate data types
  res <- lapply(res, function(x) type.convert(as.character(x), as.is = T))
  res <- data.frame(res, stringsAsFactors = F)




  # convert na and empty values in character columns into hyphens
  for (i in 1:ncol(res)) {
    if (class(res[,i]) == "character") {
      res[which(is.na(res[i])), names(res)[i]] <- "-"
    }
  }

  res[,which(sapply(res, function(x)all(is.na(x)))) ] <- as.character(res[,which(sapply(res, function(x)all(is.na(x)))) ])

  res$bmd <- NA
  res$bmd <- as.character(res$bmd)
  res$full_text_url <- "-"


  #print(names(res))

  names.list <- c("source_hash","critical_effect", "title", "author", "year", "journal", "full_text_url",
                  "long_ref", "name", "casrn", "media","guideline_compliance", "species", "strain", "sex", "population", "generation",
                  "toxval_type", "toxval_numeric", "toxval_units","record_url" ,"source_url", "exposure_route",
                  "exposure_method","study_duration_value", "study_duration_units", "study_type", "source", "subsource",
                  "critical_effect_original","year_original","media_original","strain_original","sex_original","toxval_type_original","toxval_numeric_original","toxval_units_original",
                  "exposure_route_original","exposure_method_original","study_duration_value_original","study_duration_units_original","study_type_original","document_name")

  res <- res[,(names(res)%in% names.list)]
  #####################################################################
  cat("add other columns to res\n")
  #####################################################################
  names(res)[is.element(names(res),"record_url")] <- "url"
  names(res)[is.element(names(res),"full_text_url")] <- "subsource_url"
  names(res)[is.element(names(res),"species")] <- "species_original"
  names(res)[is.element(names(res),"guideline_compliance")] <- "guideline"

  res$human_eco <- "human health"
  res$details_text <- "HAWC PFAS 430 Details"
  res$subsource <- "EPA HAWC projects"
  if(is.element("species_original",names(res))) res[,"species_original"] <- tolower(res[,"species_original"])
  res <- fill.toxval.defaults(toxval.db,res)
  #res <- generate.originals(toxval.db,res)
  print(dim(res))
  res <- unique(res)
  print(dim(res))
  #res$source_source_id <- 0
  res <- unique(res)
  print(dim(res))
  res$toxval_numeric <- as.numeric(res$toxval_numeric)
  res$toxval_numeric_original <- res$toxval_numeric
  res[is.na(res[,"document_name"]),"document_name"] <- "-"
  res <- unique(res)
  #####################################################################
  cat("checks, finds and replaces non ascii characters in res with XXX\n")
  #####################################################################
  res <- fix.non_ascii(res)

  # trims leading and trailing whitespaces from the dataframe
  res <- data.frame(lapply(res, function(x) if(class(x)=="character") trimws(x) else(x)), stringsAsFactors=F, check.names=F)
  #print(str(res))

  #####################################################################
  cat("add toxval_id to res\n")
  #####################################################################
  count <- runQuery("select count(*) from toxval",toxval.db)[1,1]
  if(count==0) tid0 <- 1
  else tid0 <- runQuery("select max(toxval_id) from toxval",toxval.db)[1,1] + 1
  tids <- seq(from=tid0,to=tid0+nrow(res)-1)
  res$toxval_id <- tids

  #####################################################################
  cat("map chemicals\n")
  #####################################################################
  res <- res[!is.na(res[,"casrn"]),]
  name.list <- c("casrn","name",names(res)[!is.element(names(res),c("casrn","name"))])
  res <- res[,name.list]
  casrn.list <- res[,c("casrn","name")]
  cid.list <- get.cid.list.toxval(toxval.db, casrn.list,source)
  res$chemical_id <- cid.list$chemical_id
  #res <- merge(res,cid.list)
  res <- res[,!is.element(names(res),c("casrn","name"))]
  print(dim(res))

  #####################################################################
  cat("pull out record source to refs\n")
  #####################################################################

  refs <- res[,c("toxval_id","source","title","author","long_ref","journal","url","year","guideline","document_name")]
  #names(refs)[is.element(names(refs),"source_source_id")] <- "source_study_id"

  #####################################################################
  cat("add extra columns to refs\n")
  #####################################################################
  refs$record_source_type <- "journal article"
  refs$record_source_note <- "Data extracted by PFAS 430 (2020)"
  refs$record_source_level <- "primary"

  #####################################################################
  cat("delete unused columns from res\n")
  #####################################################################
  res <- res[,!is.element(names(res),c("title","author","long_ref","journal","url","guideline","document_name"))]

  #####################################################################
  cat("load res and refs to the database\n")
  #####################################################################
  res <- unique(res)
  refs <- unique(refs)
  res$datestamp <- Sys.Date()
  print(dim(res))
  for(i in 1:nrow(res)) res[i,"toxval_uuid"] <- UUIDgenerate()
  for(i in 1:nrow(refs)) refs[i,"record_source_uuid"] <- UUIDgenerate()
  runInsertTable(res, "toxval", toxval.db, verbose)
  runInsertTable(refs, "record_source", toxval.db, verbose)

  #####################################################################
  cat("load chemical info to chemical_list\n")
  #####################################################################
  toxval.load.chemical.list.by.source(toxval.db, source)


  #####################################################################
  cat("map chemicals to dsstox\n")
  #####################################################################
  map.chemical.to.dsstox.by.source(toxval.db, source)
  table.list <- c("toxval","cancer_summary","genetox_summary","genetox_details","skin_eye","chemical_list","bcfbaf")
  for(table in table.list) set.dtxsid.by.source(toxval.db,table,source)

  # #####################################################################
  # cat("fix species by source\n")
  # #####################################################################
  # fix.species.by.source(toxval.db, source)

  #####################################################################
  cat("fix species by source\n")
  #####################################################################
  fix.species.ecotox.by.source(toxval.db, source)


  #####################################################################
  cat("fix human_eco by source\n")
  #####################################################################
  fix.human_eco.by.source(toxval.db, source, reset = T)

  #####################################################################
  cat("fix toxval_numeric_qualifier by source\n")
  #####################################################################
  fix.toxval_numeric_qualifier.by.source(toxval.db, source)

  #####################################################################
  cat("fix exposure_route by type and source\n")
  #####################################################################
  fix.exposure_route.by.type.new.by.source(toxval.db, source)

  #####################################################################
  cat("fix exposure_form by source\n")
  #####################################################################
  fix.exposure_form.by.source(toxval.db, source)

  #####################################################################
  cat("fix priority_id by source\n")
  #####################################################################
  fix.priority_id.by.source(toxval.db, source)

  #####################################################################
  cat("fix all.parameters(exposure_method, exposure_route, sex,strain,
      study_duration_class, study_duration_units, study_type,toxval_type,
      exposure_form, media, toxval_subtype) by source\n")
  #####################################################################
  fix.all.param.new.by.source(toxval.db, source)

  #####################################################################
  cat("fix generation by source\n")
  #####################################################################
  fix.generation.by.source(toxval.db, source)

  #####################################################################
  cat("fix critical_effect by source\n")
  #####################################################################
  fix.critical_effect.icf.by.source(toxval.db, source)

  #####################################################################
  cat("fix units by source\n")
  #####################################################################
  fix.units.new.by.source(toxval.db, source)

  #####################################################################
  cat("fix risk assessment class by source\n")
  #####################################################################

  fix.risk_assessment_class.by.source(toxval.db, source)

  #####################################################################
  cat("fill chemical by source\n")
  #####################################################################
  fill.chemical.by.source(toxval.db, source)

  #####################################################################
  cat("export missing rac by source\n")
  #####################################################################
  export.missing.rac.by.source(toxval.db, source)

  #####################################################################
  cat("fix empty cells to hyphen by source\n")
  #####################################################################
  fix.empty.by.source(toxval.db, source)

  #####################################################################
  cat("fix empty cells in record source to hyphen by source\n")
  #####################################################################
  fix.empty.record_source.by.source(toxval.db, source)


  #####################################################################
  cat("set toxval defaults globally by source\n")
  #####################################################################
  fill.toxval.defaults.global.by.source(toxval.db, source)

  #####################################################################
  cat("fix qa status by source\n")
  #####################################################################
  fix.qa_status.by.source(toxval.db, source)


  #####################################################################
  cat("fix hyphen cells to 'Not Specified' by source\n")
  #####################################################################
  fix.hyphen.by.source(toxval.db, source)

  #####################################################################
  cat("set hash toxval by source\n")
  #####################################################################
  set.hash.toxval.by.source(toxval.db, source)

  #####################################################################
  cat("set hash record_source by source\n")
  #####################################################################
  set.hash.record_source.by.source(toxval.db, source)

  #####################################################################
  cat("map hash record_source by source\n")
  #####################################################################
  map.hash.record_source.by.source(toxval.db, source )

  #####################################################################
  cat("perform extra steps if any\n")
  #####################################################################
  res1 <- runQuery(paste0("select distinct(species_id) from toxval where source like '",source,"'"),toxval.db)
  cat("Number of species in toxval for the particular source:",nrow(res1),"\n")

  res2 <- runQuery(paste0("select species_id from species where species_id in(select species_id from toxval where source like '",source,"')"),toxval.db)
  cat("Number of species in species for the particular source:",nrow(res2),"\n")


  res3 <- runQuery(paste0("select distinct species_original from toxval where species_id<0 and source like '",source,"'"),toxval.db)
  cat("Number of missing species:",nrow(res3),"\n")

  #####################################################################
  cat("stop output log \n")
  #####################################################################
  closeAllConnections()
  log_close()

  output_message <- read.delim(paste0(toxval.config()$datapath,source,"_",Sys.Date(),".log"), stringsAsFactors = F, header = F)
  names(output_message) <- "message"

  output_log <- read.delim(paste0(toxval.config()$datapath,"log/",source,"_",Sys.Date(),".log"), stringsAsFactors = F, header = F)
  names(output_log) <- "log"

  new_log <- log_message(output_log, output_message[,1])
  writeLines(new_log, paste0(toxval.config()$datapath,"output_log/",source,"_",Sys.Date(),".txt"))


  #####################################################################
  cat("finish\n")
  #####################################################################
}
