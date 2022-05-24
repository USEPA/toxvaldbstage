
#-------------------------------------------------------------------------------------
#' Load new_efsa from toxval_source to toxval
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param source.db The source database to use.
#' @param verbose Whether the loaded rows should be printed to the console.
#' @export
#--------------------------------------------------------------------------------------
toxval.load.efsa <- function(toxval.db,source.db, verbose=T) {
  printCurrentFunction(toxval.db)
  
  #####################################################################
  cat("start output log, log files for each source can be accessed from output_log folder\n")
  #####################################################################
  source <- "EFSA"
  
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
  query <- "select  source_hash, casrn,name, source,subsource,source_url,record_url, toxval_type, toxval_numeric, toxval_numeric_qualifier,
  toxval_units, critical_effect, population, exposure_route, exposure_method, study_type, study_duration_value, 
  study_duration_units, species_original, human_eco, year, long_ref, title, record_source_type, document_name  from new_efsa" 
  res <- runQuery(query,source.db,T,F) 
  #####################################################################
  cat("checks, finds and replaces non ascii characters in res with XXX\n")
  #####################################################################
  res <- fix.non_ascii(res)
  
  # trims leading and trailing whitespaces from the dataframe
  res <- data.frame(lapply(res, function(x) if(class(x)=="character") trimws(x) else(x)), stringsAsFactors=F, check.names=F)
  #print(str(res))
  
  print(dim(res))
  res <- res[!is.na(res[,"toxval_numeric"]),]
  #print(dim(res))
  res <- res[!is.na(res[,"casrn"]),]
  #print(dim(res))
  res <- unique(res)
  print(dim(res))

  ref.list <- unique(res$long_ref)
  counter <- 1000000
  for(i in 1:nrow(res)) {
    lr <- res[i,"long_ref"]
    if(!is.na(lr)) {
      index <- which.max(ref.list==lr)
      if(is.na(index)) index <- -1
      res[i,"source_study_id"] <- index
    }
    else {
      counter <- counter+1
      res[i,"source_study_id"] <- counter
    }
  }
  res <- unique(res)
  print(dim(res))

  #####################################################################
  cat("add other columns to res\n")
  #####################################################################
  res$source <- source
  #res$subsource <- "EFSA OpenFoodTox"
  res$details_text <- "EFSA details"
  res$species_original <- tolower(res$species_original)
  names(res)[is.element(names(res),"record_url")] <- "url"
  x <- res$study_duration_value
  x[x=="-"] <- NA
  x <- as.numeric(x)
  res$study_duration_value <- x
  res <- unique(res)
  res <- generate.originals(toxval.db,res)
  res <- fill.toxval.defaults(toxval.db,res)


  #####################################################################
  cat("Fix the multiple name per CASRN in EFSA\n")
  #####################################################################
  res <- unique(res)
  print(dim(res))
  temp <- unique(res[,c("casrn","name"),])
  temp <- temp[order(temp$casrn),]
  #print(View(temp))
  cat(" number of unique casrn,name before process:",nrow(temp),"\n")
  lastcasrn <- "-"
  for(i in 1:nrow(temp)) {
    casrn <- temp[i,"casrn"]
    if(casrn==lastcasrn) {
      temp[i,"name"] <- temp[i-1,"name"]
    }
    
    lastcasrn <- casrn
  }
  
  temp <- unique(temp)
  
  cat(" number of unique casrn,name after process:",nrow(temp),"\n")
  #temp2 <- temp
  #print(View(temp2))
  rownames(temp) <- temp$casrn
  for(i in 1:nrow(res)) {
    casrn <- res[i,"casrn"]
    name <- temp[casrn,"name"]
    res[i,"name"] <- name
  }
  
  res$toxval_numeric_original <- res$toxval_numeric
  res[is.na(res[,"document_name"]),"document_name"] <- "-"
  res <- unique(res)
  
  print(dim(res))

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
  casrn.list$casrn <- stri_encode(casrn.list$casrn, "", "UTF-8")
  cid.list <- get.cid.list.toxval(toxval.db, casrn.list,source)
  res$chemical_id <- cid.list$chemical_id
  #res <- merge(res,cid.list)
  res <- res[,!is.element(names(res),c("casrn","name"))]


  #####################################################################
  cat("pull out record source to refs\n")
  #####################################################################
  refs <- res[,c("toxval_id","source","long_ref","title","source_study_id","url","year","record_source_type","document_name")]

  #####################################################################
  cat("add extra columns to refs\n")
  #####################################################################
  #refs$record_source_type <- "government document"
  #refs$record_source_note <- "documents to be added later"
  refs$record_source_note <- "-"
  refs$record_source_level <- "primary (risk assessment values)"

  #####################################################################
  cat("delete unused columns from res\n")
  #####################################################################
  res <- res[,!is.element(names(res),c("long_ref","title","author",",journal","source_study_id","url","record_source_type","document_name"))]
  res <- unique(res)

  #####################################################################
  cat("load res and refs to the database\n")
  #####################################################################
  res <- unique(res)
  refs <- unique(refs)
  res$datestamp <- Sys.Date()
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
