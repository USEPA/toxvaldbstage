
#--------------------------------------------------------------------------------------
#' Load new_copper_table from toxval_source to toxval
#'
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param source.db The source database to use.
#' @param verbose If TRUE, print out extra diagnostic messages
#' @export
#--------------------------------------------------------------------------------------
toxval.load.copper <- function(toxval.db,source.db, verbose=F){
  printCurrentFunction(toxval.db)
  
  #####################################################################
  cat("start output log, log files for each source can be accessed from output_log folder\n")
  #####################################################################
  source <- "Copper Manufacturers"
  
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
  
  query <- "select * from original_copper_table"
  
  res <- runQuery(query,source.db,T,F)
  res <- res[ , !(names(res) %in% c("source_id","clowder_id"))]  
  res <- generate.originals(toxval.db,res)
  
  
  
  res[which(!is.na(res[,"toxval_subtype1"])),"toxval_subtype"] <- res[which(!is.na(res[,"toxval_subtype1"])),"toxval_subtype1"]
  res <- res[ , !(names(res) %in% c("toxval_id","toxval_subtype1","year1"))]
  res[grep("^[^[:alnum:]]",res$toxval_numeric), "toxval_numeric_qualifier"] <- res[grep("^[^[:alnum:]]",res$toxval_numeric), "toxval_numeric"]
  res[grep("^[^[:alnum:]]",res$toxval_numeric), "toxval_numeric_qualifier"] <- gsub("(^[^[:alnum:]])(.*)","\\1",res[grep("^[^[:alnum:]]",res$toxval_numeric), "toxval_numeric_qualifier"])
  res[grep("^[^[:alnum:]]",res$toxval_numeric), "toxval_numeric"] <- gsub("(^[^[:alnum:]])(.*)","\\2",res[grep("^[^[:alnum:]]",res$toxval_numeric), "toxval_numeric"])
  res[grep("\\-",res$toxval_numeric), "toxval_numeric"] <- gsub("(.*)(\\s*\\-\\s*)(.*)","\\1",res[grep("\\-",res$toxval_numeric), "toxval_numeric"])
  
  res[grep("^[a-zA-Z]+",res$study_duration_value), "study_duration_value"] <- gsub("(^[a-zA-Z]+\\s*[a-zA-Z]+\\s*)(.*)","\\2",res[grep("^[a-zA-Z]+",res$study_duration_value), "study_duration_value"])
  res[grep("\\-",res$study_duration_value), "study_duration_value"] <- gsub("(.*)(\\s*\\-\\s*)(.*)","\\3",res[grep("\\-",res$study_duration_value), "study_duration_value"])
  res[which(res$volume == "-"),"volume"] <- ""
  
  
  
  res <- lapply(res, function(x) type.convert(as.character(x), as.is = T))
  res <- data.frame(res, stringsAsFactors = F)
  print(dim(res))
  names.list <- c("source_hash","casrn","name","subsource","toxval_type","toxval_subtype","toxval_numeric_qualifier","toxval_numeric","toxval_units","study_type","study_duration_class",
                  "study_duration_value","study_duration_units","species","human_eco","strain","sex","generation","lifestage",
                  "exposure_route","exposure_method","exposure_form","media","critical_effect","year","long_ref","title","author","journal","volume",
                  "issue","url","toxval_type_original","toxval_subtype_original","toxval_numeric_qualifier_original","toxval_numeric_original","toxval_units_original","study_type_original","study_duration_class_original",
                  "study_duration_value_original","study_duration_units_original","strain_original","sex_original","exposure_route_original","exposure_method_original","exposure_form_original","media_original",
                  "critical_effect_original","year_original","document_name")
  
  res <- res[,(names(res) %in% names.list)]
  
  print(dim(res))
  
  #####################################################################
  cat("checks, finds and replaces non ascii characters in res with XXX\n")
  #####################################################################
  res <- fix.non_ascii(res)
  
  # trims leading and trailing whitespaces from the dataframe
  res <- data.frame(lapply(res, function(x) if(class(x)=="character") trimws(x) else(x)), stringsAsFactors=F, check.names=F)
  #print(str(res))

  res <- res[!is.na(res[,"casrn"]),]

  cat("step1",nrow(res),"\n")
  #####################################################################
  cat("add other columns to res\n")
  #####################################################################
  res$source <- source
  names(res)[names(res) == 'url'] <- 'source_url'
  res <- fill.toxval.defaults(toxval.db,res)
  #res <- generate.originals(toxval.db,res)
  res <- unique(res)
  res$risk_assessment_class <- res$study_type
  res$details_text <- "Copper Manufacturers Details"
  names(res)[is.element(names(res),"species")] <- "species_original"
  if(is.element("species_original",names(res))) res[,"species_original"] <- tolower(res[,"species_original"])
  res$toxval_numeric_original <- res$toxval_numeric
  res[is.na(res[,"document_name"]),"document_name"] <- "-"
  res <- unique(res)
  cat("step2",nrow(res),"\n")

  #####################################################################
  cat("map chemicals\n")
  #####################################################################
  casrn.list <- res[,c("casrn","name")]
  cid.list <- get.cid.list.toxval(toxval.db, casrn.list,source)
  res$chemical_id <- cid.list$chemical_id
  res <- res[,!is.element(names(res),c("casrn","name"))]
  cat("step3",nrow(res),"\n")

  #####################################################################
  cat("add toxval_id to res\n")
  #####################################################################
  count <- runQuery("select count(*) from toxval",toxval.db)[1,1]
  if(count==0) tid0 <- 1
  else tid0 <- runQuery("select max(toxval_id) from toxval",toxval.db)[1,1] + 1
  tids <- seq(from=tid0,to=tid0+nrow(res)-1)
  res$toxval_id <- tids
  #print(names(res))
  #####################################################################
  cat("pull out record source to refs\n")
  #####################################################################
  refs <- res[,c("toxval_id","source","year","source_url","long_ref","title","author","journal","volume","issue","document_name")]
  names(refs)[names(refs) == 'source_url'] <- 'url'

  #####################################################################
  cat("add extra columns to refs\n")
  #####################################################################
  refs$record_source_type <- "url"
  refs$record_source_note <- ""
  refs$record_source_level <- "primary"


  #####################################################################
  cat("delete unused columns from res\n")
  #####################################################################
  res <- res[,!is.element(names(res),c("long_ref","title","author","journal","volume","issue","document_name"))]
  #####################################################################
  cat("load res and refs to the database\n")
  #####################################################################
  res <- unique(res)
  refs <- unique(refs)
  res$datestamp <- Sys.Date()
  cat("step4",nrow(res),"\n")
  for(i in 1:nrow(res)) res[i,"toxval_uuid"] <- UUIDgenerate()
  for(i in 1:nrow(refs)) refs[i,"record_source_uuid"] <- UUIDgenerate()
  runInsertTable(res, "toxval", toxval.db, verbose)
  runInsertTable(refs, "record_source", toxval.db, verbose=T)

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
