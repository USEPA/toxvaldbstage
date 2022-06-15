
#--------------------------------------------------------------------------------------
#' Load atsdr from toxval_source to toxval
#'
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param source.db The source database to use.
#' @param verbose If TRUE, print out extra diagnostic messages
#' @export
#--------------------------------------------------------------------------------------
toxval.load.atsdr <- function(toxval.db,source.db, verbose=F){
  printCurrentFunction(toxval.db)
  
  #####################################################################
  cat("start output log, log files for each source can be accessed from output_log folder\n")
  #####################################################################
  source <- "ATSDR MRLs 2020"
  
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
  
  query <- "select * from original_atsdr_table"
  res <- runQuery(query,source.db,T,F)
  res <- res[ , !(names(res) %in% c("source_id","clowder_id"))]
  print(names(res))
  names.list <- c("source_hash","source_name_sid", "casrn", "name","source_url", "subsource", "source_name_cid", "exposure_route","duration","mrl","uncertainty_factor","critical_effect", "status","date","document_name")
  names(res) <- names.list
  res$study_type <- "-"
  res$toxval_numeric <-  res$mrl
  res$toxval_units <-  res$mrl
  res$study_duration_value <- res$duration
  res$study_duration_units <- res$duration
  res$year <- res$date
  res$toxval_type <- "-"

  # fix casrn
  for(i in 1:nrow(res)) res[i,"casrn"] <- fix.casrn(res[i,"casrn"])


  res <- generate.originals(toxval.db,res)
  print(names(res))

  res$study_type <- res$duration
  res[grep("15 - 364 days\\.|15 - 364 daysermediate",res$study_type),"study_type"] <- "intermediate"
  res[grep("1 - 14 days",res$study_type),"study_type"] <- "acute"
  res[grep("1 year or.*",res$study_type),"study_type"] <- "chronic"

  res[grep("Inh.*",res$exposure_route),"exposure_route"] <- "Inhalation"
  res[grep("Rad.*",res$exposure_route),"exposure_route"] <- "External Radiation MRLs"

  res$toxval_numeric <-  res$mrl
  res$toxval_numeric <-  gsub("(\\d+\\.*\\d*)(\\s*.*)","\\1",res$toxval_numeric)
  res$toxval_numeric <- as.numeric(res$toxval_numeric)
  res$toxval_units <- gsub("(\\d+\\.*\\d*\\s*)(.*)","\\2",res$mrl)

  res$study_duration_value <- res$duration
  res$study_duration_value <- gsub("(\\d+\\s*\\-*\\s*\\d*)(\\s*.*)","\\1",res$study_duration_value)
  res$study_duration_value <- gsub("^\\s+|\\s+$","",res$study_duration_value)

  res$study_duration_units <- res$duration
  res[grep("chronic",res$study_type),"study_duration_units"] <- "year"
  res[grep("acute",res$study_type),"study_duration_units"] <- "day"
  res[grep("intermediate",res$study_type),"study_duration_units"] <- "day"


  res[grep("15 - 364",res$study_duration_value),"study_duration_value"] <- "364"
  res[grep("1 - 14",res$study_duration_value),"study_duration_value"] <- "14"
  res$study_duration_value <-  as.numeric(res$study_duration_value)

  res$year <- res$date
  res$year <-  gsub("(\\d+\\-\\w+\\-)(\\d{4})", "\\2", res$year)

  name.list <- c("source_hash","casrn","name","source_url","subsource","exposure_route","study_type","toxval_numeric","toxval_units","uncertainty_factor","critical_effect","year", "study_duration_value","study_duration_units","exposure_route_original","critical_effect_original",
                 "study_type_original","toxval_numeric_original","toxval_units_original","study_duration_value_original","study_duration_units_original",
                 "year_original","toxval_type_original","document_name")
  res <- res[,name.list]
  #create copies of res
  res1 <- res
  res2 <- res
  # update res2 toxval_numeric field by multiplying values from existing toxval_numeric with uncertainity factors
  for(i in 1:nrow(res2)) res2[i,"toxval_numeric"] <- res2[i,"toxval_numeric"]*res2[i,"uncertainty_factor"]
  #assign new column toxval_type
  res1$toxval_type <- "ATSDR MRL"
  res2$toxval_type <- "NOAEL"
  #combine res1 & res2 to create res and select required fields
  res <- rbind(res1,res2)
  res <- res[,c("source_hash","casrn","name","source_url","exposure_route","study_type","toxval_numeric","toxval_units","toxval_type","critical_effect","year", "subsource", "study_duration_value","study_duration_units","exposure_route_original","critical_effect_original",
                "study_type_original","toxval_numeric_original","toxval_units_original","study_duration_value_original","study_duration_units_original",
                "year_original","toxval_type_original","document_name")]

  res$source_url <- gsub("#", "",res$source_url)

  res[grep("Neurol.",res$critical_effect),"critical_effect"] <- "neurologic"
  res[grep("Hemato.",res$critical_effect),"critical_effect"] <- "hematological"
  res[grep("Resp.",res$critical_effect),"critical_effect"] <- "respiratory"
  res[grep("Gastro.",res$critical_effect),"critical_effect"] <- "gastrointestinal"
  res[grep("Repro.",res$critical_effect),"critical_effect"] <- "reproductive"
  res[grep("Develop.",res$critical_effect),"critical_effect"] <- "developmental"
  res[grep("Metab.",res$critical_effect),"critical_effect"] <- "metabolic"
  res[grep("Body Wt.",res$critical_effect),"critical_effect"] <- "body weight"
  res[grep("Immuno.",res$critical_effect),"critical_effect"] <- "immunological"
  res[grep("Musculo.",res$critical_effect),"critical_effect"] <- "musculoskeletal"
  res[grep("Reprod.",res$critical_effect),"critical_effect"] <- "reproductive"
  res[grep("Endocr.",res$critical_effect),"critical_effect"] <- "endocrine"
  res[grep("Lymphor.",res$critical_effect),"critical_effect"] <- "lymphatic"

  res$toxval_units <- gsub("µ","u", res$toxval_units)
  res$toxval_units <- gsub("\\/day","-day", res$toxval_units)
  res$exposure_route <- tolower(res$exposure_route)
  res[grep("external.*",res$exposure_route),"exposure_route"] <- "radiation"


  # assign appropriate data types
  res <- lapply(res, function(x) type.convert(as.character(x), as.is = T))
  res <- data.frame(res, stringsAsFactors = F)

  # # fix casrn
  # for(i in 1:nrow(res)) res[i,"casrn"] <- fix.casrn(res[i,"casrn"])

  res <- res[,c("source_hash","casrn","name","source_url","exposure_route","study_type","toxval_numeric","toxval_units","toxval_type","critical_effect","year", "study_duration_value","study_duration_units","exposure_route_original","critical_effect_original",
                "study_type_original","toxval_numeric_original","toxval_units_original","study_duration_value_original","study_duration_units_original",
                "year_original","toxval_type_original","document_name")]



  #####################################################################
  cat("multiple casrn for benzene assigned to apprpriate casrn from dashboard\n")
  #####################################################################
  #print(res[grep("^benzene$",res$name, ignore.case = T), "casrn"])
  res[grep("^benzene$",res$name, ignore.case = T, useBytes = T), "casrn"] <- "71-43-2"

  #####################################################################
  cat("checks, finds and replaces non ascii characters in res with XXX\n")
  #####################################################################
  res <- fix.non_ascii(res)


  # trims leading and trailing whitespaces from the dataframe
  res <- data.frame(lapply(res, function(x) if(class(x)=="character") trimws(x) else(x)), stringsAsFactors=F, check.names=F)


  res <- res[!is.na(res[,"casrn"]),]
  cat("step1",nrow(res),"\n")
  #####################################################################
  cat("add other columns to res\n")
  #####################################################################
  res$source <- source
  res <- fill.toxval.defaults(toxval.db,res)
  
  res <- unique(res)
  res$toxval_numeric_qualifier <- "="
  res$risk_assessment_class <- res$study_type
  res$exposure_method <- "-"
  res$human_eco <- "human health"
  res$media <- "-"
  res$sex <- "-"
  res[,"species_original"] <- "human"
  res$subsource <- "CDC"
  res$details_text <- "ATSDR Details"
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
  #res <- merge(res,cid.list)
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

  #####################################################################
  cat("pull out record source to refs\n")
  #####################################################################
  refs <- res[,c("toxval_id","source","year","document_name")]

  #####################################################################
  cat("add extra columns to refs\n")
  #####################################################################
  refs$record_source_type <- "url"
  refs$record_source_note <- "All data is on single web page for ATSDR"
  refs$record_source_level <- "primary (risk assessment values)"
  refs$url <- "https://www.atsdr.cdc.gov/mrls/mrllist.asp"
  

  #####################################################################
  cat("delete unused columns from res\n")
  #####################################################################
  res <- res[,!is.element(names(res),c("document_name"))]
  
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
