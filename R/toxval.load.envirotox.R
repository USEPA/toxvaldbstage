
#-------------------------------------------------------------------------------------
#' Load original_envirotox from toxval_source to toxval
#'
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param source.db The source database to use.
#' @param verbose If TRUE, print out extra diagnostic messages
#' @export
#--------------------------------------------------------------------------------------
toxval.load.envirotox <- function(toxval.db, source.db, verbose=F) {
  printCurrentFunction(toxval.db)
  
  
  #####################################################################
  cat("start output log, log files for each source can be accessed from output_log folder\n")
  #####################################################################
  source <- "EnviroTox_v2"

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
  
  query <- "select * from original_envirotox"
  
  res <- runQuery(query,source.db,T,F)
  res <- res[ , !(names(res) %in% c("source_id","clowder_id"))]
  
  names(res)[is.element(names(res),"original_cas")] <- "casrn"
  names(res)[is.element(names(res),"reported_chemical_name")] <- "name"
  names(res)[is.element(names(res),"source")] <- "long_ref"
  names(res)[is.element(names(res),"latin_name")] <- "species_original"
  names(res)[is.element(names(res),"effect")] <- "critical_effect"
  names(res)[is.element(names(res),"effect_value")] <- "toxval_numeric"
  names(res)[is.element(names(res),"unit")] <- "toxval_units"
  names(res)[is.element(names(res),"test_type")] <- "study_type"
  names(res)[is.element(names(res),"test_statistic")] <- "toxval_type"
  
  
  # some of the duration_(days) field have only units represented in them eg:Day(s) post-hatch, 
  # Brood or litter etc in duration_(days). hence extracting their respective values from duration field
  
  res$study_duration_value <- res$`duration_(days)`
  res$study_duration_units <- "day"
  
  
  res[grep("[a-zA-Z]+",res$`duration_(days)`),"study_duration_value"] <- gsub("(^\\d+)(\\s+)(.*)","\\1",res[grep("[a-zA-Z]+",res$`duration_(days)`),"duration"])
  res[grep("[a-zA-Z]+",res$`duration_(days)`),"study_duration_units"] <- gsub("(^\\d+)(\\s+)(.*)","\\3", res[grep("[a-zA-Z]+",res$`duration_(days)`),"duration"])
  
  res$toxval_numeric <- as.numeric(res$toxval_numeric)
  res[which(res$study_duration_value == "-"),"study_duration_value"] <- ""
  
  res$study_duration_value <- as.numeric(res$study_duration_value)
  
  
  names.list <- c("source_hash","casrn","name","species_original","toxval_numeric","toxval_units","toxval_type","study_type","study_duration_value","study_duration_units",
                  "critical_effect","long_ref")
  
  res <- res[,names(res)[names(res) %in% names.list]]
  
  
  #####################################################################
  cat("add other columns to res\n")
  #####################################################################
  res$source <- source
  res$human_eco<-"eco"
  res$source_url <- "https://envirotoxdatabase.org/"
  res$year <- "2019"
  res <- fill.toxval.defaults(toxval.db,res)
  res <- generate.originals(toxval.db,res)
  

  res[grep("A",res$study_type),"study_type"] <- "acute"
  res[grep("C",res$study_type),"study_type"] <- "chronic"
  
  if(is.element("species_original",names(res))) res[,"species_original"] <- tolower(res[,"species_original"])
  
  res <- unique(res)
  
  #####################################################################
  cat("map chemicals\n")
  #####################################################################
  res <- res[!is.na(res[,"casrn"]),]
  name.list <- c("casrn","name",names(res)[!is.element(names(res),c("casrn","name"))])
  res <- res[,name.list]
  casrn.list <- res[,c("casrn","name")]
  cid.list <- get.cid.list.toxval(toxval.db, casrn.list,source)
  res$chemical_id <- cid.list$chemical_id
  res <- res[,!is.element(names(res),c("casrn","name"))]
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
  cat("pull out record source to refs\n")
  #####################################################################
  refs <- res[,c("toxval_id","source","long_ref","year")]

  #####################################################################
  cat("add extra columns to refs\n")
  #####################################################################
  refs$record_source_type <- "journal article"
  refs$record_source_note <- "-"
  refs$record_source_level <- "secondary"
  refs$document_name <- "Connors_2019a.pdf"
  #####################################################################
  cat("delete unused columns from res\n")
  #####################################################################
  res <- res[,!is.element(names(res),c("long_ref"))]

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
