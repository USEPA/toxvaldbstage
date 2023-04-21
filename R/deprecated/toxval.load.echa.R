
#-------------------------------------------------------------------------------------
#' Load ECHA from toxval_source to toxval
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param source.db The source database to use.
#' @param verbose Whether the loaded rows should be printed to the console.
#--------------------------------------------------------------------------------------
toxval.load.echa <- function(toxval.db, source.db, verbose=F) {
  printCurrentFunction(toxval.db)

  #####################################################################
  cat("start output log, log files for each source can be accessed from output_log folder\n")
  #####################################################################
  source <- "ECHA"

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

  query <- "select * from echa2"

  res <- runQuery(query,source.db,T,F)
  res <- res[ , !(names(res) %in% c("source_id","clowder_id"))]
  res <- res[!is.element(res[,"toxval_type"],"-"),]
  res[is.na(res[,"document_name"]),"document_name"] <- "-"

  #print(View(res))
  res <- aggregate(source_source_id ~ ., data = res, min)
  #v8 source_source_id 1 here used -1
  res$source_source_id <- -1
  res <- generate.originals(toxval.db,res)
  #####################################################################
  cat("create dictionary to split up toxval_numeric to numeric, qualifier and units\n")
  #####################################################################

  dict_echa_tox_values <- data.frame(unique(res$toxval_numeric), stringsAsFactors = F)
  names(dict_echa_tox_values) <- "toxval_numeric_original"
  dict_echa_tox_values$toxval_numeric <- dict_echa_tox_values$toxval_numeric_original
  dict_echa_tox_values$toxval_numeric <- gsub("^\\s+","",dict_echa_tox_values$toxval_numeric)
  quali_vals <- grep("^\\d+" ,dict_echa_tox_values$toxval_numeric, invert = T)
  dict_echa_tox_values[quali_vals,"toxval_numeric_qualifier"] <- gsub("([^[:alnum:]]?)(\\s+\\d+.*)", "\\1", dict_echa_tox_values[quali_vals, "toxval_numeric"])
  dict_echa_tox_values[which(is.na(dict_echa_tox_values$toxval_numeric_qualifier)), "toxval_numeric_qualifier"] <- "-"
  dict_echa_tox_values$toxval_units <- gsub("(^\\d+\\.*\\d*\\s+)(\\w+.*)","\\2", dict_echa_tox_values$toxval_numeric)
  dict_echa_tox_values[grep("^ca\\.\\s*\\d+\\.*\\d*\\s*.*", dict_echa_tox_values$toxval_units), "toxval_units"] <- gsub("(^ca\\.\\s*\\d+\\.*\\d*\\s*)(\\w+.*)","\\2", dict_echa_tox_values[grep("^ca\\.\\s*\\d+\\.*\\d*\\s*.*", dict_echa_tox_values$toxval_units), "toxval_units"])
  dict_echa_tox_values$toxval_units <- gsub("(^[^[:alnum:]].*\\d+\\.*\\d*\\s+)(\\w+.*)","\\2", dict_echa_tox_values$toxval_units)

  dict_echa_tox_values[grep("(^\\d+\\.\\d+\\.\\d+.*)",dict_echa_tox_values$toxval_units), "toxval_numeric_qualifier"] <- "-"
  dict_echa_tox_values[grep("(^\\d+\\.\\d+\\.\\d+.*)",dict_echa_tox_values$toxval_units), "toxval_numeric"] <- NA
  dict_echa_tox_values[grep("(^\\d+\\.\\d+\\.\\d+.*)",dict_echa_tox_values$toxval_units), "toxval_units"] <- gsub("(^\\d+\\.\\d+\\.\\d+\\s+)(.*)","\\2", dict_echa_tox_values[grep("(^\\d+\\.\\d+\\.\\d+.*)",dict_echa_tox_values$toxval_units), "toxval_units"])
  dict_echa_tox_values$toxval_units <- gsub("(ca\\.\\s*\\d+\\.*\\d*\\s+)(.*)","\\2",dict_echa_tox_values$toxval_units)
  dict_echa_tox_values[grep(".*\\d+\\.*\\d*\\s*mg.*", dict_echa_tox_values$toxval_units), "toxval_units"] <- gsub("(.*\\d+\\.*\\d*\\s*)(mg.*)","\\2",dict_echa_tox_values[grep(".*\\d+\\.*\\d*\\s*mg.*", dict_echa_tox_values$toxval_units), "toxval_units"])
  dict_echa_tox_values$toxval_units <- gsub("(.*\\d+\\s+)(.*)","\\2",dict_echa_tox_values$toxval_units)
  dict_echa_tox_values[grep("^air$", dict_echa_tox_values$toxval_units), "toxval_units"] <- gsub("(.*\\d+\\s+)(mg.*)","\\2",dict_echa_tox_values[grep("^air$", dict_echa_tox_values$toxval_units), "toxval_numeric"])
  dict_echa_tox_values[grep("^air\\s+\\(nominal\\)", dict_echa_tox_values$toxval_units), "toxval_units"] <- gsub("(.*\\d+\\s+)(mg.*)","\\2",dict_echa_tox_values[grep("^air\\s+\\(nominal\\)", dict_echa_tox_values$toxval_units), "toxval_numeric"])
  dict_echa_tox_values[grep("^air\\s+\\(analytical\\)", dict_echa_tox_values$toxval_units), "toxval_units"] <- gsub("(.*\\d+\\s+)(mg.*)","\\2",dict_echa_tox_values[grep("^air\\s+\\(analytical\\)", dict_echa_tox_values$toxval_units), "toxval_numeric"])
  dict_echa_tox_values[grep("^per", dict_echa_tox_values$toxval_units), "toxval_units"] <- gsub("(.*\\d+\\s+)(mg.*)","\\2",dict_echa_tox_values[grep("^per", dict_echa_tox_values$toxval_units), "toxval_numeric"])
  dict_echa_tox_values[grep("\\-", dict_echa_tox_values$toxval_units), "toxval_units"] <- gsub("(.*\\d+\\s+)(.*)","\\2",dict_echa_tox_values[grep("\\-", dict_echa_tox_values$toxval_units), "toxval_numeric"])
  dict_echa_tox_values[which(is.na(dict_echa_tox_values$toxval_units)), "toxval_units"] <- "-"


  dict_echa_tox_values[grep("ca\\.", dict_echa_tox_values$toxval_numeric_qualifier), "toxval_numeric_qualifier"] <- "ca."
  dict_echa_tox_values[grep("\\w+\\s*\\-", dict_echa_tox_values$toxval_numeric_qualifier), "toxval_numeric_qualifier"] <- "-"

  dict_echa_tox_values[grep("^\\d+\\.*\\d*\\s*\\w+.*",dict_echa_tox_values$toxval_numeric), "toxval_numeric"] <- gsub("(^\\d+\\.*\\d*)(\\s*\\w+.*)","\\1",dict_echa_tox_values[grep("^\\d+\\.*\\d*\\s*\\w+.*",dict_echa_tox_values$toxval_numeric), "toxval_numeric"])
  dict_echa_tox_values[grep("^[^[:alnum:]]",dict_echa_tox_values$toxval_numeric), "toxval_numeric"] <- gsub("(^[^[:alnum:]]+\\s+)(\\d+\\.*\\d*)(\\s+.*)","\\2",dict_echa_tox_values[grep("^[^[:alnum:]]",dict_echa_tox_values$toxval_numeric), "toxval_numeric"])
  dict_echa_tox_values[grep("^ca\\.",dict_echa_tox_values$toxval_numeric), "toxval_numeric"] <- gsub("(^ca\\.\\s+)(\\d+\\.*\\d*)(\\s+.*)","\\2",dict_echa_tox_values[grep("^ca\\.",dict_echa_tox_values$toxval_numeric), "toxval_numeric"])
  dict_echa_tox_values[grep("^\\w+\\s+\\w+",dict_echa_tox_values$toxval_numeric), "toxval_numeric"] <- gsub("(.*\\-\\s+[ca\\.]*\\s*)(\\d+\\.*\\d*)(\\s+.*)","\\2",dict_echa_tox_values[grep("^\\w+\\s+\\w+",dict_echa_tox_values$toxval_numeric), "toxval_numeric"])
  dict_echa_tox_values[grep("\\d+\\.\\d+\\.\\d+",dict_echa_tox_values$toxval_numeric), "toxval_numeric"] <- NA
  dict_echa_tox_values[grep("^\\w+\\)+",dict_echa_tox_values$toxval_numeric), "toxval_numeric"] <- gsub("(^\\w+\\)+\\s+\\-\\s+)(\\d+\\.*\\d*)(\\s+.*)","\\2",dict_echa_tox_values[grep("^\\w+\\)+",dict_echa_tox_values$toxval_numeric), "toxval_numeric"])

  dict_echa_tox_values[grep("^0\\s+[^[:alnum:]]",dict_echa_tox_values$toxval_numeric), "toxval_numeric_qualifier"] <- gsub("(^0\\s+)([^[:alnum:]])(\\s+\\d+\\.*\\d*)(\\s+.*)","\\2",dict_echa_tox_values[grep("^0\\s+[^[:alnum:]]",dict_echa_tox_values$toxval_numeric), "toxval_numeric"])
  dict_echa_tox_values[grep("^0\\s+[^[:alnum:]]",dict_echa_tox_values$toxval_numeric), "toxval_numeric"] <- gsub("(^0\\s+[^[:alnum:]]\\s+)(\\d+\\.*\\d*)(\\s+.*)","\\2",dict_echa_tox_values[grep("^0\\s+[^[:alnum:]]",dict_echa_tox_values$toxval_numeric), "toxval_numeric"])
  dict_echa_tox_values$toxval_numeric <- as.numeric(dict_echa_tox_values$toxval_numeric)
  dict_echa_tox_values[grep("\\)",dict_echa_tox_values$toxval_numeric_qualifier), "toxval_numeric_qualifier"] <- "-"

  print(View(dict_echa_tox_values))
  #runInsertTable(dict_echa_tox_values, "echa2_toxicity_value_dictionary", source.db, do.halt=T,verbose=F)

  #names(res)[names(res) == "toxval_numeric"] <- "toxval_numeric_original"
  res$toxval_numeric_original <- res$toxval_numeric
  res$toxval_units_original <- res$toxval_numeric
  res$toxval_numeric_qualifier_original <- res$toxval_numeric


  for(i in 1:nrow(dict_echa_tox_values)) {
    valold <- dict_echa_tox_values[i,1]
    valnew <- dict_echa_tox_values[i,2]
    res[is.element(res$toxval_numeric_original,valold),"toxval_numeric"] <- valnew
  }

  for(i in 1:nrow(dict_echa_tox_values)) {
    valold <- dict_echa_tox_values[i,1]
    valnew <- dict_echa_tox_values[i,3]
    res[is.element(res$toxval_numeric_original,valold),"toxval_numeric_qualifier"] <- valnew
  }

  for(i in 1:nrow(dict_echa_tox_values)) {
    valold <- dict_echa_tox_values[i,1]
    valnew <- dict_echa_tox_values[i,4]
    res[is.element(res$toxval_numeric_original,valold),"toxval_units"] <- valnew
  }

  res[res$toxval_numeric<100000,] -> res
  res[!is.na(res$toxval_numeric),] -> res

  #####################################################################
  cat("add other columns to res\n")
  #####################################################################
  res$source <- source
  res$subsource <- "eChemPortal"
  res$details_text <- "ECHA details"
  res$human_eco <- "human health"
  #Fix Year to only MAX Year
  res$year <- unlist(lapply(res$year, FUN = function(x){max(as.numeric(unlist(strsplit(x,", "))))}))

  name.list <- names(res)
  name.list[is.element(name.list,"GLP")] <- "glp"
  name.list[is.element(name.list,"Guideline")] <- "guideline"
  name.list[is.element(name.list,"Reliability")] <- "quality"
  names(res) <- name.list

  file <- "./echa/echa_files/ECHA goidline map.xlsx"
  dict <- read.xlsx(file)
  dict <- dict[dict[,"study_duration_value"]>0,]
  for(i in 1:nrow(dict)) {
    guideline <- dict[i,"guideline"]
    study_duration_units <- dict[i,"study_duration_units"]
    study_duration_value <- dict[i,"study_duration_value"]
    res[is.element(res[,"guideline"],guideline),"study_duration_value"] <- study_duration_value
    res[is.element(res[,"guideline"],guideline),"study_duration_units"] <- study_duration_units
    x <- res[is.element(res[,"guideline"],guideline),]
  }

  res$exposure_method = str_trim(res$exposure_method)

  #Toxval_Type Collapse
  res$toxval_type[grep("NOELR",res$toxval_type)] <- "N-OELR"
  res$toxval_type[grep("NOEL",res$toxval_type)] <- "NOEL"
  res$toxval_type[grep("LOEL",res$toxval_type)] <- "LOEL"
  res$toxval_type[grep("NOAEL",res$toxval_type)] <- "NOAEL"
  res$toxval_type[grep("LOAEL",res$toxval_type)] <- "LOAEL"
  res$toxval_type[grep("NOEC",res$toxval_type)] <- "NOEC"
  res$toxval_type[grep("LOEC",res$toxval_type)] <- "LOEC"
  res$toxval_type[grep("NOAEC",res$toxval_type)] <- "NOAEC"
  res$toxval_type[grep("LOAEC",res$toxval_type)] <- "LOAEC"
  res$toxval_type[grep("Toxicity threshold",res$toxval_type, ignore.case = TRUE)] <- "Toxicity threshold"
  res$toxval_type[grep("Geometric Mean",res$toxval_type, ignore.case = TRUE)] <- "Geometric Mean"
  res$toxval_type[grep("N-OELR",res$toxval_type)] <- "NOELR"
  res$toxval_type[!is.na(str_extract(res$toxval_type,"[A-Za-z]+[0-9]+"))] <- str_trim(str_extract(res$toxval_type[!is.na(str_extract(res$toxval_type,"[A-Za-z]+[0-9]+"))],"[A-Za-z]+[0-9]+"))
  res$toxval_type <- gsub("other:","", res$toxval_type, fixed = TRUE)
  res$toxval_type <- str_trim(res$toxval_type)

  #Fix species
  res$species <- gsub("^.*: ", "", res$species)
  res$species <- gsub(",.*$", "", res$species)
  res$species <- gsub(";.*$", "", res$species)
  res$species <- gsub(" and .*$", "", res$species)
  res$species <- gsub("\\(.*$", "", res$species)
  res$species <- gsub("\\+.*$", "", res$species)

  res$species[grep("sludge",res$species)] <- "sludge microorganisms"
  res$species[grep("sewage",res$species)] <- "sludge microorganisms"
  res$species[grep("2xDaphnia",res$species)] <- "Daphnia"
  res$species[grep("Beta vulgaris",res$species)] <- "beta vulgaris"

  res$species <- gsub("^.* of ", "", res$species)
  res$species <- gsub(" from .*$", "", res$species)
  res$species <- gsub("\\).*$", "", res$species)
  res$species <- str_trim(res$species)
  res$species <- gsub("^[0-9]*", "", res$species)
  res$species <- gsub("-.*$", "", res$species)
  res$species <- str_trim(res$species)


  res$species <- tolower(res$species)
  res <- res[,names(res)[which(names(res) != "species_original")]]
  name.list <- names(res)
  name.list[is.element(name.list,"species")] <- "species_original"

  res <- fill.toxval.defaults(toxval.db,res)

  #####################################################################
  cat("update critical effects\n")
  #####################################################################
  print(dim(res))
  file <- "./echa/echa_files/ECHA critical_effect map.xlsx"
  ce <- read.xlsx(file)
  for(i in 1:nrow(ce)) {
    ce.old <- ce[i,2]
    ce.new <- ce[i,1]
    res[is.element(res$critical_effect,ce.old),"critical_effect"] <- ce.new
    if(i%%500==0) cat("   finished",i,"rows\n")
  }

  res$source_url <- "https://www.echemportal.org/echemportal/index.action"

  res$toxval_numeric_original <- res$toxval_numeric
  res <- unique(res)

  #####################################################################
  cat("checks, finds and replaces non ascii characters in res with XXX\n")
  #####################################################################
  res <- fix.non_ascii(res)

  # trims leading and trailing whitespaces from the dataframe
  res <- data.frame(lapply(res, function(x) if(class(x)=="character") trimws(x) else(x)), stringsAsFactors=F, check.names=F)
  print(names(res))

  #####################################################################
  cat("add toxval_id to res\n")
  #####################################################################
  #browser()

  count <- runQuery("select count(*) from toxval",toxval.db)[1,1]
  if(count==0) tid0 <- 1
  else tid0 <- runQuery("select max(toxval_id) from toxval",toxval.db)[1,1] + 1
  tids <- seq(from=tid0,to=tid0+nrow(res)-1)
  res$toxval_id <- tids


  #####################################################################
  cat("map chemicals\n")
  #####################################################################
  name.list <- c("casrn","name",names(res)[!is.element(names(res),c("casrn","name"))])
  res <- res[,name.list]
  casrn.list <- res[,c("casrn","name")]
  cid.list <- get.cid.list.toxval(toxval.db, casrn.list,source)
  res$chemical_id <- cid.list$chemical_id
  #res <- merge(res,cid.list)
  res <- res[,!is.element(names(res),c("casrn","name"))]


  #####################################################################
  cat("pull out record source to refs\n")
  #####################################################################
  refs <- res[,c("toxval_id","source","year","glp","quality","guideline","document_name","guideline_qualifier")]

  #####################################################################
  cat("add extra columns to refs\n")
  #####################################################################
  refs[,"guideline"] <- paste(refs[,"guideline"],refs[,"guideline_qualifier"])
  refs <- refs[,!is.element(names(refs),"guideline_qualifier")]
  refs$record_source_type <- "website"
  refs$record_source_note <- "currently, there are no URLs for specific records"
  refs$record_source_level <- "secondary"

  #####################################################################
  cat("delete unused columns from res\n")
  #####################################################################
  res <- res[,!is.element(names(res),c("guideline_qualifier","document_name","guideline","glp","quality"))]

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

