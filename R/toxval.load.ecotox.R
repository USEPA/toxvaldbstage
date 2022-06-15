#-------------------------------------------------------------------------------------
#' Load ECOTOX from toxval_source to toxval
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param verbose Whether the loaded rows should be printed to the console.
#' @param do.load If TRUE, load the data from the input file and put into a global variable
#' @export
#--------------------------------------------------------------------------------------
toxval.load.ecotox <- function(toxval.db,verbose=T,do.load=T) {
  printCurrentFunction(toxval.db)
  
  #####################################################################
  cat("start output log, log files for each source can be accessed from output_log folder\n")
  #####################################################################
  source <- "ECOTOX"
  
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
  if(do.load) {
    cat("load ECOTOX data\n")
    file <- paste0(toxval.config()$datapath,"ecotox/ecotox_files/ecotox_web_services_vw_090121.xlsx")
    print(file)
    temp <- read.xlsx(file)
    print(dim(temp))
    print(View(temp))
    temp <- unique(temp)
    print(dim(temp))
    ECOTOX <<- temp
  }
  res <- ECOTOX
  dict <- unique(ECOTOX[,c("SPECIES_SCIENTIFIC_NAME","SPECIES_COMMON_NAME","SPECIES_GROUP","HABITAT")])
  file <- paste0(toxval.config()$datapath,"ecotox/ecotox_files/ECOTOX_dictionary_",Sys.Date(),".xlsx")
  write.xlsx(dict,file)

  name.list <- c("HABITAT" ,"casrn","name","CHEMICAL_GRADE",
                 "CHEMICAL_PURITY","species_original","species_common","population",
                 "ORGANISM_AGE","ORGANISM_AGE_UNITS","lifestage","exposure_route",
                 "quality","CHEMICAL_ANALYSIS_METHOD","media","TEST_LOCATION",
                 "NUMBER_DOSES","study_duration_value","study_duration_units","toxval_type",
                 "study_type","critical_effect","RESPONSE_SITE","CONC1_TYPE_STD",
                 "toxval_numeric","toxval_units","CONC1_AUTHOR","CONC1_UNITS_AUTHOR","source_study_id","author", "title",
                 "long_ref","year","SUMMARY_ADDITIONAL_PARAMETERS")

  names(res) <- name.list
  name.list <- c("casrn","name",
                 "species_original","species_common",
                 "exposure_route",
                 "quality","media",
                 "study_duration_value","study_duration_units","toxval_type",
                 "study_type","critical_effect","lifestage",
                 "toxval_numeric","toxval_units","source_study_id","author", "title",
                 "long_ref","year")
  res <- res[,name.list]
  print(dim(res))
  res <- unique(res)
  print(dim(res))


  tt.list <- res$toxval_type
  #tt.list <- substr(tt.list,1,2)
  tt.list <- gsub("(^[a-zA-Z]+)([0-9]*)(.*)","\\1",tt.list)

  res1 <- res[tt.list!="LT",]
  res2 <- res[tt.list=="LT",]
  dose.units <- res2$toxval_units
  time.value <- res2$study_duration_value
  dose.value <- res2$toxval_numeric
  dose.value <- gsub("NR",NA, dose.value)
  dose.qualifier <- str_extract_all(dose.value, "[^0-9.E+-]+", simplify = TRUE)[,1]
  dose.value <- gsub("[^0-9.E+-]+", "", dose.value)
  time.units <- res2$study_duration_units
  type <- res2$toxval_type

  for(i in 1:length(dose.value)) {
    if(!is.na(as.numeric(dose.value[i]))) {
      dose.value[i] <- signif(as.numeric(dose.value[i]),digits=4)
    }
  }

  new.type <- paste0(type,"@",dose.qualifier," ",dose.value," ",dose.units)
  res3 <- res2
  res3$toxval_type <- new.type
  res3$toxval_numeric <- time.value
  res3$toxval_units <- time.units

  res <- rbind(res1,res3)

  x <- res[,"casrn"]
  for(i in 1:length(x)) x[i] <- fix.casrn(x[i])
  res[,"casrn"] <- x
  res[is.element(res[,"study_duration_value"],"NR"),"study_duration_value"] <- NA
  res[is.element(res[,"toxval_numeric"],"NR"),"toxval_numeric"] <- NA
  res[is.element(res[,"toxval_numeric"],"+ NR"),"toxval_numeric"] <- NA

  res <- res[!is.na(res[,"toxval_numeric"]),]
  res$quality <- paste("Control type:",res$quality)
  x <- res$toxval_numeric
  x <- str_replace_all(x,"ca","~")
  x <- str_replace_all(x," ","")
  x <- str_replace_all(x,"\\,","")
  #Pull out qualifier
  res$toxval_numeric_qualifier <- str_extract_all(x, "[^0-9.E+-]+", simplify = TRUE)[,1]
  res$toxval_numeric <- as.numeric(str_extract_all(x, "[0-9.E+-]+", simplify = TRUE)[,1])
  res <- res[!is.na(res$toxval_numeric),]
  res <- res[res$toxval_numeric>=0,]
  res$toxval_numeric_qualifier[which(is.na(res$toxval_numeric_qualifier)|res$toxval_numeric_qualifier == "")] <- "-"

  res$exposure_method <- res$media


  #####################################################################
  cat("add other columns to res\n")
  #####################################################################
  res$source <- source
  #Get rid of wacko characters
  do.fix.units <- T
  if(do.fix.units) {
    res$toxval_units <- str_replace_all(res$toxval_units,"AI ","")
    res$toxval_units <- str_replace_all(res$toxval_units,"ae ","")
    res$toxval_units <- str_replace_all(res$toxval_units,"ai ","")
  }
  res$toxval_type <- str_replace_all(res$toxval_type,fixed("*"),"")
  res$toxval_type <- str_replace_all(res$toxval_type,fixed("/"),"")
  res$study_type <- str_replace_all(res$study_type,fixed("~"),"")
  res$study_duration_value <- str_replace_all(res$study_duration_value,fixed("~"),"")
  res$study_duration_value <- str_replace_all(res$study_duration_value,fixed(">"),"")
  res$study_duration_value <- str_replace_all(res$study_duration_value,fixed("<"),"")
  res$study_duration_value <- str_replace_all(res$study_duration_value,fixed("="),"")
  res$study_duration_value <- str_replace_all(res$study_duration_value,fixed("NR"),"-999")
  res$study_duration_value <- as.numeric(res$study_duration_value)
  res$study_duration_value[is.na(res$study_duration_value)] <- -999
  res$study_duration_units <- str_replace_all(res$study_duration_units,"Day(s)","Day")
  res$study_duration_class <- "chronic"
  res[regexpr("Day",res$study_duration_units, ignore.case = TRUE)+1 & res$study_duration_value<=28 & res$study_duration_value>-1,"study_duration_class"] <- "acute"
  res$risk_assessment_class <- tolower(paste(res$study_type,":",res$study_duration_class,sep=""))
  res$source_source_id <- res$source_study_id
  res <- unique(res)
  res$subsource <- "EPA ORD"
  res$source_url <- "https://cfpub.epa.gov/ecotox/"
  res$details_text <- "ECOTOX Details"
  res$datestamp <- Sys.time()
  res$datestamp <- as.character(res$datestamp)
  res[,"species_original"] <- tolower(res[,"species_original"])
  res <- res[,!is.element(names(res),"species_common")]
  res <- fill.toxval.defaults(toxval.db,res)
  res <- generate.originals(toxval.db,res)
  print(dim(res))
  #res$toxval_numeric_original <- res$toxval_numeric
  res <- unique(res)
  print(dim(res))

  #####################################################################
  cat("checks, finds and replaces non ascii characters in res with XXX\n")
  #####################################################################
  res <- fix.non_ascii(res)

  # trims leading and trailing whitespaces from the dataframe
  res <- data.frame(lapply(res, function(x) if(class(x)=="character") trimws(x) else(x)), stringsAsFactors=F, check.names=F)
  #print(str(res))

  # file <- paste0(toxval.config()$datapath,"ecotox/ecotox_files/ECOTOX_",Sys.Date(),".xlsx")
  # write.xlsx(res,file)

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


  #####################################################################
  cat("pull out record source to refs\n")
  #####################################################################
  refs <- res[,c("toxval_id","source","author","title","long_ref","year","source_study_id","quality")]

  #####################################################################
  cat("add extra columns to refs\n")
  #####################################################################
  refs$page = str_trim(gsub("p.","",word(refs$long_ref,-1,sep=":")))
  refs$volume = gsub("\\(.*\\)","",word(gsub("^.*\\.","",gsub(":[^:]*$","",refs$long_ref)),-1))
  refs[grep("^\\d+$", refs$volume, invert = T),"volume"] = ""
  refs$volume <- as.numeric(refs$volume)
  refs$journal = gsub("[0-9\\(\\)]*:[^:]*$","",refs$long_ref)
  refs$long_ref = paste0(refs$author," (",refs$year,"). ",refs$title,". ",refs$long_ref)
  refs$record_source_type <- "journal article"
  refs$record_source_note <- "-"
  refs$record_source_level <- "primary"

  #####################################################################
  cat("delete unused columns from res\n")
  #####################################################################
  res <- res[!is.element(names(res),c("author","title","long_ref","quality","source_study_id"))]

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

  #####################################################################
  cat("fix species by source\n")
  #####################################################################
  fix.species.by.source(toxval.db, source)

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
      exposure_form, media, toxval_subtype, generation) by source\n")
  #####################################################################
  fix.all.param.new.by.source(toxval.db, source)

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
