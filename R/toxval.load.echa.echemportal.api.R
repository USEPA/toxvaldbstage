#-------------------------------------------------------------------------------------
#' Load echa_echemportal_api from toxval_source to toxval
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param source.db The source database to use.
#' @param verbose Whether the loaded rows should be printed to the console.
#' @export
#--------------------------------------------------------------------------------------
toxval.load.echa.echemportal.api <- function(toxval.db,source.db,log=F) {
  printCurrentFunction(toxval.db)
  source <- "ECHA eChemPortal"
  source_table = "source_echa_echemportal_api"
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
 # query = paste0("select * from ",source_table," limit 1000")
  res = runQuery(query,source.db,T,F)
  res = res[ , !(names(res) %in% c("source_id","clowder_id","parent_hash","create_time","modify_time","created_by"))]
  res$source = source
  res$details_text = paste(source,"Details")
  print(dim(res))
  #####################################################################
  cat("Add the code from the original version from Aswani\n")
  #####################################################################
  res$toxval_type <- res$value_type
  res$study_type <- res$endpoint_type
  res$exposure_route <- res$route_of_administration_exposure
  res$exposure_method <- res$route_of_administration_exposure
  res$exposure_form <- res$inhalation_exposure_type
  res$critical_effect <- res$basis
  res$study_duration_value <- res$duration_sampling_time
  res$study_duration_units <- res$duration_sampling_time
  res <- generate.originals(toxval.db,res)
  res1 <- res
  res1$sex <- "-"

  # change encoding to utf8
  res1$experimental_value <- enc2utf8(res1$experimental_value)

  # change column names
  names(res1)[names(res1) == "participant"] <- "subsource"
  names(res1)[names(res1) == "reliability"] <- "quality"
  names(res1)[names(res1) == "guidelines_qualifiers"] <- "guideline"
  names(res1)[names(res1) == "glp_compliance"] <- "glp"
  res1$source_table <- gsub('(.)([[:upper:]])','\\1 \\2', res1$source_table)
  res1$source_table <- gsub("(.*)(\\s+Teratogenicity)", "\\1 /\\2", res1$source_table)

  #####################################################################
  cat("extract minimum of multiple years\n")
  #####################################################################
  res1$year <- sapply(
    str_extract_all(res1$year, "[0-9]+"),
    function(x) min(as.integer(x))
  )

  # extract qualifier values
  res1[grep("^[^0-9]+", res1$experimental_value),"toxval_numeric_qualifier"] <-  gsub("(^[^0-9]+)(\\s+\\d+.*)","\\1", res1[grep("^[^0-9]+", res1$experimental_value),"experimental_value"])
  res1[grep("\\:", res1$toxval_numeric_qualifier),"toxval_numeric_qualifier"] <- gsub("(.*\\:)(.*)","\\2",res1[grep("\\:", res1$toxval_numeric_qualifier),"toxval_numeric_qualifier"])
  res1$toxval_numeric_qualifier <- gsub("^\\s+", "", res1$toxval_numeric_qualifier)
  # extract minimum numeric value
  res1[grep("\\d+\\s+.*\\s+\\d+", res1$experimental_value), "toxval_numeric"] <- sapply( str_extract_all(res1[grep("\\d+\\s+.*\\s+\\d+", res1$experimental_value), "experimental_value"], "\\d+\\.*\\d*"),function(x) min(as.numeric(x)))
  res1$toxval_units <- res1$experimental_value
  #####################################################################
  cat("checks, finds and replaces non ascii characters in res1 columns with XXX\n")
  #####################################################################
  res1[,c("experimental_value","toxval_numeric_qualifier","toxval_units")] <- fix.non_ascii.v2(res1[,c("experimental_value","toxval_numeric_qualifier","toxval_units")],source)

  #####################################################################
  cat("substituting simultaneous appearance of non ascii flag to one occurance to prevent the trucation of values\n")
  #####################################################################
  res1[grep(".*(<c.*\\?)\\s*\\d+\\.*\\d*\\s*(<c.*\\?).*",res1$experimental_value),"experimental_value"] <- gsub("(<c[0-9b]>\\?)+","XXX",res1[grep(".*(<c.*\\?)\\s*\\d+\\.*\\d*\\s*(<c.*\\?).*",res1$experimental_value),"experimental_value"])
  res1$experimental_value[grep("XXX", res1$experimental_value)]<- gsub("(XXX)+","XXX",res1$experimental_value[grep("XXX", res1$experimental_value)])
  res1[grep(".*(<c.*\\?).*",res1$experimental_value),"experimental_value"] <- gsub("(<c[0-9b]>\\?)+","XXX",res1[grep(".*(<c.*\\?).*",res1$experimental_value),"experimental_value"])
  res1$experimental_value[grep("XXX", res1$experimental_value)] <- gsub("(XXX)+","XXX",res1$experimental_value[grep("XXX", res1$experimental_value)])

  #####################################################################
  cat("read in dictionary from source\n")
  #####################################################################
  #query <- "select * from echa_echemportal_api_dict"
  #dict_effect_levels_2 <- runQuery(query,source.db,T,F)

  file = paste0("../dictionary/echa_echemportal_api_dict_2021-08-18.xlsx")
  dict_effect_levels_2 = openxlsx::read.xlsx(file)
  dict_effect_levels_2[grep("XXX",dict_effect_levels_2$original_effect_level),"original_effect_level"] <- gsub("XXX\\?","XXX",dict_effect_levels_2[grep("XXX",dict_effect_levels_2$original_effect_level),"original_effect_level"])
  dict_effect_levels_2[grep("XXX",dict_effect_levels_2$toxval_units),"toxval_units"] <- gsub("XXX\\?","XXX",dict_effect_levels_2[grep("XXX",dict_effect_levels_2$toxval_units),"toxval_units"])

  #####################################################################
  cat("check for missing values in dictionary\n")
  #####################################################################
  x <- unique(res1$experimental_value)
  x <- x[!is.element(x,dict_effect_levels_2[,1])]
  cat("   missing values in new dictionary:",length(x),"\n")

  #####################################################################
  cat("assign toxval_numeric, toxval_numeric_qualifier, toxval_units, generation values from dictionary\n")
  #####################################################################
  for(i in 1:nrow(dict_effect_levels_2)) {
    valold = dict_effect_levels_2[i,1]
    valnew1 = dict_effect_levels_2[i,2]
    valnew2 = dict_effect_levels_2[i,3]
    valnew3 = dict_effect_levels_2[i,4]
    valnew4 = dict_effect_levels_2[i,5]
    #res1[is.element(res1$experimental_value,valold),"toxval_numeric"] <- valnew
    res1[res1$experimental_value==valold,"toxval_numeric"] = valnew1
    res1[res1$experimental_value==valold,"toxval_numeric_qualifier"] = valnew2
    res1[res1$experimental_value==valold,"toxval_units"] = valnew3
    res1[res1$experimental_value==valold,"generation"] = valnew4
    if(i%%5000==0) cat("finished",i,"out of ",nrow(dict_effect_levels_2),"\n")
  }

  #####################################################################
  cat("assign toxval_numeric_qualifier values from dictionary\n")
  #####################################################################
  # for(i in 1:nrow(dict_effect_levels_2)) {
  #   valold <- dict_effect_levels_2[i,1]
  #   valnew <- dict_effect_levels_2[i,3]
  #   #res1[is.element(res1$experimental_value,valold),"toxval_numeric_qualifier"] <- valnew
  #   res1[res1$experimental_value==valold,"toxval_numeric_qualifier"] <- valnew
  #   if(i%%1000==0) cat("finished",i,"out of ",nrow(dict_effect_levels_2),"\n")
  # }

  #####################################################################
  cat("assign toxval_units values from dictionary\n")
  #####################################################################
  #  for(i in 1:nrow(dict_effect_levels_2)) {
  #   valold <- dict_effect_levels_2[i,1]
  #   valnew <- dict_effect_levels_2[i,4]
  #   res1[is.element(res1$experimental_value,valold),"toxval_units"] <- valnew
  # }

  #####################################################################
  cat("assign generation values from dictionary\n")
  #####################################################################
  # for(i in 1:nrow(dict_effect_levels_2)) {
  #   valold <- dict_effect_levels_2[i,1]
  #   valnew <- dict_effect_levels_2[i,5]
  #   res1[is.element(res1$experimental_value,valold),"generation"] <- valnew
  # }

  #####################################################################
  cat("fix toxval_type\n")
  #####################################################################
  res1$value_type <- enc2utf8(res1$value_type)
  res1$toxval_type <- sapply(str_extract_all(res1$value_type, "^[A-Z]+[0-9]*$"), paste, collapse= ' ')
  type <- res1[which(res1$toxval_type == ""), "value_type"]
  type <- data.frame(unique(type), stringsAsFactors = F)
  names(type) <- "original_tox_type"
  type$toxval_type <- gsub("(.*)(\\b[A-Z]{2,6}?\\b)(.*)", "\\2",type$original_tox_type)
  type[grep("\\b[A-Za-z]+[0-9]+\\b", type$toxval_type), "toxval_type"] <- gsub("(.*)(\\b[A-Za-z]+[0-9]+\\b)(.*)", "\\2",type[grep("\\b[A-Za-z]+[0-9]+\\b", type$toxval_type), "toxval_type"])
  type[grep("No-effect dose level", type$toxval_type), "toxval_type"] <- "NEL"
  type[grep("dose level", type$toxval_type), "toxval_type"] <- "LEL"
  type[grep("conc. level", type$toxval_type), "toxval_type"] <- "LEL"
  type[grep("effective dose level", type$original_tox_type), "toxval_type"] <- "ED50"
  type[grep("lethal dose range", type$original_tox_type), "toxval_type"] <- "LD"
  type[grep("TCLo", type$original_tox_type), "toxval_type"] <- "TCLo"
  type[grep("TDLo", type$original_tox_type), "toxval_type"] <- "TDLo"
  type[grep("Maximal Tolerated Dose", type$original_tox_type, ignore.case = T), "toxval_type"] <- "MTD"
  type[grep("other: highest", type$original_tox_type, ignore.case = T), "toxval_type"] <- "HDT"
  type[grep("other: Max.*\\s*tolerate", type$original_tox_type, ignore.case = T), "toxval_type"] <- "MTD"
  type[grep("other: approximate lethal dose", type$original_tox_type, ignore.case = T), "toxval_type"] <- "ALD"
  type[grep("Median Lethal Dose", type$original_tox_type, ignore.case = T), "toxval_type"] <- "LD50"
  type[grep("Discriminating dose", type$original_tox_type, ignore.case = T), "toxval_type"] <- "LEL"
  type[grep("maximal non-lethal dose", type$original_tox_type, ignore.case = T), "toxval_type"] <- "ND50"
  type[grep("NO adverse(.*)EFFECT LEVEL",type$toxval_type, ignore.case = T), "toxval_type"] <- "NOAEL"
  type[grep("NO(.*)EFFECT LEVEL",type$toxval_type, ignore.case = T), "toxval_type"] <- "NOEL"
  type[grep("approx(.*)lethal dose",type$toxval_type, ignore.case = T), "toxval_type"] <- "ALD"
  type[grep("mini(.*)lethal dose",type$toxval_type, ignore.case = T), "toxval_type"] <- "LD0"
  type[grep("lethal dose",type$toxval_type, ignore.case = T), "toxval_type"] <- "LD"
  type[grep("no effect concentration",type$toxval_type, ignore.case = T), "toxval_type"] <- "NEC"
  for(i in 1:nrow(type)) {
    valold <- type[i,1]
    valnew <- type[i,2]
    res1[is.element(res1$value_type,valold), "toxval_type"] <- valnew
  }

  #####################################################################
  cat("fix study type, study_type info extracted only from endpoint_type\n")
  #####################################################################
  res1$property_name <- enc2utf8(res1$property_name)
  res1$endpoint_type <- enc2utf8(res1$endpoint_type)
  res1$study_type <- res1$endpoint_type
  res1$study_type <- str_extract_all(res1$study_type, paste(c("chronic","sub-chronic","acute","short-term", "long-term","carcinogenicity","repeated dose",  "developmental","reproductive"), collapse="|")) %>%
    sapply(., paste, collapse = ", ")
  #res1[which(res1$study_type == ""), "study_type"] <- res1[which(res1$study_type == ""),"property_name"]
  res1[grep("acute",res1$study_type, ignore.case = T),"study_type"] <- "acute"
  res1[grep("^development",res1$study_type, ignore.case = T),"study_type"] <- "developmental"
  res1[grep("ToxicityRepro",res1$study_type, ignore.case = T),"study_type"] <- "reproductive"
  res1[grep("shortTerm",res1$study_type, ignore.case = T),"study_type"] <- "short-term"
  res1[grep("LongTerm",res1$study_type, ignore.case = T),"study_type"] <- "long-term"
  res1[grep("^ToxicityTo",res1$property_name, ignore.case = T),"study_type"] <- res1[grep("^ToxicityTo",res1$property_name, ignore.case = T),"endpoint_type"]
  res1[grep("short-term$|short-term dietary",res1$study_type, ignore.case = T),"study_type"] <- "short-term"
  res1[grep("\\s+long-term$",res1$study_type, ignore.case = T),"study_type"] <- "long-term"
  res1[grep("reproduction",res1$study_type, ignore.case = T),"study_type"] <- "reproductive"
  #res1[grep("^tox|^activ|^other|^avoid",res1$study_type, ignore.case = T),"study_type"] <- "-"
  res1[which(res1$study_type == ""),"study_type"] <- "-"

  #####################################################################
  cat("fix exposure_route, exposure_method and exposure_form\n")
  #####################################################################
  res1$exposure_route <- res1$route_of_administration_exposure
  res1[grep("^other\\:",res1$exposure_route,ignore.case = T, useBytes = T),"exposure_route"] <- gsub("(^other\\:\\s*)(.*)","\\2",res1[grep("^other\\:",res1$exposure_route,ignore.case = T, useBytes = T),"exposure_route"])
  res1[grep("^inhalation\\:",res1$exposure_route,ignore.case = T,useBytes = T),"exposure_route"] <- gsub("(.*?)(\\:\\s*.*)","\\1",res1[grep("^inhalation\\:",res1$exposure_route,ignore.case = T,useBytes = T),"exposure_route"])
  res1[grep("^oral\\:",res1$exposure_route,ignore.case = T,useBytes = T),"exposure_route"] <- gsub("(.*?)(\\:\\s*.*)","\\1",res1[grep("^oral\\:",res1$exposure_route,ignore.case = T,useBytes = T),"exposure_route"])
  res1$exposure_route <- gsub("(^\\s+)(.*)","\\2", res1$exposure_route)
  res1$exposure_route <- gsub("(.*)(\\s+$)","\\1", res1$exposure_route)
  res1$exposure_route[res1$exposure_route == ""] <- "-"
  #print(unique(res1$exposure_route))

  res1$exposure_method <- res1$route_of_administration_exposure
  res1$exposure_method <- enc2utf8(res1$exposure_method)
  res1[grep("^other\\:",res1$exposure_method,ignore.case = T),"exposure_method"] <- gsub("(^other\\:\\s*)(.*)","\\2",res1[grep("^other\\:",res1$exposure_method,ignore.case = T),"exposure_method"])
  res1[grep("^inhalation\\:",res1$exposure_method,ignore.case = T),"exposure_method"] <- gsub("(.*?\\:\\s*)(.*)","\\2",res1[grep("^inhalation\\:",res1$exposure_method,ignore.case = T),"exposure_method"])
  res1[grep("^oral\\:",res1$exposure_method,ignore.case = T),"exposure_method"] <- gsub("(.*?\\:\\s*)(.*)","\\2",res1[grep("^oral\\:",res1$exposure_method,ignore.case = T),"exposure_method"])
  res1$exposure_method <- gsub("(^\\s+)(.*)","\\2", res1$exposure_method)
  res1$exposure_method <- gsub("(.*)(\\s+$)","\\1", res1$exposure_method)
  res1$exposure_method[res1$exposure_method == ""] <- "-"
  #print(unique(res1$exposure_method))

  res1$exposure_form <- res1$inhalation_exposure_type

  #####################################################################
  cat("fix critical_effect\n")
  #####################################################################
  res1$critical_effect <- res1$basis
  # # combine the column name to the values
  res1[which(res1$histopathological_findings_neoplastic != ""),names(res1)[ names(res1)%in% "histopathological_findings_neoplastic"]] <- paste(names(res1)[ names(res1)%in% "histopathological_findings_neoplastic"], res1[which(res1$histopathological_findings_neoplastic != ""),names(res1)[ names(res1)%in% "histopathological_findings_neoplastic"]], sep = ';')
  res1 <- res1 %>% unite("critical_effect", names(res1)[ names(res1)%in% c("histopathological_findings_neoplastic","critical_effect")],sep = "|", na.rm = TRUE, remove = FALSE)
  res1$critical_effect <- gsub("^\\||\\|$","", res1$critical_effect)
  res1$critical_effect <- gsub("^\\|$","-", res1$critical_effect)
  res1$critical_effect[res1$critical_effect == ""] <- "-"
  res1$critical_effect[which(is.na(res1$critical_effect))] <- "-"

  #####################################################################
  cat("fix duration value and units\n")
  #####################################################################
  res1$exposure_duration <- res1$duration_sampling_time
  res1[grep("\\[Total exposure duration\\]", res1$exposure_duration), "exposure_duration"] <-gsub("\\s+\\[.*\\]","",res1[grep("\\[Total exposure duration\\]", res1$exposure_duration), "exposure_duration"])
  res1$study_duration_value <- gsub("(^\\d+\\.*\\d*)(\\s+.*)", "\\1", res1$exposure_duration)
  res1$study_duration_value <- as.numeric(res1$study_duration_value)
  res1[grep("^\\d+\\.*\\d*\\s+\\w+$",res1$exposure_duration), "study_duration_units"] <- gsub("(.*\\s+)(\\w+)", "\\2", res1[grep("^\\d+\\.*\\d*\\s+\\w+$",res1$exposure_duration), "exposure_duration"])
  res1[grep("^d$",res1$study_duration_units), "study_duration_units"] <- "days"
  res1[grep("^h$",res1$study_duration_units), "study_duration_units"] <- "hours"
  res1[grep("^wk$",res1$study_duration_units), "study_duration_units"] <- "weeks"
  res1[grep("^mo$",res1$study_duration_units), "study_duration_units"] <- "months"
  res1[grep("^min$",res1$study_duration_units), "study_duration_units"] <- "minutes"

  #####################################################################
  cat("subset required fields\n")
  #####################################################################
  res <- res1
  names.list <- c("chemical_id","source_hash","name","casrn","subsource","url","quality","guideline" ,
                  "glp","species","strain","sex","study_duration_value","study_duration_units",
                  "year","toxval_numeric_qualifier","toxval_numeric","toxval_units","generation",
                  "toxval_type","study_type","exposure_route","exposure_method","exposure_form","critical_effect",
                  "strain_original","year_original",
                  "toxval_type_original","study_type_original","exposure_route_original","exposure_method_original","exposure_form_original",
                  "critical_effect_original","study_duration_value_original","study_duration_units_original","document_name",
                  "source","qc_status","source_table")

  res <- res[,(names(res)%in% names.list)]

  #####################################################################
  cat("strip leading and trailing spaces\n")
  #####################################################################
  res <- data.frame(sapply(res, function(x) gsub("(.*)(\\s+$)", "\\1", x)),stringsAsFactors = F)
  res <- data.frame(sapply(res, function(x) gsub("(^\\s+)(.*)", "\\2", x)),stringsAsFactors = F)
  res$year <- as.integer(res$year)
  res$study_duration_value <- as.numeric(res$study_duration_value)
  res$toxval_numeric <- as.numeric(res$toxval_numeric)
  res$toxval_units_original <- "-"
  res$toxval_numeric_qualifier_original <- "-"
  res <- res[!is.na(res[,"casrn"]),]
  cremove = c("","","","")
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
  refs$record_source_type = "-"
  refs$record_source_note = "-"
  refs$record_source_level = "-"
  print(dim(res))

  #####################################################################
  cat("load res and refs to the database\n")
  #####################################################################
  res = unique(res)
  refs = unique(refs)
  res$datestamp = Sys.Date()
  res$source_table = source_table
  res$source_url = "https://www.echemportal.org/echemportal/"
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
  toxval.load.postprocess(toxval.db,source.db,source,do.convert.units=F)

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
  source <- "ECHA eChemPortal"

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

  query <- "select * from original_echa_echemportal_api"
  res <- runQuery(query,source.db,T,F)
  print(dim(res))

  res <- res[ , !(names(res) %in% c("source_id","clowder_id"))]
  res <- unique(res)
  print(dim(res))

  res$toxval_type <- res$value_type
  res$study_type <- res$endpoint_type
  res$exposure_route <- res$route_of_administration_exposure
  res$exposure_method <- res$route_of_administration_exposure
  res$exposure_form <- res$inhalation_exposure_type
  res$critical_effect <- res$basis
  res$study_duration_value <- res$duration_sampling_time
  res$study_duration_units <- res$duration_sampling_time

  res <- generate.originals(toxval.db,res)
  res1 <- res


  res1$sex <- "-"


  # change encoding to utf8
  res1$experimental_value <- enc2utf8(res1$experimental_value)

  # change column names
  names(res1)[names(res1) == "participant"] <- "subsource"
  names(res1)[names(res1) == "number"] <- "casrn"
  names(res1)[names(res1) == "reliability"] <- "quality"
  names(res1)[names(res1) == "guidelines_qualifiers"] <- "guideline"
  names(res1)[names(res1) == "glp_compliance"] <- "glp"


  res1$source_table <- gsub('(.)([[:upper:]])','\\1 \\2', res1$source_table)
  res1$source_table <- gsub("(.*)(\\s+Teratogenicity)", "\\1 /\\2", res1$source_table)


  #####################################################################
  cat("extract minimum of multiple years\n")
  #####################################################################

  res1$year <- sapply(
    str_extract_all(res1$year, "[0-9]+"),
    function(x) min(as.integer(x))
  )

  # extract qualifier values
  res1[grep("^[^0-9]+", res1$experimental_value),"toxval_numeric_qualifier"] <-  gsub("(^[^0-9]+)(\\s+\\d+.*)","\\1", res1[grep("^[^0-9]+", res1$experimental_value),"experimental_value"])
  res1[grep("\\:", res1$toxval_numeric_qualifier),"toxval_numeric_qualifier"] <- gsub("(.*\\:)(.*)","\\2",res1[grep("\\:", res1$toxval_numeric_qualifier),"toxval_numeric_qualifier"])
  res1$toxval_numeric_qualifier <- gsub("^\\s+", "", res1$toxval_numeric_qualifier)
  # extract minimum numeric value
  res1[grep("\\d+\\s+.*\\s+\\d+", res1$experimental_value), "toxval_numeric"] <- sapply( str_extract_all(res1[grep("\\d+\\s+.*\\s+\\d+", res1$experimental_value), "experimental_value"], "\\d+\\.*\\d*"),function(x) min(as.numeric(x)))
  res1$toxval_units <- res1$experimental_value
  #####################################################################
  cat("checks, finds and replaces non ascii characters in res1 columns with XXX\n")
  #####################################################################
  res1[,c("experimental_value","toxval_numeric_qualifier","toxval_units")] <- fix.non_ascii(res1[,c("experimental_value","toxval_numeric_qualifier","toxval_units")])

  #####################################################################
  cat("substituting simultaneous appearance of non ascii flag to one occurance to prevent the trucation of values\n")
  #####################################################################


  res1[grep(".*(<c.*\\?)\\s*\\d+\\.*\\d*\\s*(<c.*\\?).*",res1$experimental_value),"experimental_value"] <- gsub("(<c[0-9b]>\\?)+","XXX",res1[grep(".*(<c.*\\?)\\s*\\d+\\.*\\d*\\s*(<c.*\\?).*",res1$experimental_value),"experimental_value"])
  res1$experimental_value[grep("XXX", res1$experimental_value)]<- gsub("(XXX)+","XXX",res1$experimental_value[grep("XXX", res1$experimental_value)])


  res1[grep(".*(<c.*\\?).*",res1$experimental_value),"experimental_value"] <- gsub("(<c[0-9b]>\\?)+","XXX",res1[grep(".*(<c.*\\?).*",res1$experimental_value),"experimental_value"])
  res1$experimental_value[grep("XXX", res1$experimental_value)] <- gsub("(XXX)+","XXX",res1$experimental_value[grep("XXX", res1$experimental_value)])



  #####################################################################
  cat("read in dictionary from source\n")
  #####################################################################

  query <- "select * from echa_echemportal_api_dict"


  dict_effect_levels_2 <- runQuery(query,source.db,T,F)

  dict_effect_levels_2[grep("XXX",dict_effect_levels_2$original_effect_level),"original_effect_level"] <- gsub("XXX\\?","XXX",dict_effect_levels_2[grep("XXX",dict_effect_levels_2$original_effect_level),"original_effect_level"])
  dict_effect_levels_2[grep("XXX",dict_effect_levels_2$toxval_units),"toxval_units"] <- gsub("XXX\\?","XXX",dict_effect_levels_2[grep("XXX",dict_effect_levels_2$toxval_units),"toxval_units"])

  #####################################################################
  cat("check for missing values in dictionary\n")
  #####################################################################

  x <- unique(res1$experimental_value)

  x <- x[!is.element(x,dict_effect_levels_2[,1])]

  cat("   missing values in new dictionary:",length(x),"\n")

  # file <- paste0("./dictionary/missing_echa_echemportal_api_dict_",Sys.Date(),".xlsx")
  # write.xlsx(x, file)

  #####################################################################
  cat("assign toxval_numeric values from dictionary\n")
  #####################################################################


  for(i in 1:nrow(dict_effect_levels_2)) {
    valold <- dict_effect_levels_2[i,1]
    valnew <- dict_effect_levels_2[i,2]
    res1[is.element(res1$experimental_value,valold),"toxval_numeric"] <- valnew
  }

  #####################################################################
  cat("assign toxval_numeric_qualifier values from dictionary\n")
  #####################################################################


  for(i in 1:nrow(dict_effect_levels_2)) {
    valold <- dict_effect_levels_2[i,1]
    valnew <- dict_effect_levels_2[i,3]
    res1[is.element(res1$experimental_value,valold),"toxval_numeric_qualifier"] <- valnew
  }

  #####################################################################
  cat("assign toxval_units values from dictionary\n")
  #####################################################################



  for(i in 1:nrow(dict_effect_levels_2)) {
    valold <- dict_effect_levels_2[i,1]
    valnew <- dict_effect_levels_2[i,4]
    res1[is.element(res1$experimental_value,valold),"toxval_units"] <- valnew
  }

  print(head(unique(res1$toxval_units), 10))

  #####################################################################
  cat("assign generation values from dictionary\n")
  #####################################################################


  for(i in 1:nrow(dict_effect_levels_2)) {
    valold <- dict_effect_levels_2[i,1]
    valnew <- dict_effect_levels_2[i,5]
    res1[is.element(res1$experimental_value,valold),"generation"] <- valnew
  }

  #####################################################################
  cat("fix toxval_type\n")
  #####################################################################

  res1$value_type <- enc2utf8(res1$value_type)
  res1$toxval_type <- sapply(str_extract_all(res1$value_type, "^[A-Z]+[0-9]*$"), paste, collapse= ' ')
  type <- res1[which(res1$toxval_type == ""), "value_type"]
  type <- data.frame(unique(type), stringsAsFactors = F)
  names(type) <- "original_tox_type"
  type$toxval_type <- gsub("(.*)(\\b[A-Z]{2,6}?\\b)(.*)", "\\2",type$original_tox_type)
  type[grep("\\b[A-Za-z]+[0-9]+\\b", type$toxval_type), "toxval_type"] <- gsub("(.*)(\\b[A-Za-z]+[0-9]+\\b)(.*)", "\\2",type[grep("\\b[A-Za-z]+[0-9]+\\b", type$toxval_type), "toxval_type"])
  type[grep("No-effect dose level", type$toxval_type), "toxval_type"] <- "NEL"
  type[grep("dose level", type$toxval_type), "toxval_type"] <- "LEL"
  type[grep("conc. level", type$toxval_type), "toxval_type"] <- "LEL"
  type[grep("effective dose level", type$original_tox_type), "toxval_type"] <- "ED50"
  type[grep("lethal dose range", type$original_tox_type), "toxval_type"] <- "LD"
  type[grep("TCLo", type$original_tox_type), "toxval_type"] <- "TCLo"
  type[grep("TDLo", type$original_tox_type), "toxval_type"] <- "TDLo"
  type[grep("Maximal Tolerated Dose", type$original_tox_type, ignore.case = T), "toxval_type"] <- "MTD"
  type[grep("other: highest", type$original_tox_type, ignore.case = T), "toxval_type"] <- "HDT"
  type[grep("other: Max.*\\s*tolerate", type$original_tox_type, ignore.case = T), "toxval_type"] <- "MTD"
  type[grep("other: approximate lethal dose", type$original_tox_type, ignore.case = T), "toxval_type"] <- "ALD"
  type[grep("Median Lethal Dose", type$original_tox_type, ignore.case = T), "toxval_type"] <- "LD50"
  type[grep("Discriminating dose", type$original_tox_type, ignore.case = T), "toxval_type"] <- "LEL"
  type[grep("maximal non-lethal dose", type$original_tox_type, ignore.case = T), "toxval_type"] <- "ND50"
  type[grep("NO adverse(.*)EFFECT LEVEL",type$toxval_type, ignore.case = T), "toxval_type"] <- "NOAEL"
  type[grep("NO(.*)EFFECT LEVEL",type$toxval_type, ignore.case = T), "toxval_type"] <- "NOEL"
  type[grep("approx(.*)lethal dose",type$toxval_type, ignore.case = T), "toxval_type"] <- "ALD"
  type[grep("mini(.*)lethal dose",type$toxval_type, ignore.case = T), "toxval_type"] <- "LD0"
  type[grep("lethal dose",type$toxval_type, ignore.case = T), "toxval_type"] <- "LD"
  type[grep("no effect concentration",type$toxval_type, ignore.case = T), "toxval_type"] <- "NEC"
  for(i in 1:nrow(type)) {
    valold <- type[i,1]
    valnew <- type[i,2]
    res1[is.element(res1$value_type,valold), "toxval_type"] <- valnew
  }

  names(res1)[names(res1) == "number"] <- "casrn"

  #####################################################################
  cat("fix study type, study_type info extracted only from endpoint_type\n")
  #####################################################################

  res1$property_name <- enc2utf8(res1$property_name)
  res1$endpoint_type <- enc2utf8(res1$endpoint_type)
  res1$study_type <- res1$endpoint_type
  res1$study_type <- str_extract_all(res1$study_type, paste(c("chronic","sub-chronic","acute","short-term", "long-term","carcinogenicity","repeated dose",  "developmental","reproductive"), collapse="|")) %>%
    sapply(., paste, collapse = ", ")
  #res1[which(res1$study_type == ""), "study_type"] <- res1[which(res1$study_type == ""),"property_name"]
  res1[grep("acute",res1$study_type, ignore.case = T),"study_type"] <- "acute"
  res1[grep("^development",res1$study_type, ignore.case = T),"study_type"] <- "developmental"
  res1[grep("ToxicityRepro",res1$study_type, ignore.case = T),"study_type"] <- "reproductive"
  res1[grep("shortTerm",res1$study_type, ignore.case = T),"study_type"] <- "short-term"
  res1[grep("LongTerm",res1$study_type, ignore.case = T),"study_type"] <- "long-term"
  res1[grep("^ToxicityTo",res1$property_name, ignore.case = T),"study_type"] <- res1[grep("^ToxicityTo",res1$property_name, ignore.case = T),"endpoint_type"]
  res1[grep("short-term$|short-term dietary",res1$study_type, ignore.case = T),"study_type"] <- "short-term"
  res1[grep("\\s+long-term$",res1$study_type, ignore.case = T),"study_type"] <- "long-term"
  res1[grep("reproduction",res1$study_type, ignore.case = T),"study_type"] <- "reproductive"
  #res1[grep("^tox|^activ|^other|^avoid",res1$study_type, ignore.case = T),"study_type"] <- "-"
  res1[which(res1$study_type == ""),"study_type"] <- "-"

  #####################################################################
  cat("fix exposure_route, exposure_method and exposure_form\n")
  #####################################################################

  res1$exposure_route <- res1$route_of_administration_exposure
  res1[grep("^other\\:",res1$exposure_route,ignore.case = T, useBytes = T),"exposure_route"] <- gsub("(^other\\:\\s*)(.*)","\\2",res1[grep("^other\\:",res1$exposure_route,ignore.case = T, useBytes = T),"exposure_route"])
  res1[grep("^inhalation\\:",res1$exposure_route,ignore.case = T,useBytes = T),"exposure_route"] <- gsub("(.*?)(\\:\\s*.*)","\\1",res1[grep("^inhalation\\:",res1$exposure_route,ignore.case = T,useBytes = T),"exposure_route"])
  res1[grep("^oral\\:",res1$exposure_route,ignore.case = T,useBytes = T),"exposure_route"] <- gsub("(.*?)(\\:\\s*.*)","\\1",res1[grep("^oral\\:",res1$exposure_route,ignore.case = T,useBytes = T),"exposure_route"])
  res1$exposure_route <- gsub("(^\\s+)(.*)","\\2", res1$exposure_route)
  res1$exposure_route <- gsub("(.*)(\\s+$)","\\1", res1$exposure_route)
  res1$exposure_route[res1$exposure_route == ""] <- "-"
  #print(unique(res1$exposure_route))

  res1$exposure_method <- res1$route_of_administration_exposure
  res1$exposure_method <- enc2utf8(res1$exposure_method)
  res1[grep("^other\\:",res1$exposure_method,ignore.case = T),"exposure_method"] <- gsub("(^other\\:\\s*)(.*)","\\2",res1[grep("^other\\:",res1$exposure_method,ignore.case = T),"exposure_method"])
  res1[grep("^inhalation\\:",res1$exposure_method,ignore.case = T),"exposure_method"] <- gsub("(.*?\\:\\s*)(.*)","\\2",res1[grep("^inhalation\\:",res1$exposure_method,ignore.case = T),"exposure_method"])
  res1[grep("^oral\\:",res1$exposure_method,ignore.case = T),"exposure_method"] <- gsub("(.*?\\:\\s*)(.*)","\\2",res1[grep("^oral\\:",res1$exposure_method,ignore.case = T),"exposure_method"])
  res1$exposure_method <- gsub("(^\\s+)(.*)","\\2", res1$exposure_method)
  res1$exposure_method <- gsub("(.*)(\\s+$)","\\1", res1$exposure_method)
  res1$exposure_method[res1$exposure_method == ""] <- "-"
  #print(unique(res1$exposure_method))

  res1$exposure_form <- res1$inhalation_exposure_type

  #####################################################################
  cat("fix critical_effect\n")
  #####################################################################

  res1$critical_effect <- res1$basis
  # # combine the column name to the values
  res1[which(res1$histopathological_findings_neoplastic != ""),names(res1)[ names(res1)%in% "histopathological_findings_neoplastic"]] <- paste(names(res1)[ names(res1)%in% "histopathological_findings_neoplastic"], res1[which(res1$histopathological_findings_neoplastic != ""),names(res1)[ names(res1)%in% "histopathological_findings_neoplastic"]], sep = ';')
  res1 <- res1 %>% unite("critical_effect", names(res1)[ names(res1)%in% c("histopathological_findings_neoplastic","critical_effect")],sep = "|", na.rm = TRUE, remove = FALSE)
  res1$critical_effect <- gsub("^\\||\\|$","", res1$critical_effect)
  res1$critical_effect <- gsub("^\\|$","-", res1$critical_effect)
  res1$critical_effect[res1$critical_effect == ""] <- "-"
  res1$critical_effect[which(is.na(res1$critical_effect))] <- "-"
  #####################################################################
  cat("fix duration value and units\n")
  #####################################################################

  res1$exposure_duration <- res1$duration_sampling_time
  res1[grep("\\[Total exposure duration\\]", res1$exposure_duration), "exposure_duration"] <-gsub("\\s+\\[.*\\]","",res1[grep("\\[Total exposure duration\\]", res1$exposure_duration), "exposure_duration"])

  res1$study_duration_value <- gsub("(^\\d+\\.*\\d*)(\\s+.*)", "\\1", res1$exposure_duration)
  res1$study_duration_value <- as.numeric(res1$study_duration_value)
  res1[grep("^\\d+\\.*\\d*\\s+\\w+$",res1$exposure_duration), "study_duration_units"] <- gsub("(.*\\s+)(\\w+)", "\\2", res1[grep("^\\d+\\.*\\d*\\s+\\w+$",res1$exposure_duration), "exposure_duration"])
  res1[grep("^d$",res1$study_duration_units), "study_duration_units"] <- "days"
  res1[grep("^h$",res1$study_duration_units), "study_duration_units"] <- "hours"
  res1[grep("^wk$",res1$study_duration_units), "study_duration_units"] <- "weeks"
  res1[grep("^mo$",res1$study_duration_units), "study_duration_units"] <- "months"
  res1[grep("^min$",res1$study_duration_units), "study_duration_units"] <- "minutes"

  #####################################################################
  cat("subset required fields\n")
  #####################################################################


  res <- res1


  names.list <- c("source_hash","name","casrn","subsource","url","quality","guideline" ,
                  "glp","species","strain","sex","study_duration_value","study_duration_units",
                  "year","toxval_numeric_qualifier","toxval_numeric","toxval_units","generation",
                  "toxval_type","study_type","exposure_route","exposure_method","exposure_form","critical_effect",
                  "strain_original","year_original",
                  "toxval_type_original","study_type_original","exposure_route_original","exposure_method_original","exposure_form_original",
                  "critical_effect_original","study_duration_value_original","study_duration_units_original","document_name")

  res <- res[,(names(res)%in% names.list)]

  #####################################################################
  cat("strip leading and trailing spaces\n")
  #####################################################################


  res <- data.frame(sapply(res, function(x) gsub("(.*)(\\s+$)", "\\1", x)),stringsAsFactors = F)
  res <- data.frame(sapply(res, function(x) gsub("(^\\s+)(.*)", "\\2", x)),stringsAsFactors = F)


  res$year <- as.integer(res$year)
  res$study_duration_value <- as.numeric(res$study_duration_value)
  res$toxval_numeric <- as.numeric(res$toxval_numeric)

  res$toxval_units_original <- "-"
  res$toxval_numeric_qualifier_original <- "-"

  #print(str(res))

  #####################################################################
  cat("assign source_url\n")
  #####################################################################

  res[,"source_url"] <- "https://www.echemportal.org/echemportal/"

  #####################################################################
  cat("checks, finds and replaces non ascii characters in res with XXX\n")
  #####################################################################
  res <- fix.non_ascii(res)


  # trims leading and trailing whitespaces from the dataframe
  res <- data.frame(lapply(res, function(x) if(class(x)=="character") trimws(x) else(x)), stringsAsFactors=F, check.names=F)
  #print(str(res))

  res <- res[!is.na(res[,"casrn"]),]

  #####################################################################
  cat("add other columns to res\n")
  #####################################################################
  res$source <- source
  res$details_text <- "ECHA details"
  res$human_eco <- "human health"
  names(res)[is.element(names(res),"species")] <- "species_original"
  res$species_original <- tolower(res$species_original)
  res[is.na(res[,"document_name"]),"document_name"] <- "-"

  res <- fill.toxval.defaults(toxval.db,res)

  res <- unique(res)


  #####################################################################
  cat("map chemicals\n")
  #####################################################################
  name.list <- c("casrn","name",names(res)[!is.element(names(res),c("casrn","name"))])
  res <- res[,name.list]
  casrn.list <- res[,c("casrn","name")]
  cid.list <- get.cid.list.toxval(toxval.db, casrn.list,source)
  res$chemical_id <- cid.list$chemical_id
  res <- res[,!is.element(names(res),c("casrn","name"))]

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
  refs <- res[,c("toxval_id","source","quality","guideline", "glp","url","year","document_name")]


  #####################################################################
  cat("add extra columns to refs\n")
  #####################################################################
  refs$record_source_type <- "website"
  refs$record_source_note <- "currently, there are URLs for specific records"
  refs$record_source_level <- "primary"
  #####################################################################
  cat("delete unused columns from res\n")
  #####################################################################
  res <- res[,!is.element(names(res),c("quality","guideline","glp","url","document_name"))]

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
