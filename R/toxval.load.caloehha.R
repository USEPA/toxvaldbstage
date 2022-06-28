
#--------------------------------------------------------------------------------------
#' Load new_caloehha from toxval_source to toxval
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param source.db The source database to use.
#' @param verbose If TRUE, print out extra diagnostic messages
#' @export
#--------------------------------------------------------------------------------------
toxval.load.caloehha <- function(toxval.db,source.db ,log=F){
  printCurrentFunction(toxval.db)

  source <- "Cal OEHHA"
  source_table = "source_caloehha"
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

  ####################################################################################
  ####################################################################################
  ####################################################################################
  ####################################################################################
  ####################################################################################
  ####################################################################################
  ####################################################################################
  ####################################################################################
  mat = res
  mat[is.null(mat)] <- NA
  name.list <- c("source_hash","casrn","name","toxval_type","toxval_numeric","toxval_units","chemical_id",
                 "species_original","critical_effect",
                 "risk_assessment_class","year","exposure_route",
                 "study_type","study_duration_value","study_duration_units","record_url","document_name")

  row <- as.data.frame(matrix(nrow=1,ncol=length(name.list)))
  names(row) <- name.list
  res <- NULL
  for(i in 1:nrow(mat)) {
    casrn <- fix.casrn(mat[i,"casrn"])
    name <- mat[i,"name"]
    source_hash <- mat[i,"source_hash"]
    cname <- tolower(name)
    cname <- str_replace_all(cname," ","-")
    if(!is.na(mat[i,"inhalation_unit_risk"])) {
      row[] <- NA
      row[1,"source_hash"] <- source_hash
      row[1,"casrn"] <- casrn
      row[1,"name"] <- name
      row[1,"toxval_numeric"] <- mat[i,"inhalation_unit_risk"]
      row[1,"toxval_units"] <- "(ug/m3)-1"
      row[1,"toxval_type"] <- "inhalation unit risk"
      row[1,"exposure_route"] <- "inhalation"
      row[1,"risk_assessment_class"] <- "chronic"
      row[1,"study_type"] <- "cancer"
      row[1,"year"] <- mat[i,"cancer_potency_year"]
      row[1,"record_url"] <- paste0("https://oehha.ca.gov/chemicals/",cname)
      row[1,"chemical_id"] <- mat[i,"chemical_id"]
      res <- rbind(res,row)
    }

    if(!is.na(mat[i,"inhalation_slope_factor"])) {
      row[] <- NA
      row[1,"source_hash"] <- source_hash
      row[1,"casrn"] <- casrn
      row[1,"name"] <- name
      row[1,"toxval_numeric"] <- mat[i,"inhalation_slope_factor"]
      row[1,"toxval_units"] <- "(mg/kg-day)-1"
      row[1,"toxval_type"] <- "inhalation slope factor"
      row[1,"exposure_route"] <- "inhalation"
      row[1,"risk_assessment_class"] <- "chronic"
      row[1,"study_type"] <- "cancer"
      row[1,"year"] <- mat[i,"cancer_potency_year"]
      row[1,"record_url"] <- paste0("https://oehha.ca.gov/chemicals/",cname)
      row[1,"chemical_id"] <- mat[i,"chemical_id"]
      res <- rbind(res,row)
    }

    if(!is.na(mat[i,"oral_slope_factor"])) {
      row[] <- NA
      row[1,"source_hash"] <- source_hash
      row[1,"casrn"] <- casrn
      row[1,"name"] <- name
      row[1,"toxval_numeric"] <- mat[i,"oral_slope_factor"]
      row[1,"toxval_units"] <- "(mg/kg-day)-1"
      row[1,"toxval_type"] <- "oral unit risk"
      row[1,"exposure_route"] <- "oral"
      row[1,"risk_assessment_class"] <- "chronic"
      row[1,"study_type"] <- "cancer"
      row[1,"year"] <- mat[i,"cancer_potency_year"]
      row[1,"record_url"] <- paste0("https://oehha.ca.gov/chemicals/",cname)
      row[1,"chemical_id"] <- mat[i,"chemical_id"]
      res <- rbind(res,row)
    }

    if(!is.na(mat[i,"acute_rel"])) {
      row[] <- NA
      row[1,"source_hash"] <- source_hash
      row[1,"casrn"] <- casrn
      row[1,"name"] <- name
      row[1,"toxval_numeric"] <- mat[i,"acute_rel"]
      row[1,"toxval_units"] <- "ug/m3"
      row[1,"toxval_type"] <- "REL"
      row[1,"exposure_route"] <- "oral"
      row[1,"risk_assessment_class"] <- "acute"
      row[1,"study_type"] <- "acute"
      row[1,"year"] <- mat[i,"acute_rel_year"]
      row[1,"species_original"] <- tolower(mat[i,"acute_rel_species"])
      row[1,"critical_effect"] <- paste(mat[i,"acute_rel_critical_effect"],"|",mat[i,"acute_rel_target_organ"],"|",mat[i,"acute_rel_severity"])
      row[1,"record_url"] <- paste0("https://oehha.ca.gov/chemicals/",cname)
      row[1,"chemical_id"] <- mat[i,"chemical_id"]
      res <- rbind(res,row)
    }

    if(!is.na(mat[i,"inhalation_rel_8_hour"])) {
      row[] <- NA
      row[1,"source_hash"] <- source_hash
      row[1,"casrn"] <- casrn
      row[1,"name"] <- name
      row[1,"toxval_numeric"] <- mat[i,"inhalation_rel_8_hour"]
      row[1,"toxval_units"] <- "ug/m3"
      row[1,"toxval_type"] <- "REL"
      row[1,"exposure_route"] <- "inhalation"
      row[1,"risk_assessment_class"] <- "acute"
      row[1,"study_type"] <- "acute"
      row[1,"year"] <- mat[i,"inhalation_rel_year"]
      row[1,"study_duration_value"] <- 8
      row[1,"study_duration_units"] <- "hour"
      row[1,"record_url"] <- paste0("https://oehha.ca.gov/chemicals/",cname)
      row[1,"chemical_id"] <- mat[i,"chemical_id"]
      res <- rbind(res,row)
    }

    if(!is.na(mat[i,"chronic_inhalation_rel"])) {
      row[] <- NA
      row[1,"source_hash"] <- source_hash
      row[1,"casrn"] <- casrn
      row[1,"name"] <- name
      row[1,"toxval_numeric"] <- mat[i,"chronic_inhalation_rel"]
      row[1,"toxval_units"] <- "ug/m3"
      row[1,"toxval_type"] <- "REL"
      row[1,"exposure_route"] <- "inhalation"
      row[1,"risk_assessment_class"] <- "chronic"
      row[1,"study_type"] <- "chronic"
      row[1,"year"] <- mat[i,"inhalation_rel_year"]
      row[1,"critical_effect"] <- paste( mat[i,"chronic_inhalation_critical_effect"],"|", mat[i,"chronic_inhalation_target_organ"])
      row[1,"record_url"] <- paste0("https://oehha.ca.gov/chemicals/",cname)
      row[1,"chemical_id"] <- mat[i,"chemical_id"]
      res <- rbind(res,row)
    }

    if(!is.na(mat[i,"mcl"])) {
      row[] <- NA
      row[1,"source_hash"] <- source_hash
      row[1,"casrn"] <- casrn
      row[1,"name"] <- name
      row[1,"toxval_numeric"] <- mat[i,"mcl"]
      row[1,"toxval_units"] <- "mg/L"
      row[1,"toxval_type"] <- "MCL"
      row[1,"exposure_route"] <- "oral"
      row[1,"risk_assessment_class"] <- "chronic"
      row[1,"study_type"] <- "chronic"
      row[1,"record_url"] <- paste0("https://oehha.ca.gov/chemicals/",cname)
      row[1,"chemical_id"] <- mat[i,"chemical_id"]
      res <- rbind(res,row)
    }
  }
  res <- generate.originals(toxval.db,res)

  ##########################################################
  cat("Convert multiple date formats present in year field to the corresponding year value,
      then change the data type from character to integer \n ")
  ###########################################################
  res1 <- res
  date_fix <- excel_numeric_to_date(as.numeric(as.character(res1[grep("[0-9]{5}", res1$year),'year'])), date_system = "modern")
  date_fix <- format(date_fix, format = "%Y")

  res1[grep("[0-9]{5}", res1$year),'year'] <- date_fix
  res1[grep("[a-zA-Z]+", res1$year),'year'] <- gsub(".*\\,\\s+(\\d{4})", "\\1", grep("[a-zA-Z]+", res1$year,value= T))
  res1[which(res1$year == "-"), "year"] <- NA
  res1$year <- as.integer(res1$year)

  ###########################################################
  cat("seperate multiple casrns \n")
  ###########################################################
  res1 <- separate_rows(res1, casrn, sep = ";")
  res1$casrn <- gsub("^\\s+","", res1$casrn)

  ###########################################################
  cat("seperate multiple toxval_numeric entries
      , convert character data type of toxval_numeric to numeric data type \n")
  ###########################################################
  # convert values expressed with E to numeric
  tox_num <- grep("E",res1$toxval_numeric)

  for (i in 1:length(tox_num)){
    res1[tox_num[i], "toxval_numeric"] <- tolower(res1[tox_num[i], "toxval_numeric"])
    res1[tox_num[i], "toxval_numeric"] <- gsub("(^\\d+\\.*\\d*)(\\s*)(e.*)","\\1\\3",res1[tox_num[i], "toxval_numeric"])
  }

  # seperate rows with multiple numeric values seperated by ;
  res1 <- separate_rows(res1, toxval_numeric, sep = ";")
  res1$toxval_numeric <- gsub("^\\s+","", res1$toxval_numeric)

  # extract numeric and unit values from 1.4e-13 (fibers/l water)^-1
  res1[grep("\\(.*\\/.*\\)", res1$toxval_numeric),'toxval_units']<- gsub(".*\\((.*\\/.*)\\)", "\\1",grep("\\(.*\\/.*\\)", res1$toxval_numeric, value= T))
  res1[grep("\\(.*\\/.*\\)", res1$toxval_numeric),'toxval_numeric'] <- gsub("\\((.*\\/.*)\\).*", "", grep("\\(.*\\/.*\\)", res1$toxval_numeric, value= T))

  # oral route values in toxval numeric with unit values attached
  oral_route <- grep("oral", res1$toxval_numeric)
  for (i in 1:length(oral_route)){
    res1[oral_route[i],"exposure_route_original"] <- res1[oral_route[i], "toxval_numeric"]
  }

  for (i in 1:length(oral_route)){
    res1[oral_route[i], "toxval_units"] <- gsub("(.*\\d+\\s+)(.*)(\\s+\\(.*\\))","\\2",res1[oral_route[i], "toxval_numeric"])
  }

  # extract unit, exposure route values from toxval numeric
  route_val <- grep("\\(.*\\)", res1$toxval_numeric)

  for ( i in 1:length(route_val)){
    res1[route_val[i],"exposure_route_original"] <- res1[route_val[i], "toxval_numeric"]
  }

  for ( i in 1:length(route_val)){
    res1[route_val[i],'exposure_route']  <- gsub(".*\\((.*)\\)", "\\1",res1[route_val[i],'toxval_numeric'])
    res1[route_val[i],'toxval_numeric'] <- gsub("(.*\\d+)(\\s+.*)(\\(.*\\))", "\\1",res1[route_val[i],'toxval_numeric'])
  }

  # print(View(res1))
  # unit values with numeric values pCi/L and million fibers/L
  tox_units <- grep("[A-Z]+", res1$toxval_numeric)

  for (i in 1:length(tox_units)){
    res1[tox_units[i], "toxval_units"] <- gsub("(.*\\d+\\s+)(.*)","\\2",res1[tox_units[i], "toxval_numeric"])
    res1[tox_units[i], "toxval_numeric"] <- gsub("(.*\\d+)(\\s+.*)","\\1",res1[tox_units[i], "toxval_numeric"])
  }

  # comma value
  res1[grep("\\,", res1$toxval_numeric), "toxval_numeric"] <- gsub("\\,", "", res1[grep("\\,", res1$toxval_numeric), "toxval_numeric"])
  # values with none, --
  none_val <- grep("[0-9]+",res1$toxval_numeric, invert = T)

  for (i in 1:length(none_val)){
    res1[none_val[i],"toxval_numeric"] <- gsub(paste0(unique(res1[none_val[i],"toxval_numeric"]), "|", collapse = "" ), "", res1[none_val[i],"toxval_numeric"])
  }

  # values with 10 as nitrogen, 0.05 for total chromium
  char_vals <- grep("[a-z]+\\s+[a-z]+",res1$toxval_numeric)

  #print(data.frame(res1[char_vals, "toxval_numeric"]))
  for (i in 1:length(char_vals)){
    res1[char_vals[i], "toxval_units_original"] <- res1[char_vals[i], "toxval_numeric"]
    res1[char_vals[i], "toxval_numeric"] <- ""
    res1[char_vals[i], "toxval_units"] <- "-"
  }
  res1$toxval_numeric <-  gsub("^\\s+|\\s+$", "", res1$toxval_numeric)
  res1$toxval_numeric <- as.numeric(res1$toxval_numeric)
  print(names(res1))
  res <- res1[,(names(res1) %in% c("source_hash","casrn","name","toxval_type","toxval_numeric","toxval_units","species_original","critical_effect",
                                   "risk_assessment_class","year","exposure_route","study_type","study_duration_value", "study_duration_units",
                                   "record_url","toxval_type_original","toxval_numeric_original","toxval_units_original","critical_effect_original",
                                   "year_original","exposure_route_original","study_type_original","study_duration_value_original",
                                   "study_duration_units_original","document_name","chemical_id"))]

  # clean critical values with -| and |- patterns formed by pasting critical effect with organs for certain study types
  clean_effect <- grep("^\\-\\s+\\|\\s+", res$critical_effect)
  for (i in 1:length(clean_effect)){
    res[clean_effect[i],"critical_effect"] <- gsub("(\\-\\s+\\|)+","",res[clean_effect[i],"critical_effect"])
  }
  clean_effect_reverse <- grep("\\s+\\|\\s+\\-", res$critical_effect)
  for (i in 1:length(clean_effect_reverse)){
    res[clean_effect_reverse[i],"critical_effect"] <- gsub("(\\s+\\|\\s+\\-)+","",res[clean_effect_reverse[i],"critical_effect"])
  }
  res$critical_effect <- gsub("^\\s+|\\s+$","", res$critical_effect)
  print(dim(res))
  # trims leading and trailing whitespaces from the dataframe
  res <- data.frame(lapply(res, function(x) if(class(x)=="character") trimws(x) else(x)), stringsAsFactors=F, check.names=F)

  res <- res[!is.na(res[,"casrn"]),]
  #print(View(res))
  #####################################################################
  cat("add other columns to res\n")
  #####################################################################
  res$source <- source
  res$human_eco <- "human health"
  res$subsource <- "California DPH"
  res$details_text = "Cal OEHHA Details"
  res$source_url = "https://oehha.ca.gov/chemicals"
  res$toxval_numeric_qualifier = "="
  res <- res[!is.na(res[,"casrn"]),]
  res <- fill.toxval.defaults(toxval.db,res)

  res <- unique(res)
  res <- res[res$toxval_numeric != "-999",]
  res <- unique(res)
  res$toxval_numeric_original <- res$toxval_numeric
  ####################################################################################
  ####################################################################################
  ####################################################################################
  ####################################################################################
  ####################################################################################
  ####################################################################################
  ####################################################################################
  ####################################################################################
  res = res[!is.na(res$casrn),]

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
  refs$record_source_type = "website"
  refs$record_source_note = "to be cleaned up"
  refs$record_source_level = "primary (risk assessment values)"
  print(dim(res))

  #####################################################################
  cat("load res and refs to the database\n")
  #####################################################################
  res = unique(res)
  refs = unique(refs)
  res$datestamp = Sys.Date()
  res$source_table = source_table
  res$source_url = "https://oehha.ca.gov/chemicals"
  res$subsource_url = "-"
  res$details_text = paste(source,"Details")
  for(i in 1:nrow(res)) res[i,"toxval_uuid"] = UUIDgenerate()
  for(i in 1:nrow(refs)) refs[i,"record_source_uuid"] = UUIDgenerate()
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






  #####################################################################
  cat("start output log, log files for each source can be accessed from output_log folder\n")
  #####################################################################

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

  query <- "select * from original_caloehha_table"

  mat <- runQuery(query,source.db,T,F)
  mat <- mat[ , !(names(mat) %in% c("source_id","clowder_id"))]
  #####################################################################
  cat("checks, finds and replaces non ascii characters in mat with XXX\n")
  #####################################################################
  mat <- fix.non_ascii(mat)


  mat[is.null(mat)] <- NA
  name.list <- c("source_hash","casrn","name","toxval_type","toxval_numeric","toxval_units",
                 "species_original","critical_effect",
                 "risk_assessment_class","year","exposure_route",
                 "study_type","study_duration_value","study_duration_units","record_url","document_name")


  row <- as.data.frame(matrix(nrow=1,ncol=length(name.list)))
  names(row) <- name.list
  res <- NULL
  for(i in 1:nrow(mat)) {

    casrn <- fix.casrn(mat[i,"casrn"])
    name <- mat[i,"name"]
    source_hash <- mat[i,"source_hash"]
    cname <- tolower(name)
    cname <- str_replace_all(cname," ","-")
    if(!is.na(mat[i,"inhalation_unit_risk_(ug/m3)-1"])) {
      row[] <- NA
      row[1,"source_hash"] <- source_hash
      row[1,"casrn"] <- casrn
      row[1,"name"] <- name
      row[1,"toxval_numeric"] <- mat[i,"inhalation_unit_risk_(ug/m3)-1"]
      row[1,"toxval_units"] <- "(ug/m3)-1"
      row[1,"toxval_type"] <- "inhalation unit risk"
      row[1,"exposure_route"] <- "inhalation"
      row[1,"risk_assessment_class"] <- "chronic"
      row[1,"study_type"] <- "cancer"
      row[1,"year"] <- mat[i,"cancer_potency_year"]
      row[1,"record_url"] <- paste0("https://oehha.ca.gov/chemicals/",cname)
      res <- rbind(res,row)
    }

    if(!is.na(mat[i,"inhalation_slope_factor_(mg/kg-day)-1"])) {
      row[] <- NA
      row[1,"source_hash"] <- source_hash
      row[1,"casrn"] <- casrn
      row[1,"name"] <- name
      row[1,"toxval_numeric"] <- mat[i,"inhalation_slope_factor_(mg/kg-day)-1"]
      row[1,"toxval_units"] <- "(mg/kg-day)-1"
      row[1,"toxval_type"] <- "inhalation slope factor"
      row[1,"exposure_route"] <- "inhalation"
      row[1,"risk_assessment_class"] <- "chronic"
      row[1,"study_type"] <- "cancer"
      row[1,"year"] <- mat[i,"cancer_potency_year"]
      row[1,"record_url"] <- paste0("https://oehha.ca.gov/chemicals/",cname)
      res <- rbind(res,row)
    }

    if(!is.na(mat[i,"oral_slope_factor_(mg/kg-day)-1"])) {
      row[] <- NA
      row[1,"source_hash"] <- source_hash
      row[1,"casrn"] <- casrn
      row[1,"name"] <- name
      row[1,"toxval_numeric"] <- mat[i,"oral_slope_factor_(mg/kg-day)-1"]
      row[1,"toxval_units"] <- "(mg/kg-day)-1"
      row[1,"toxval_type"] <- "oral unit risk"
      row[1,"exposure_route"] <- "oral"
      row[1,"risk_assessment_class"] <- "chronic"
      row[1,"study_type"] <- "cancer"
      row[1,"year"] <- mat[i,"cancer_potency_year"]
      row[1,"record_url"] <- paste0("https://oehha.ca.gov/chemicals/",cname)
      res <- rbind(res,row)
    }

    if(!is.na(mat[i,"acute_rel_ug/m3"])) {
      row[] <- NA
      row[1,"source_hash"] <- source_hash
      row[1,"casrn"] <- casrn
      row[1,"name"] <- name
      row[1,"toxval_numeric"] <- mat[i,"acute_rel_ug/m3"]
      row[1,"toxval_units"] <- "ug/m3"
      row[1,"toxval_type"] <- "REL"
      row[1,"exposure_route"] <- "oral"
      row[1,"risk_assessment_class"] <- "acute"
      row[1,"study_type"] <- "acute"
      row[1,"year"] <- mat[i,"acute_rel_year"]
      row[1,"species_original"] <- tolower(mat[i,"acute_rel_species"])
      row[1,"critical_effect"] <- paste(mat[i,"acute_rel_critical_effect"],"|",mat[i,"acute_rel_target_organ"],"|",mat[i,"acute_rel_severity"])
      row[1,"record_url"] <- paste0("https://oehha.ca.gov/chemicals/",cname)
      res <- rbind(res,row)
    }

    if(!is.na(mat[i,"inhalation_rel_8_hour_ug/m3"])) {
      row[] <- NA
      row[1,"source_hash"] <- source_hash
      row[1,"casrn"] <- casrn
      row[1,"name"] <- name
      row[1,"toxval_numeric"] <- mat[i,"inhalation_rel_8_hour_ug/m3"]
      row[1,"toxval_units"] <- "ug/m3"
      row[1,"toxval_type"] <- "REL"
      row[1,"exposure_route"] <- "inhalation"
      row[1,"risk_assessment_class"] <- "acute"
      row[1,"study_type"] <- "acute"
      row[1,"year"] <- mat[i,"inhalation_rel_year"]
      row[1,"study_duration_value"] <- 8
      row[1,"study_duration_units"] <- "hour"
      row[1,"record_url"] <- paste0("https://oehha.ca.gov/chemicals/",cname)
      res <- rbind(res,row)
    }

    if(!is.na(mat[i,"chronic_inhalation_rel_ug/m3"])) {
      row[] <- NA
      row[1,"source_hash"] <- source_hash
      row[1,"casrn"] <- casrn
      row[1,"name"] <- name
      row[1,"toxval_numeric"] <- mat[i,"chronic_inhalation_rel_ug/m3"]
      row[1,"toxval_units"] <- "ug/m3"
      row[1,"toxval_type"] <- "REL"
      row[1,"exposure_route"] <- "inhalation"
      row[1,"risk_assessment_class"] <- "chronic"
      row[1,"study_type"] <- "chronic"
      row[1,"year"] <- mat[i,"inhalation_rel_year"]
      row[1,"critical_effect"] <- paste( mat[i,"chronic_inhalation_critical_effect"],"|", mat[i,"chronic_inhalation_target_organ"])
      row[1,"record_url"] <- paste0("https://oehha.ca.gov/chemicals/",cname)
      res <- rbind(res,row)
    }

    if(!is.na(mat[i,"mcl_mg/L"])) {
      row[] <- NA
      row[1,"source_hash"] <- source_hash
      row[1,"casrn"] <- casrn
      row[1,"name"] <- name
      row[1,"toxval_numeric"] <- mat[i,"mcl_mg/L"]
      row[1,"toxval_units"] <- "mg/L"
      row[1,"toxval_type"] <- "MCL"
      row[1,"exposure_route"] <- "oral"
      row[1,"risk_assessment_class"] <- "chronic"
      row[1,"study_type"] <- "chronic"
      row[1,"record_url"] <- paste0("https://oehha.ca.gov/chemicals/",cname)
      res <- rbind(res,row)
    }

  }


  res <- generate.originals(toxval.db,res)

  ##########################################################
  cat("Convert multiple date formats present in year field to the corresponding year value,
      then change the data type from character to integer \n ")
  ###########################################################
  res1 <- res
  date_fix <- excel_numeric_to_date(as.numeric(as.character(res1[grep("[0-9]{5}", res1$year),'year'])), date_system = "modern")
  date_fix <- format(date_fix, format = "%Y")

  res1[grep("[0-9]{5}", res1$year),'year'] <- date_fix
  res1[grep("[a-zA-Z]+", res1$year),'year'] <- gsub(".*\\,\\s+(\\d{4})", "\\1", grep("[a-zA-Z]+", res1$year,value= T))
  res1[which(res1$year == "-"), "year"] <- NA
  res1$year <- as.integer(res1$year)

  ###########################################################
  cat("seperate multiple casrns \n")
  ###########################################################

  res1 <- separate_rows(res1, casrn, sep = ";")
  res1$casrn <- gsub("^\\s+","", res1$casrn)

  ###########################################################
  cat("seperate multiple toxval_numeric entries
      , convert character data type of toxval_numeric to numeric data type \n")
  ###########################################################


  # convert values expressed with E to numeric
  tox_num <- grep("E",res1$toxval_numeric)

  for (i in 1:length(tox_num)){
    res1[tox_num[i], "toxval_numeric"] <- tolower(res1[tox_num[i], "toxval_numeric"])
    res1[tox_num[i], "toxval_numeric"] <- gsub("(^\\d+\\.*\\d*)(\\s*)(e.*)","\\1\\3",res1[tox_num[i], "toxval_numeric"])
  }

  # seperate rows with multiple numeric values seperated by ;
  res1 <- separate_rows(res1, toxval_numeric, sep = ";")
  res1$toxval_numeric <- gsub("^\\s+","", res1$toxval_numeric)

  # extract numeric and unit values from 1.4e-13 (fibers/l water)^-1
  res1[grep("\\(.*\\/.*\\)", res1$toxval_numeric),'toxval_units']<- gsub(".*\\((.*\\/.*)\\)", "\\1",grep("\\(.*\\/.*\\)", res1$toxval_numeric, value= T))
  res1[grep("\\(.*\\/.*\\)", res1$toxval_numeric),'toxval_numeric'] <- gsub("\\((.*\\/.*)\\).*", "", grep("\\(.*\\/.*\\)", res1$toxval_numeric, value= T))


  # oral route values in toxval numeric with unit values attached
  oral_route <- grep("oral", res1$toxval_numeric)
  for (i in 1:length(oral_route)){
    res1[oral_route[i],"exposure_route_original"] <- res1[oral_route[i], "toxval_numeric"]
  }

  for (i in 1:length(oral_route)){
    res1[oral_route[i], "toxval_units"] <- gsub("(.*\\d+\\s+)(.*)(\\s+\\(.*\\))","\\2",res1[oral_route[i], "toxval_numeric"])
  }

  # extract unit, exposure route values from toxval numeric
  route_val <- grep("\\(.*\\)", res1$toxval_numeric)

  for ( i in 1:length(route_val)){
    res1[route_val[i],"exposure_route_original"] <- res1[route_val[i], "toxval_numeric"]
  }

  for ( i in 1:length(route_val)){
    res1[route_val[i],'exposure_route']  <- gsub(".*\\((.*)\\)", "\\1",res1[route_val[i],'toxval_numeric'])

    res1[route_val[i],'toxval_numeric'] <- gsub("(.*\\d+)(\\s+.*)(\\(.*\\))", "\\1",res1[route_val[i],'toxval_numeric'])


  }

  # print(View(res1))
  # unit values with numeric values pCi/L and million fibers/L
  tox_units <- grep("[A-Z]+", res1$toxval_numeric)


  for (i in 1:length(tox_units)){
    res1[tox_units[i], "toxval_units"] <- gsub("(.*\\d+\\s+)(.*)","\\2",res1[tox_units[i], "toxval_numeric"])
    res1[tox_units[i], "toxval_numeric"] <- gsub("(.*\\d+)(\\s+.*)","\\1",res1[tox_units[i], "toxval_numeric"])
  }


  # comma value
  res1[grep("\\,", res1$toxval_numeric), "toxval_numeric"] <- gsub("\\,", "", res1[grep("\\,", res1$toxval_numeric), "toxval_numeric"])

  # values with none, --
  none_val <- grep("[0-9]+",res1$toxval_numeric, invert = T)


  for (i in 1:length(none_val)){
    res1[none_val[i],"toxval_numeric"] <- gsub(paste0(unique(res1[none_val[i],"toxval_numeric"]), "|", collapse = "" ), "", res1[none_val[i],"toxval_numeric"])

  }


  # values with 10 as nitrogen, 0.05 for total chromium
  char_vals <- grep("[a-z]+\\s+[a-z]+",res1$toxval_numeric)

  #print(data.frame(res1[char_vals, "toxval_numeric"]))

  for (i in 1:length(char_vals)){
    res1[char_vals[i], "toxval_units_original"] <- res1[char_vals[i], "toxval_numeric"]
    res1[char_vals[i], "toxval_numeric"] <- ""
    res1[char_vals[i], "toxval_units"] <- "-"

  }




  res1$toxval_numeric <-  gsub("^\\s+|\\s+$", "", res1$toxval_numeric)
  res1$toxval_numeric <- as.numeric(res1$toxval_numeric)


  print(names(res1))
  res <- res1[,(names(res1) %in% c("source_hash","casrn","name","toxval_type","toxval_numeric","toxval_units","species_original","critical_effect",
                                   "risk_assessment_class","year","exposure_route","study_type","study_duration_value", "study_duration_units",
                                   "record_url","toxval_type_original","toxval_numeric_original","toxval_units_original","critical_effect_original",
                                   "year_original","exposure_route_original","study_type_original","study_duration_value_original","study_duration_units_original","document_name"))]



  # clean critical values with -| and |- patterns formed by pasting critical effect with organs for certain study types

  clean_effect <- grep("^\\-\\s+\\|\\s+", res$critical_effect)

  for (i in 1:length(clean_effect)){
    res[clean_effect[i],"critical_effect"] <- gsub("(\\-\\s+\\|)+","",res[clean_effect[i],"critical_effect"])
  }

  clean_effect_reverse <- grep("\\s+\\|\\s+\\-", res$critical_effect)

  for (i in 1:length(clean_effect_reverse)){
    res[clean_effect_reverse[i],"critical_effect"] <- gsub("(\\s+\\|\\s+\\-)+","",res[clean_effect_reverse[i],"critical_effect"])
  }

  res$critical_effect <- gsub("^\\s+|\\s+$","", res$critical_effect)



  print(dim(res))
  # trims leading and trailing whitespaces from the dataframe
  res <- data.frame(lapply(res, function(x) if(class(x)=="character") trimws(x) else(x)), stringsAsFactors=F, check.names=F)

  res <- res[!is.na(res[,"casrn"]),]
  #print(View(res))
  #####################################################################
  cat("add other columns to res\n")
  #####################################################################
  res$source <- source
  res$human_eco <- "human health"
  res$subsource <- "California DPH"
  res$details_text = "Cal OEHHA Details"
  res$source_url = "https://oehha.ca.gov/chemicals"
  res$toxval_numeric_qualifier = "="
  res <- res[!is.na(res[,"casrn"]),]
  res <- fill.toxval.defaults(toxval.db,res)

  res <- unique(res)
  res <- res[res$toxval_numeric != "-999",]
  res <- unique(res)
  res$toxval_numeric_original <- res$toxval_numeric
  res[is.na(res[,"document_name"]),"document_name"] <- "-"

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
  refs <- res[,c("toxval_id","record_url","source","year","document_name")]
  names(refs) <- c("toxval_id","url","source","year","document_name")

  #####################################################################
  cat("add extra columns to refs\n")
  #####################################################################
  refs$record_source_type <- "url"
  refs$record_source_note <- "Risk assessment values determined by Cal OEHHA"
  refs$record_source_level <- "primary (risk assessment values)"


  #####################################################################
  cat("delete unused columns from res\n")
  #####################################################################
  res <- res[,!is.element(names(res),c("record_url","document_name"))]

  #####################################################################
  cat("load res and refs to the database\n")
  #####################################################################
  res <- unique(res)
  refs <- unique(refs)
  res$datestamp <- Sys.Date()
  for(i in 1:nrow(res)) res[i,"toxval_uuid"] <- UUIDgenerate()
  for(i in 1:nrow(refs)) refs[i,"record_source_uuid"] <- UUIDgenerate()

  print(dim(res))
  print(dim(refs))

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

