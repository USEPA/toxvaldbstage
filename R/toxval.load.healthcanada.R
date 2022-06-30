
#-------------------------------------------------------------------------------------
#' Load new_health_canada_table from toxval_source to toxval
#'
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param source.db The version of toxval_source from which the tables are loaded.
#' @param verbose If TRUE, print out extra diagnostic messages
#' @export
#--------------------------------------------------------------------------------------
toxval.load.healthcanada <- function(toxval.db,source.db,log=F) {
  printCurrentFunction(toxval.db)
  source <- "Health Canada"
  source_table = "source_health_canada"
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
  names(res)[names(res)=="trv_source"] <- "long_ref"


  #res1$duration <- enc2utf8(res1$duration)
  #####################################################################
  cat("checks, finds and replaces non ascii characters in res with XXX\n")
  #####################################################################
  res1 <- res
  res1 <- as.data.frame(apply(res1, 2, function(y) as.character(gsub("µ", "u", y))),stringsAsFactors = F)
  res1 <- as.data.frame(apply(res1, 2, function(y) as.character(gsub("[³]", "3", y))),stringsAsFactors = F)
  res1$study_duration_value <- res1$duration
  res1[grep(".*60 years$",res1$study_duration_value),"study_duration_value"] <- "<= 60 years"
  res1[grep(".*12 months occupational exposure$",res1$study_duration_value),"study_duration_value"] <- ">= 12 months occupational exposure"
  replacement_value <- c("days 1-24 (rabbits) and 1-19 (rats) of gestational period","Duration and Dosing Regime: single bolus dose (0, 12.5, 50, 200, or 800 ng 2,3,7,8-TCDD)/kg-bw) on day 15 of gestation (Oshako et al., 2001); subcutaneous loading dose 25, 60, or 300 ng TCDD/kgbw) followed by weekly maintenance doses (5, 12, or 60 ng TCDD/kgbw) beginning 2 weeks prior to mating, and continuing through mating, gestation and lactation (Faqi et al., 1998)",
                         "F0: prior to and during mating (males and females) and throughout gestation lactation; F1: from weaning through reproduction until weaning of F2 pups","gestational days 0-17","nd","chronic","1, 2, 6, 8 weeks (various groups)","4 and 8 months","105 to 107 weeks","1 gestational period, 48 d post-natal exposure","3 dosing regimes: for 3 months before pregnancy, for 2 months before and 21 d during pregnancy, or for 21 d during pregnancy only",
                         "Duration and Dosing Regime: single bolus dose (0, 12.5, 50, 200, or 800 ng 2,3,7,8-TCDD)/kg-bw) on day 15 of gestation (Oshako et al., 2001); subcutaneous loading dose 25, 60, or 300 ng TCDD/kgbw) followed by weekly maintenance doses (5, 12, or 60 ng TCDD")
  new_replacement_value <- c(19,15,2,17,0,0,8,8,107,48,3,15)
  new_replacement_unit <- c("days","days","generations","days","-","-", "weeks","months","weeks","days","months","days")
  replacement_table <- data.frame(replacement_value, new_replacement_value, new_replacement_unit, stringsAsFactors = F)

  for (i in 1:length(replacement_table$replacement_value)){
    for (k in 1:nrow(res1)){
      res1$study_duration_value[k][res1$duration[k] %in% replacement_table$replacement_value[[i]]] <- replacement_table$new_replacement_value[i]
    }
  }

  res1$study_duration_value <-  gsub("^\\D+(\\d.*)", "\\1", res1$study_duration_value)
  res1$study_duration_units <- word(res1$study_duration_value,2)
  res1$study_duration_value <- word(res1$study_duration_value,1)
  num_alpha_val <- grep("[0-9]+[a-zA-Z]+", res1$study_duration_value, value = T)
  num_alpha_unit <- gsub("\\d","",num_alpha_val)
  num_alpha_vals <- gsub("[a-zA-Z]","",num_alpha_val)
  res1$study_duration_value[res1$duration %in% num_alpha_val] <- num_alpha_vals
  res1$study_duration_units[res1$duration %in% num_alpha_val] <- num_alpha_unit

  for (i in 1:length(replacement_table$replacement_value)){
    for (k in 1:nrow(res1)){
      res1$study_duration_units[k][res1$duration[k] %in% replacement_table$replacement_value[[i]]] <- replacement_table$new_replacement_unit[i]
    }
  }

  res1$study_duration_value <- gsub(".*-", "", res1$study_duration_value)
  res1$study_duration_units <- gsub("\\bd\\b","days", res1$study_duration_units)
  res1$study_duration_units <- gsub("\\bh\\b","hours", res1$study_duration_units)
  res1$study_duration_units <- gsub("\\bwk\\b","weeks", res1$study_duration_units)
  res1$study_duration_units <- gsub("\\byear\\b","years", res1$study_duration_units)
  res1$study_duration_units <- gsub("\\,|\\;|\\s+|\\)", "",res1$study_duration_units)
  res1$study_duration_value <-  as.numeric(res1$study_duration_value)
  res1$toxval_numeric <- as.numeric(res1$toxval_numeric)
  res1$study_duration_qualifier <- res1$duration
  res1$study_duration_qualifier <- gsub("[a-zA-Z0-9.,\\+() ;:/-]", "",res1$study_duration_qualifier)
  names(res1) <- tolower(names(res1))

  exp_val <- grep(".*\\:",res1$exposure_route)
  for (i in 1:length(exp_val)){
    res1[exp_val[i],"exposure_method_original"] <- res1[exp_val[i],"exposure_route"]
    res1[exp_val[i],"exposure_route_original"] <- res1[exp_val[i],"exposure_route"]
    res1[exp_val[i],"exposure_method"] <- gsub("(.*\\:)(.*)","\\2",res1[exp_val[i],"exposure_route"])
    res1[exp_val[i],"exposure_method"] <- gsub("^\\s+","",res1[exp_val[i],"exposure_method"])
    res1[exp_val[i],"exposure_route"] <- gsub("(.*\\:)(.*)","\\1",res1[exp_val[i],"exposure_route"])
    res1[exp_val[i],"exposure_route"] <- gsub("\\:","",res1[exp_val[i],"exposure_route"])
    res1[exp_val[i],"exposure_route"] <- gsub("\\s+$","",res1[exp_val[i],"exposure_route"])
  }

  open_paranthesis_effect <- which(str_count(res1$critical_effect,"\\)") == 0 & str_count(res1$critical_effect,"\\(") == 1)
  open_paranthesis_effect2 <- grep("[^\\)]$",res1[which(str_count(res1$critical_effect,"\\)") == 0 & str_count(res1$critical_effect,"\\(") == 1),"critical_effect"])
  critical_effect_to_clean <- open_paranthesis_effect[open_paranthesis_effect2]
  res1[critical_effect_to_clean,"critical_effect"] <- gsub("\\(","",res1[critical_effect_to_clean,"critical_effect"])
  open_paranthesis_effect <-which(str_count(res1$critical_effect,"\\(") == 6)
  res1[open_paranthesis_effect,"critical_effect"] <- gsub("(.*\\(.*\\)\\;\\s+)(\\()(.*)","\\1\\3",res1[open_paranthesis_effect,"critical_effect"])
  res <- res1
  cremove = c("row_id","study_id","study_reference","dosing_regime","duration",
            "uncertainty_factors","threshold_endpoint","trv_derivation","cancer_class","study_duration_qualifier")
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
  res$source_url = "source_url"
  res$subsource_url = "subsource_url"
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
  source <- "Health Canada"

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


  query <- "select * from original_health_canada_table"

  res <- runQuery(query,source.db,T,F)
  res <- res[ , !(names(res) %in% c("source_id","clowder_id"))]
  res$study_duration_value <- res$duration
  res$study_duration_units <- res$duration
  res$exposure_method <- res$exposure_route

  res <- generate.originals(toxval.db,res)


  names(res)[names(res)=="trv_source"] <- "long_ref"


  #res1$duration <- enc2utf8(res1$duration)
  #####################################################################
  cat("checks, finds and replaces non ascii characters in res with XXX\n")
  #####################################################################
  res <- fix.non_ascii(res)
  res1 <- res
  res1 <- as.data.frame(apply(res1, 2, function(y) as.character(gsub("µ", "u", y))),stringsAsFactors = F)
  res1 <- as.data.frame(apply(res1, 2, function(y) as.character(gsub("[³]", "3", y))),stringsAsFactors = F)
  res1$study_duration_value <- res1$duration
  res1[grep(".*60 years$",res1$study_duration_value),"study_duration_value"] <- "<= 60 years"
  res1[grep(".*12 months occupational exposure$",res1$study_duration_value),"study_duration_value"] <- ">= 12 months occupational exposure"
  replacement_value <- c("days 1-24 (rabbits) and 1-19 (rats) of gestational period","Duration and Dosing Regime: single bolus dose (0, 12.5, 50, 200, or 800 ng 2,3,7,8-TCDD)/kg-bw) on day 15 of gestation (Oshako et al., 2001); subcutaneous loading dose 25, 60, or 300 ng TCDD/kgbw) followed by weekly maintenance doses (5, 12, or 60 ng TCDD/kgbw) beginning 2 weeks prior to mating, and continuing through mating, gestation and lactation (Faqi et al., 1998)",
                         "F0: prior to and during mating (males and females) and throughout gestation lactation; F1: from weaning through reproduction until weaning of F2 pups","gestational days 0-17","nd","chronic","1, 2, 6, 8 weeks (various groups)","4 and 8 months","105 to 107 weeks","1 gestational period, 48 d post-natal exposure","3 dosing regimes: for 3 months before pregnancy, for 2 months before and 21 d during pregnancy, or for 21 d during pregnancy only",
                         "Duration and Dosing Regime: single bolus dose (0, 12.5, 50, 200, or 800 ng 2,3,7,8-TCDD)/kg-bw) on day 15 of gestation (Oshako et al., 2001); subcutaneous loading dose 25, 60, or 300 ng TCDD/kgbw) followed by weekly maintenance doses (5, 12, or 60 ng TCDD")
  new_replacement_value <- c(19,15,2,17,0,0,8,8,107,48,3,15)
  new_replacement_unit <- c("days","days","generations","days","-","-", "weeks","months","weeks","days","months","days")
  replacement_table <- data.frame(replacement_value, new_replacement_value, new_replacement_unit, stringsAsFactors = F)


  for (i in 1:length(replacement_table$replacement_value)){
    for (k in 1:nrow(res1)){
      res1$study_duration_value[k][res1$duration[k] %in% replacement_table$replacement_value[[i]]] <- replacement_table$new_replacement_value[i]
    }
  }


  res1$study_duration_value <-  gsub("^\\D+(\\d.*)", "\\1", res1$study_duration_value)
  res1$study_duration_units <- word(res1$study_duration_value,2)
  res1$study_duration_value <- word(res1$study_duration_value,1)
  num_alpha_val <- grep("[0-9]+[a-zA-Z]+", res1$study_duration_value, value = T)
  num_alpha_unit <- gsub("\\d","",num_alpha_val)
  num_alpha_vals <- gsub("[a-zA-Z]","",num_alpha_val)
  res1$study_duration_value[res1$duration %in% num_alpha_val] <- num_alpha_vals
  res1$study_duration_units[res1$duration %in% num_alpha_val] <- num_alpha_unit

  for (i in 1:length(replacement_table$replacement_value)){
    for (k in 1:nrow(res1)){
      res1$study_duration_units[k][res1$duration[k] %in% replacement_table$replacement_value[[i]]] <- replacement_table$new_replacement_unit[i]
    }
  }


  res1$study_duration_value <- gsub(".*-", "", res1$study_duration_value)
  res1$study_duration_units <- gsub("\\bd\\b","days", res1$study_duration_units)
  res1$study_duration_units <- gsub("\\bh\\b","hours", res1$study_duration_units)
  res1$study_duration_units <- gsub("\\bwk\\b","weeks", res1$study_duration_units)
  res1$study_duration_units <- gsub("\\byear\\b","years", res1$study_duration_units)
  res1$study_duration_units <- gsub("\\,|\\;|\\s+|\\)", "",res1$study_duration_units)


  res1$study_duration_value <-  as.numeric(res1$study_duration_value)

  res1$toxval_numeric <- as.numeric(res1$toxval_numeric)

  res1$study_duration_qualifier <- res1$duration
  res1$study_duration_qualifier <- gsub("[a-zA-Z0-9.,\\+() ;:/-]", "",res1$study_duration_qualifier)
  names(res1) <- tolower(names(res1))

  exp_val <- grep(".*\\:",res1$exposure_route)
  for (i in 1:length(exp_val)){

    res1[exp_val[i],"exposure_method_original"] <- res1[exp_val[i],"exposure_route"]
    res1[exp_val[i],"exposure_route_original"] <- res1[exp_val[i],"exposure_route"]
    res1[exp_val[i],"exposure_method"] <- gsub("(.*\\:)(.*)","\\2",res1[exp_val[i],"exposure_route"])
    res1[exp_val[i],"exposure_method"] <- gsub("^\\s+","",res1[exp_val[i],"exposure_method"])
    res1[exp_val[i],"exposure_route"] <- gsub("(.*\\:)(.*)","\\1",res1[exp_val[i],"exposure_route"])
    res1[exp_val[i],"exposure_route"] <- gsub("\\:","",res1[exp_val[i],"exposure_route"])
    res1[exp_val[i],"exposure_route"] <- gsub("\\s+$","",res1[exp_val[i],"exposure_route"])

    }

  #print(unique(res1$exposure_method[res1$exposure_method == res1$exposure_route]))

  #res1$exposure_method[res1$exposure_method == res1$exposure_route] <- "nd"


  open_paranthesis_effect <- which(str_count(res1$critical_effect,"\\)") == 0 & str_count(res1$critical_effect,"\\(") == 1)
  open_paranthesis_effect2 <- grep("[^\\)]$",res1[which(str_count(res1$critical_effect,"\\)") == 0 & str_count(res1$critical_effect,"\\(") == 1),"critical_effect"])

  critical_effect_to_clean <- open_paranthesis_effect[open_paranthesis_effect2]
  res1[critical_effect_to_clean,"critical_effect"] <- gsub("\\(","",res1[critical_effect_to_clean,"critical_effect"])

  open_paranthesis_effect <-which(str_count(res1$critical_effect,"\\(") == 6)
  res1[open_paranthesis_effect,"critical_effect"] <- gsub("(.*\\(.*\\)\\;\\s+)(\\()(.*)","\\1\\3",res1[open_paranthesis_effect,"critical_effect"])

  res <- res1

  names.list <- c( "source_hash","casrn", "name","toxval_type","toxval_numeric","toxval_units","toxval_subtype",
                  "species","exposure_route","exposure_method","study_duration_value","study_duration_class",
                  "study_duration_units","critical_effect","long_ref","year","toxval_type_original",
                  "toxval_numeric_original","toxval_units_original","exposure_route_original",
                  "exposure_method_original","study_duration_value_original","study_duration_units_original",
                  "critical_effect_original","year_original","toxval_subtype_original","study_duration_class_original","document_name")

  res <- res[,(names(res)%in% names.list)]


  # trims leading and trailing whitespaces from the dataframe
  res <- data.frame(lapply(res, function(x) if(class(x)=="character") trimws(x) else(x)), stringsAsFactors=F, check.names=F)
  #print(str(res))

  print(dim(res))
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
  res <- res[!is.na(res[,"casrn"]),]

  # original health canada table one of the toxval type oral SF entry represented as inhalation exposure type
  res[which(res$toxval_type_original == "oral SF" & res$exposure_route_original == "inhalation"),c("exposure_method")] <- "-"
  res[which(res$toxval_type_original == "oral SF" & res$exposure_route_original == "inhalation"),c("exposure_method_original")] <- "-"
  res[which(res$toxval_type_original == "oral SF" & res$exposure_route_original == "inhalation"),c("exposure_route")] <- "oral"
  res[which(res$toxval_type_original == "oral SF" & res$exposure_route_original == "inhalation"),c("exposure_route_original")] <- "oral"

  #print(unique(res[which(res$toxval_type_original == "oral SF" ),c("exposure_route","exposure_route_original","exposure_method","exposure_method_original")]))


  #####################################################################
  cat("add other columns to res\n")
  #####################################################################
  res$source <- source
  res$source_source_id <- -1
  res$subsource <- "Health Canada"
  res$source_url <- "http://publications.gc.ca/collections/collection_2012/sc-hc/H128-1-11-638-eng.pdf"
  names(res)[is.element(names(res),"species")] <- "species_original"
  if(is.element("species_original",names(res))) res[,"species_original"] <- tolower(res[,"species_original"])
  res <- unique(res)
  res <- fill.toxval.defaults(toxval.db,res)
  #res <- generate.originals(toxval.db,res)
  res$toxval_numeric_original <- res$toxval_numeric
  res[is.na(res[,"document_name"]),"document_name"] <- "-"
  res <- unique(res)
  #print(View(res))
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
  refs <- res[,c("toxval_id","source","long_ref","year","source_study_id","document_name")]

  #####################################################################
  cat("add extra columns to refs\n")
  #####################################################################
  refs$record_source_type <- "government document"
  refs$record_source_note <- "-"
  refs$record_source_level <- "primary (risk assessment values)"
  refs$document_name <- "H128-1-11-638-eng.pdf"

  #####################################################################
  cat("delete unused columns from res\n")
  #####################################################################
  res <- res[,!is.element(names(res),c("long_ref","source_study_id","document_name"))]

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
