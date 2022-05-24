
#-------------------------------------------------------------------------------------
#' Load opp from toxval_source to toxval
#'
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param source.db The version of toxval_source from which the tables are loaded.
#' @param verbose If TRUE, print out extra diagnostic messages
#' @export
#-------------------------------------------------------------------------------------
toxval.load.opp <- function(toxval.db, source.db,verbose){
  printCurrentFunction(toxval.db)
  
  #####################################################################
  cat("start output log, log files for each source can be accessed from output_log folder\n")
  #####################################################################
  source <- "EPA OPP"
  
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
  
  query <- "select * from original_opp_table"
  
  res <- runQuery(query,source.db,T,F)
  res <- res[ , !(names(res) %in% c("source_id","clowder_id"))]
  res1 <- res
  # names replace period with underscore
  names(res1) <- gsub("\\.",'\\_', names(res1))
  # Split carcinogenic_HHBP_ppb range values to higher and lower values
  hhbp_ppb_idx <- which(!is.na(res1$carcinogenic_HHBP_ppb))
  hhbp_ppb_val <- res1$carcinogenic_HHBP_ppb[1:nrow(res1) %in% hhbp_ppb_idx]
  lower_carcinogenic_HHBP_ppb_val <- gsub("-[^-]+$", "", hhbp_ppb_val)
  res1$lower_carcinogenic_HHBP_ppb[1:nrow(res1) %in% hhbp_ppb_idx] <- lower_carcinogenic_HHBP_ppb_val
  higher_carcinogenic_HHBP_ppb_val <- gsub(".*\\-", "", hhbp_ppb_val)
  res1$higher_carcinogenic_HHBP_ppb[1:nrow(res1) %in% hhbp_ppb_idx] <- higher_carcinogenic_HHBP_ppb_val
  res1$lower_carcinogenic_HHBP_ppb[res1$lower_carcinogenic_HHBP_ppb == "-"] <- NA
  res1$higher_carcinogenic_HHBP_ppb[res1$higher_carcinogenic_HHBP_ppb == ""] <- NA
  res1$lower_carcinogenic_HHBP_ppb <-  as.numeric(res1$lower_carcinogenic_HHBP_ppb)
  res1$higher_carcinogenic_HHBP_ppb <- as.numeric(res1$higher_carcinogenic_HHBP_ppb)

  # rename carcinogenic_HHBP_ppb
  colnames(res1)[11] <- "original_carcinogenic_HHBP_ppb"
  nums <- unlist(lapply(res1, is.numeric))
  # extract toxval_type, toxval_units and study_duration class info from numeric value column names
  type_names <- names(res1[,nums])
  toxval_type_val <- gsub("_[^_]+$", "", type_names)
  study_duration_class <-  gsub("_RfD|_HHBP", "", toxval_type_val)
  study_duration_class <-replace(study_duration_class, study_duration_class=="cancer_slope_factor", "-")
  toxval_type <- gsub("^\\w+_","" ,toxval_type_val)
  toxval_type <- replace(toxval_type, toxval_type=="factor", "cancer_slope_factor")
  toxval_units <- gsub("^[^_]+_[^_]+_|^[^_]+_[^_]+_[^_]+_", "", type_names)
  #print(toxval_type)
  
  print(names(res1))
  #print(View(res1))
  t1 <- res1[,c(1,2,3,4,6,12)]
  # t1 <- res1[,c(1,2,3,5,8)]
  colnames(t1)[4] <- c("toxval_numeric")
  colnames(t1)[5] <- c("population")
  t1[,"population"] <- "-"
  t1[,"toxval_type"] <- c(rep(toxval_type[1], nrow(t1)))
  t1[,"toxval_units"] <- c(rep(toxval_units[1], nrow(t1)))
  t1[,"study_duration_class"] <- c(rep(study_duration_class[1], nrow(t1)))
  t1[,"study_type"] <- c(rep(study_duration_class[1], nrow(t1)))
  t1[,"critical_effect"] <- "-"
  t1[,"toxval_numeric_qualifier"] <- "="
  t1[,"species_original"] <- "-"
  t1[,"risk_assessment_class"] <- "repeat_dose"
  
  
  t2 <- res1[,c(1,2,3,5,6,12)]
  #t2 <- res1[,c(1,2,4,5,8)]
  colnames(t2)[4] <- c("toxval_numeric")
  colnames(t2)[5] <- c("population")
  t2["toxval_type"] <- c(rep(toxval_type[2], nrow(t2)))
  t2["toxval_units"] <- c(rep(toxval_units[2], nrow(t2)))
  t2["study_duration_class"] <- c(rep(study_duration_class[2], nrow(t2)))
  t2["study_type"] <- c(rep(study_duration_class[2], nrow(t2)))
  t2[,"critical_effect"] <- "-"
  t2[,"toxval_numeric_qualifier"] <- "="
  t2[,"species_original"] <- "human"
  t2[,"risk_assessment_class"] <- "acute"
  
  
  
  # t3 <- res1[,c(1,2,6,5,8)]
  # colnames(t3)[3] <- c("toxval_numeric")
  t3 <- res1[,c(1,2,3,7,9,12)]
  colnames(t3)[4] <- c("toxval_numeric")
  colnames(t3)[5] <- c("population")
  t3["population"] <- "-"
  t3["toxval_type"] <- c(rep(toxval_type[3], nrow(t3)))
  t3["toxval_units"] <- c(rep(toxval_units[3], nrow(t3)))
  t3["study_duration_class"] <- c(rep(study_duration_class[3], nrow(t3)))
  t3["study_type"] <- c(rep(study_duration_class[3], nrow(t3)))
  t3[,"critical_effect"] <- "-"
  t3[,"toxval_numeric_qualifier"] <- "="
  t3[,"species_original"] <- "-"
  t3[,"risk_assessment_class"] <- "chronic"
  
  
  
  
  t4 <- res1[,c(1,2,3,8,9,12)]
  colnames(t4)[4] <- c("toxval_numeric")
  colnames(t4)[5] <- c("population")
  # t4 <- res1[,c(1,2,7,5,8)]
  # colnames(t4)[3] <- c("toxval_numeric")
  t4["toxval_type"] <- c(rep(toxval_type[4], nrow(t4)))
  t4["toxval_units"] <- c(rep(toxval_units[4], nrow(t4)))
  t4["study_duration_class"] <- c(rep(study_duration_class[4], nrow(t4)))
  t4["study_type"] <- c(rep(study_duration_class[4], nrow(t4)))
  t4[,"critical_effect"] <- "-"
  t4[,"toxval_numeric_qualifier"] <- "="
  t4[,"species_original"] <- "human"
  t4[,"risk_assessment_class"] <- "chronic"
  
  
  # t5 <- res1[,c(1,2,9,5,8)]
  # colnames(t5)[3] <- c("toxval_numeric")
  t5 <- res1[,c(1,2,3,10,9,12)]
  colnames(t5)[4] <- c("toxval_numeric")
  colnames(t5)[5] <- c("population")
  t5["population"] <- "-"
  t5["toxval_type"] <- c(rep(toxval_type[5], nrow(t5)))
  t5["toxval_units"] <- c(rep(toxval_units[5], nrow(t5)))
  t5["study_duration_class"] <- c(rep(study_duration_class[5], nrow(t5)))
  t5["study_type"] <- "cancer"
  t5[,"critical_effect"] <- "cancer"
  t5[,"toxval_numeric_qualifier"] <- "="
  t5[,"species_original"] <- "-"
  t5[,"risk_assessment_class"] <- "cancer"
  
  
  # t6 <- res1[,c(1,2,11,5,8)]
  # colnames(t6)[3] <- c("toxval_numeric")
  t6 <- res1[,c(1,2,3,13,9,12)]
  colnames(t6)[4] <- c("toxval_numeric")
  colnames(t6)[5] <- c("population")
  t6["population"] <- "-"
  t6["toxval_type"] <- c(rep(toxval_type[6], nrow(t6)))
  t6["toxval_units"] <- c(rep(toxval_units[6], nrow(t6)))
  t6["study_duration_class"] <- c(rep(study_duration_class[6], nrow(t6)))
  t6["study_type"] <- "cancer"
  t6[,"critical_effect"] <- "cancer"
  t6[,"toxval_numeric_qualifier"] <- ">="
  t6[,"species_original"] <- "-"
  t6[,"risk_assessment_class"] <- "cancer"
  
  
  
  # 
  # t7 <- res1[,c(1,2,12,5,8)]
  # colnames(t7)[3] <- c("toxval_numeric")
  t7 <- res1[,c(1,2,3,14,9,12)]
  colnames(t7)[4] <- c("toxval_numeric")
  colnames(t7)[5] <- c("population")
  t7["population"] <- "-"
  t7["toxval_type"] <- c(rep(toxval_type[7], nrow(t7)))
  t7["toxval_units"] <- c(rep(toxval_units[7], nrow(t7)))
  t7["study_duration_class"] <- c(rep(study_duration_class[7], nrow(t7)))
  t7["study_type"] <- "cancer"
  t7[,"critical_effect"] <- "cancer"
  t7[,"toxval_numeric_qualifier"] <- "<="
  t7[,"species_original"] <- "-"
  t7[,"risk_assessment_class"] <- "cancer"
  
  

  opp_types <- rbind(t1,t2,t3,t4,t5,t6,t7)
  opp_types <- subset(opp_types,opp_types[,"toxval_numeric"]!="")
  opp_types <- opp_types[!is.na(opp_types$toxval_numeric),]
  opp_types$toxval_numeric <- as.numeric(opp_types$toxval_numeric)

  res <- opp_types
  
  #print(View(res))

  # res$population <- "-"
  # res$species_original <- "-"
  # res$toxval_numeric_qualifier <- "-"
  # res$phenotype <- "-"
  # name.list <- names(res)
  # name.list[is.element(name.list,"phenotype")] <- "critical_effect"
  # names(res) <- name.list
  # 
  res <- generate.originals(toxval.db,res)
  # 
  # opp_types <- res
  # 
  # opp_types$population[opp_types$toxval_type=="HHBP" & opp_types$study_duration_class=="acute"]<- opp_types$acute_HHBP_sensitive_lifestage[opp_types$toxval_type=="HHBP" & opp_types$study_duration_class=="acute"]
  # opp_types$population[opp_types$toxval_type=="HHBP" & opp_types$study_duration_class=="chronic"]<- opp_types$chronic_HHBP_sensitive_lifestage[opp_types$toxval_type=="HHBP" & opp_types$study_duration_class=="chronic"]
  # opp_types$species[opp_types$population != ""] <- "human"
  # opp_types$toxval_numeric_qualifier <- "="
  # opp_types$toxval_numeric_qualifier[opp_types$study_duration_class == "lower_carcinogenic"] <- ">="
  # opp_types$toxval_numeric_qualifier[opp_types$study_duration_class == "higher_carcinogenic"] <- "<="
  # opp_types$study_duration_class[opp_types$study_duration_class == "lower_carcinogenic"] <- "-"
  # opp_types$study_duration_class[opp_types$study_duration_class == "higher_carcinogenic"] <- "-"
  # opp_types$critical_effect[opp_types$toxval_type == "HHBP" & opp_types$toxval_numeric_qualifier == ">="] <- "cancer"
  # opp_types$critical_effect[opp_types$toxval_type == "HHBP" & opp_types$toxval_numeric_qualifier == "<="] <- "cancer"
  # opp_types$critical_effect[opp_types$toxval_type == "cancer_slope_factor"] <- "cancer"
  # 
  # res <- opp_types
  rm(opp_types) 
  print(names(res))

  names.list <- c("source_hash","casrn", "name", "toxval_numeric", "population","document_name","toxval_type", "toxval_units",
                  "study_duration_class", "study_type", "critical_effect", "toxval_numeric_qualifier", "species_original",
                  "toxval_numeric_original", "toxval_type_original","toxval_units_original","study_duration_class_original",
                  "study_type_original","critical_effect_original","toxval_numeric_qualifier_original")

  res <- res[,(names(res)%in% names.list)]
  res[is.na(res[,"document_name"]),"document_name"] <- "-"
  
  
  #####################################################################
  cat("checks, finds and replaces non ascii characters in res with XXX\n")
  #####################################################################
  res <- fix.non_ascii(res)

  # trims leading and trailing whitespaces from the dataframe
  res <- data.frame(lapply(res, function(x) if(class(x)=="character") trimws(x) else(x)), stringsAsFactors=F, check.names=F)
  #print(str(res))

  res[is.null(res[,"casrn"]),"casrn"] <- ""
  res[is.na(res[,"casrn"]),"casrn"] <- ""
  res[nchar(res[,"casrn"])<2,"casrn"] <- paste("NOCAS_",res[nchar(res[,"casrn"])<2,"name"],sep="")
  res <- res[!is.na(res[,"casrn"]),]
  # res$study_type <- res$study_duration_class
  # res[res[,"study_type"]=="-","study_type"] <- "cancer"
  # res[res[,"study_type"]=="","study_type"] <- "-"
  # 
  res$exposure_route <- "-"
  res$exposure_method <- "-"
  res$human_eco <- "human health"
  res$media <- "-"

  #####################################################################
  cat("add other columns to res\n")
  #####################################################################
  res$source <- source
  res$sex <- "-"
  res <- fill.toxval.defaults(toxval.db,res)
  #res <- generate.originals(toxval.db,res)
  res <- unique(res)
  res$subsource <- "OPP"
  res$details_text <- "EPA OPP Details"
  res$source_url <- "https://iaspub.epa.gov/apex/pesticides/f?p=HHBP:home"
  # name.list <- names(res)
  # name.list[is.element(name.list,"species")] <- "species_original"
  # names(res) <- name.list
  if(is.element("species_original",names(res))) res[,"species_original"] <- tolower(res[,"species_original"])

  #####################################################################
  cat("Fix the multiple name per CASRN in OPP\n")
  #####################################################################
  res <- unique(res)
  print(dim(res))
  temp <- unique(res[,c("casrn","name"),])
  temp <- temp[order(temp$casrn),]
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
  rownames(temp) <- temp$casrn
  for(i in 1:nrow(res)) {
    casrn <- res[i,"casrn"]
    name <- temp[casrn,"name"]
    res[i,"name"] <- name
  }

  res$toxval_numeric_original <- res$toxval_numeric
  res <- unique(res)
  print(dim(res))

  #####################################################################
  cat("add toxval_id to res\n")
  #####################################################################
  print(dim(res))
  res <- unique(res)
  print(dim(res))
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
  res <- merge(res,cid.list)
  res <- res[,!is.element(names(res),c("casrn","name"))]


  #####################################################################
  cat("pull out record source to refs\n")
  #####################################################################
  refs <- res[,c("toxval_id","source","source_url","document_name")]
  names(refs)[3] <- "url"

  #####################################################################
  cat("add extra columns to refs\n")
  #####################################################################
  refs$record_source_type <- "url"
  refs$record_source_note <- "All data is linked to from this page"
  refs$record_source_level <- "primary (risk assessment values)"
  refs$year <- "2018"

  #####################################################################
  cat("delete unused columns from res\n")
  #####################################################################
  #browser()
  res <- res[,!is.element(names(res),c("document_name"))]

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
