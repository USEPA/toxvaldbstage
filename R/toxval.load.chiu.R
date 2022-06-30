#--------------------------------------------------------------------------------------
#' Load new_chiu from toxval_source to toxval
#'
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param source.db The source database to use.
#' @param verbose If TRUE, print out extra diagnostic messages
#' @export
#--------------------------------------------------------------------------------------
toxval.load.chiu <- function(toxval.db, source.db, log=F){
  printCurrentFunction(toxval.db)
  verbose = F
  #####################################################################
  cat("start output log, log files for each source can be accessed from output_log folder\n")
  #####################################################################
  source <- "Chiu"
  source_table = "source_chiu"
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
  res[,"toxval_units"] <- "mg/kg-day"
  res$study_type <- "chronic"
  res$risk_assessment_class <- "chronic"
  res$source <- "Chiu"
  res$exposure_method <- "-"
  res$source_url <- "https://doi.org/10.1289/EHP3368"
  res <- res[!is.na(res[,"casrn"]),]

  for(i in 1:nrow(res)) {
    if(contains(res[i,"toxval_type"],"RfD")) res[i,"toxval_type"] <- "RfD"
    if(contains(res[i,"toxval_type"],"MRL")) res[i,"toxval_type"] <- "MRL"

    res[i,"study_duration_units"] <- tolower(res[i,"study_duration_units"])
    res[i,"species"] <- tolower(res[i,"species"])
    res[i,"strain"] <- tolower(res[i,"strain"])
    res[i,"sex"] <- tolower(res[i,"sex"])
    if(is.na(res[i,"exposure_route"])) {res[i,"exposure_route"] <- "-"; res[i,"exposure_method"] <- "-"}
    else if(res[i,"exposure_route"]=="Oral - other") {res[i,"exposure_route"] <- "oral"; res[i,"exposure_method"] <- "other"}
    else if(res[i,"exposure_route"]=="Oral diet") {res[i,"exposure_route"] <- "oral"; res[i,"exposure_method"] <- "diet"}
    else if(res[i,"exposure_route"]=="Oral drinking water") {res[i,"exposure_route"] <- "oral"; res[i,"exposure_method"] <- "drinking water"}
    else if(res[i,"exposure_route"]=="Oral gavage") {res[i,"exposure_route"] <- "oral"; res[i,"exposure_method"] <- "gavage"}
    else if(res[i,"exposure_route"]=="Other") {res[i,"exposure_route"] <- "other"; res[i,"exposure_method"] <- "other"}
    res[i,"casrn"] <- fix.casrn(res[i,"casrn"])
  }
  #multiple year formats and year values with 5 digit number are flagged as -1
  res[grep("^.*\\/\\s*[0-9]{4}$", res$year),"year"] <- gsub("(^.*\\/\\s*)([0-9]{4}$)","\\2",res[grep("^.*\\/\\s*[0-9]{4}$", res$year),"year"])
  res[grep("^.*\\s+[0-9]{4}$", res$year),"year"] <- gsub("(^.*\\s+)([0-9]{4}$)","\\2",res[grep("^.*\\s+[0-9]{4}$", res$year),"year"])
  res[grep("^[0-9]{5,}$", res$year),"year"] <- -1
  res[grep("^-$", res$year),"year"] <- -1
  res$year <- as.numeric(res$year)

  print(dim(res))
  res <- unique(res)
  print(dim(res))

  res$critical_effect <- enc2utf8(res$critical_effect)
  res[grep("\\[",res$critical_effect),"critical_effect"] <- gsub("\\[","(",res[grep("\\[",res$critical_effect),"critical_effect"])
  res[grep("\\]",res$critical_effect),"critical_effect"] <- gsub("\\]",")",res[grep("\\]",res$critical_effect),"critical_effect"])

  res <- res[,(names(res) %in% c("source_hash","casrn","name","subsource","toxval_type","toxval_numeric","record_url","long_ref","toxval_units","critical_effect",
                                 "year","species","strain","study_duration_value", "study_duration_units", "sex", "uncertainty_factor",
                                 "ufa","ufh","ufs","ufl","ufd","ufother",
                                 "exposure_route","study_type", "risk_assessment_class", "source", "exposure_method", "source_url", "toxval_type_original","toxval_numeric_original","toxval_units_original",
                                 "critical_effect_original","year_original","strain_original","study_duration_value_original","study_duration_units_original","sex_original",
                                 "exposure_route_original","chemical_id"))]

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
  nlist = nlist[!is.element(nlist,c("ufa","ufh","ufs","ufl","ufd","ufother","uncertainty_factor"))]
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

  uf0 = res[c("toxval_id","ufa","ufh","ufs","ufl","ufd","ufother","uncertainty_factor")]
  res = res[ , !(names(res) %in% c("ufa","ufh","ufs","ufl","ufd","ufother","uncertainty_factor"))]
  ufx = uf0[,c("toxval_id","ufa")]
  ufx$uf_type = "A"
  names(ufx) = c("toxval_id","uf")
  uf1 = ufx

  ufx = uf0[,c("toxval_id","ufh")]
  ufx$uf_type = "H"
  names(ufx) = c("toxval_id","uf")
  uf1 = rbind(uf1,ufx)

  ufx = uf0[,c("toxval_id","ufs")]
  ufx$uf_type = "S"
  names(ufx) = c("toxval_id","uf")
  uf1 = rbind(uf1,ufx)

  ufx = uf0[,c("toxval_id","ufl")]
  ufx$uf_type = "L"
  names(ufx) = c("toxval_id","uf")
  uf1 = rbind(uf1,ufx)

  ufx = uf0[,c("toxval_id","ufd")]
  ufx$uf_type = "D"
  names(ufx) = c("toxval_id","uf")
  uf1 = rbind(uf1,ufx)

  ufx = uf0[,c("toxval_id","ufother")]
  ufx$uf_type = "Other"
  names(ufx) = c("toxval_id","uf")
  uf1 = rbind(uf1,ufx)

  ufx = uf0[,c("toxval_id","uncertainty_factor")]
  ufx$uf_type = "Total"
  names(ufx) = c("toxval_id","uf")
  uf1 = rbind(uf1,ufx)
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
  res$source_url = "https://ehp.niehs.nih.gov/doi/full/10.1289/EHP3368"
  res$subsource_url = "-"
  res$details_text = paste(source,"Details")
  for(i in 1:nrow(res)) res[i,"toxval_uuid"] = UUIDgenerate()
  for(i in 1:nrow(refs)) refs[i,"record_source_uuid"] = UUIDgenerate()
  runInsertTable(res, "toxval", toxval.db, verbose)
  runInsertTable(refs, "record_source", toxval.db, verbose)
  runInsertTable(uf1, "toxval_uf", toxval.db, F)
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

  query <- "select * from original_chiu"

  res <- runQuery(query,source.db,T,F)
  print(dim(res))

  res <- res[ , !(names(res) %in% c("source_id","clowder_id"))]

  res <- generate.originals(toxval.db,res)
  print(dim(res))

  res[,"toxval_units"] <- "mg/kg-day"
  res$study_type <- "chronic"
  res$risk_assessment_class <- "chronic"
  res$source <- "Chiu"
  res$exposure_method <- "-"
  res$source_url <- "https://doi.org/10.1289/EHP3368"
  res <- res[!is.na(res[,"casrn"]),]

  for(i in 1:nrow(res)) {
    if(contains(res[i,"toxval_type"],"RfD")) res[i,"toxval_type"] <- "RfD"
    if(contains(res[i,"toxval_type"],"MRL")) res[i,"toxval_type"] <- "MRL"

    res[i,"study_duration_units"] <- tolower(res[i,"study_duration_units"])
    res[i,"species"] <- tolower(res[i,"species"])
    res[i,"strain"] <- tolower(res[i,"strain"])
    res[i,"sex"] <- tolower(res[i,"sex"])
    if(is.na(res[i,"exposure_route"])) {res[i,"exposure_route"] <- "-"; res[i,"exposure_method"] <- "-"}
    else if(res[i,"exposure_route"]=="Oral - other") {res[i,"exposure_route"] <- "oral"; res[i,"exposure_method"] <- "other"}
    else if(res[i,"exposure_route"]=="Oral diet") {res[i,"exposure_route"] <- "oral"; res[i,"exposure_method"] <- "diet"}
    else if(res[i,"exposure_route"]=="Oral drinking water") {res[i,"exposure_route"] <- "oral"; res[i,"exposure_method"] <- "drinking water"}
    else if(res[i,"exposure_route"]=="Oral gavage") {res[i,"exposure_route"] <- "oral"; res[i,"exposure_method"] <- "gavage"}
    else if(res[i,"exposure_route"]=="Other") {res[i,"exposure_route"] <- "other"; res[i,"exposure_method"] <- "other"}
    res[i,"casrn"] <- fix.casrn(res[i,"casrn"])
  }
  #multiple year formats and year values with 5 digit number are flagged as -1
  res[grep("^.*\\/\\s*[0-9]{4}$", res$year),"year"] <- gsub("(^.*\\/\\s*)([0-9]{4}$)","\\2",res[grep("^.*\\/\\s*[0-9]{4}$", res$year),"year"])
  res[grep("^.*\\s+[0-9]{4}$", res$year),"year"] <- gsub("(^.*\\s+)([0-9]{4}$)","\\2",res[grep("^.*\\s+[0-9]{4}$", res$year),"year"])
  res[grep("^[0-9]{5,}$", res$year),"year"] <- -1
  res[grep("^-$", res$year),"year"] <- -1
  res$year <- as.numeric(res$year)

  print(dim(res))
  res <- unique(res)
  print(dim(res))

  res$critical_effect <- enc2utf8(res$critical_effect)
  res[grep("\\[",res$critical_effect),"critical_effect"] <- gsub("\\[","(",res[grep("\\[",res$critical_effect),"critical_effect"])
  res[grep("\\]",res$critical_effect),"critical_effect"] <- gsub("\\]",")",res[grep("\\]",res$critical_effect),"critical_effect"])

  res <- res[,(names(res) %in% c("source_hash","casrn","name","subsource","toxval_type","toxval_numeric","record_url","long_ref","toxval_units","critical_effect",
                                 "year","species","strain","study_duration_value", "study_duration_units", "sex", "uncertainty_factor","UFa","UFh","UFs","UFl","UFd","UFother",
                                 "exposure_route","study_type", "risk_assessment_class", "source", "exposure_method", "source_url", "toxval_type_original","toxval_numeric_original","toxval_units_original",
                                 "critical_effect_original","year_original","strain_original","study_duration_value_original","study_duration_units_original","sex_original",
                                 "exposure_route_original"))]

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
  res$toxval_numeric_qualifier <- "="
  res$human_eco <- "human health"
  name.list <- names(res)
  name.list[is.element(name.list,"species")] <- "species_original"
  res$media <- "-"
  res$details_text <- "Chiu Details"
  res <- fill.toxval.defaults(toxval.db,res)

  res$toxval_numeric_original <- res$toxval_numeric
  res <- unique(res)
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
  refs1 <- res[,c("toxval_id","source_url","source")]
  names(refs1)[2] <- "url"

  refs2 <- res[,c("toxval_id","record_url","long_ref","source")]
  names(refs2)[2] <- "url"

  cat("add extra columns to refs\n")
  #####################################################################
  refs1$pmid <- 29968566
  refs1$record_source_type <- "journal article"
  refs1$record_source_note <- "This article compiles PODs and RfD values from primary sources"
  refs1$record_source_level <- "secondary"
  refs1$long_ref <- "Beyond the RfD: Broad Application of a Probabilistic Approach to Improve Chemical Dose-Response Assessments for Noncancer Effects.,Chiu WA, Axelrad DA, Dalaijamts C, Dockins C, Shao K, Shapiro AJ, Paoli G, Environ Health Perspect. 2018 Jun 28;126(6):067009. doi: 10.1289/EHP3368"
  refs1$title <- "Beyond the RfD: Broad Application of a Probabilistic Approach to Improve Chemical Dose-Response Assessments for Noncancer Effects"
  refs1$journal <- "Environ Health Perspect"
  refs1$volume <- "126"
  refs1$issue <- "6"
  refs1$year <- "2018"
  refs1$document_name <- "EHP3368.pdf"

  refs2$record_source_type <- "journal article"
  refs2$record_source_note <- "-"
  refs2$record_source_level <- "primary"

  #####################################################################
  cat("delete unused columns from res\n")
  #####################################################################
  exclude.list <- c("long_ref","uncertainty_factor","UFa","UFh","UFs","UFl" ,"UFd","UFother","record_url")
  res <- res[,!is.element(names(res),exclude.list)]

  #####################################################################
  cat("load res and refs to the database\n")
  #####################################################################
  res <- unique(res)
  refs1 <- unique(refs1)
  refs2 <- unique(refs2)
  res$datestamp <- Sys.Date()
  for(i in 1:nrow(res)) res[i,"toxval_uuid"] <- UUIDgenerate()
  for(i in 1:nrow(refs1)) refs1[i,"record_source_uuid"] <- UUIDgenerate()
  for(i in 1:nrow(refs2)) refs2[i,"record_source_uuid"] <- UUIDgenerate()

  runInsertTable(res, "toxval", toxval.db, verbose)
  runInsertTable(refs1, "record_source", toxval.db, verbose)
  runInsertTable(refs2, "record_source", toxval.db, verbose)

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
