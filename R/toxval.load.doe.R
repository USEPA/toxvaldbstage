
#-------------------------------------------------------------------------------------
#' Load new_doe_table and new_doe_benchmarks_table from toxval_source to toxval
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param source.db The source database to use.
#' @param verbose Whether the loaded rows should be printed to the console.
#' @export
#--------------------------------------------------------------------------------------
toxval.load.doe <- function(toxval.db,source.db,verbose=F) {
  printCurrentFunction(toxval.db)
  
  #####################################################################
  cat("start output log, log files for each source can be accessed from output_log folder\n")
  #####################################################################
  source <- "DOE Protective Action Criteria"

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
  query <- "select source_hash, casrn, name, PAC_1, PAC_2, PAC_3, toxval_units from new_doe_table"
  res <- runQuery(query,source.db,T,F)
  #####################################################################
  cat("checks, finds and replaces non ascii characters in res with XXX\n")
  #####################################################################
  res <- fix.non_ascii(res)

  res <- gather(res, "toxval_subtype", "toxval_numeric", PAC_1, PAC_2, PAC_3)
  res <- res[!is.na(res[,"casrn"]),]

  res$exposure_route <- "-"
  res$toxval_type <- "-"
  res$risk_assessment_class <- "-"
  res$study_type <- "-"
  res$subsource <- "-"
  res$year <- ""

  res <- generate.originals(toxval.db,res)
  #print(names(res))
  #####################################################################
  cat("add other columns to res\n")
  #####################################################################
  res$source <- source
  res$exposure_route <- "inhalation"
  res$source_url <- "https://www.energy.gov/ehss/protective-action-criteria-pac-aegls-erpgs-teels-rev-29-chemicals-concern-may-2016"
  res$human_eco <- "human health"
  res$toxval_type <- res$toxval_subtype
  res$risk_assessment_class <- "acute"
  res$study_type <- "acute"
  res <- fill.toxval.defaults(toxval.db,res)
  
  res$subsource <- "DOE EHSS"
  res$year <- "2016"
  res$toxval_numeric_original <- res$toxval_numeric
  res <- unique(res)

  # trims leading and trailing whitespaces from the dataframe
  res <- data.frame(lapply(res, function(x) if(class(x)=="character") trimws(x) else(x)), stringsAsFactors=F, check.names=F)
  #print(str(res))

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
  refs <- res[,c("source","toxval_id","year")]

  #####################################################################
  cat("add extra columns to refs\n")
  #####################################################################
  refs$url <- "https://sp.eota.energy.gov/pac/TeelDocs"
  refs$document_name <- "Revision_29A_Table2.pdf"
  refs$record_source_type <- "government document"
  refs$record_source_note <- "All data is on a single pdf"
  refs$record_source_level <- "primary (risk assessment values)"
  #refs$year <- "2018"
  refs$long_ref <- "Table 2: Protective Action Criteria (PAC) Rev. 29a based on applicable 60-minute AEGLs, ERPGs, or TEELs"
  refs$title <- "Table 2: Protective Action Criteria (PAC) Rev. 29a based on applicable 60-minute AEGLs, ERPGs, or TEELs"

  #####################################################################
  cat("delete unused columns from res\n")
  #####################################################################

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


  ###################################################################################################
  ###################################################################################################
  ###################################################################################################
  
  #####################################################################
  cat("start output log, log files for each source can be accessed from output_log folder\n")
  #####################################################################
  source <- "DOE Wildlife Benchmarks"

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

  query <- "select * from original_doe_benchmarks_table"
  #print(names(res))

  res <- runQuery(query,source.db,T,F)
  res <- res[ , !(names(res) %in% c("source_id","clowder_id"))]
  res1 <- res

  names(res1) <- gsub("\\.",'\\_', names(res1))
  names(res1) <- tolower(names(res1))
  types <- grep(".*\\(.*\\)", names(res1), value = T)
  toxval_units <- gsub(".*\\(|\\)", "", types)
  toxval_types <- gsub("_\\(.*\\)","", types)

  res1$`test_species_noael_(mg/kg/d)`[which(is.na(res1$`test_species_noael_(mg/kg/d)`))] <- ""
  res1$`test_species_noael_(mg/kg/d)` <- gsub("day-old white", "", res1$`test_species_noael_(mg/kg/d)`)
  res1$`test_species_noael_(mg/kg/d)`[which(res1$`test_species_noael_(mg/kg/d)` == "-")] <- ""
  res1$`test_species_noael_(mg/kg/d)`<- as.numeric(res1$`test_species_noael_(mg/kg/d)`)

  res1$`noael_piscivore_(mg/l)`[which(is.na(res1$`noael_piscivore_(mg/l)`))] <- ""
  res1$`noael_piscivore_(mg/l)`[which(res1$`noael_piscivore_(mg/l)` == "-")] <- ""
  res1$`noael_piscivore_(mg/l)` <- as.numeric(res1$`noael_piscivore_(mg/l)`)

  res1$`loael_piscivore_(mg/l)`[ which(is.na(res1$`loael_piscivore_(mg/l)`))] <- ""
  res1$`loael_piscivore_(mg/l)`[ which(res1$`loael_piscivore_(mg/l)` == "-")] <- ""
  res1$`loael_piscivore_(mg/l)` <- as.numeric(res1$`loael_piscivore_(mg/l)`)

  #print(names(res1))

  t1 <- res1[,c(1,2,11,4,5,6,10,7,3)]
  colnames(t1)[3] <- c("toxval_numeric")
  colnames(t1)[6] <- c("source")
  colnames(t1)[5] <- c("source_url")
  colnames(t1)[7] <- c("species")
  t1[,"toxval_type"] <- c(rep(toxval_types[1], nrow(t1)))
  t1[,"toxval_units"] <- c(rep(toxval_units[1], nrow(t1)))
  t1[,"media"] <- "oral"
  t1 <- t1[,c(names(t1[1:3]),names(t1[11]), names(t1[10]), names(t1[4:9]), names(t1[12]))]

  t2 <- res1[,c(1,2,12,4,5,6,10,7,3)]
  colnames(t2)[3] <- c("toxval_numeric")
  colnames(t2)[6] <- c("source")
  colnames(t2)[5] <- c("source_url")
  colnames(t2)[7] <- c("species")
  t2[,"toxval_type"] <- c(rep(toxval_types[2], nrow(t2)))
  t2[,"toxval_units"] <- c(rep(toxval_units[2], nrow(t2)))
  t2[,"media"] <- "oral"
  t2 <- t2[c(names(t2[1:3]),names(t2[11]), names(t2[10]), names(t2[4:9]), names(t1[12]))]


  t3 <- res1[,c(1,2,14,4,5,6,13,7,3)]
  colnames(t3)[3] <- c("toxval_numeric")
  colnames(t3)[6] <- c("source")
  colnames(t3)[5] <- c("source_url")
  colnames(t3)[7] <- c("species")
  t3[,"toxval_type"] <- c(rep(toxval_types[3], nrow(t3)))
  t3[,"toxval_units"] <- c(rep(toxval_units[3], nrow(t3)))
  t3[,"media"] <- "oral"
  t3 <- t3[c(names(t3[1:3]),names(t3[11]), names(t3[10]), names(t3[4:9]), names(t1[12]))]

  t4 <- res1[,c(1,2,15,4,5,6,13,7,3)]
  colnames(t4)[3] <- c("toxval_numeric")
  colnames(t4)[6] <- c("source")
  colnames(t4)[5] <- c("source_url")
  colnames(t4)[7] <- c("species")
  t4[,"toxval_type"] <- c(rep(toxval_types[4], nrow(t4)))
  t4[,"toxval_units"] <- c(rep(toxval_units[4], nrow(t4)))
  t4[,"media"] <- "food"
  t4 <- t4[c(names(t4[1:3]),names(t4[11]), names(t4[10]), names(t4[4:9]), names(t1[12]))]

  t5 <- res1[,c(1,2,16,4,5,6,13,7,3)]
  colnames(t5)[3] <- c("toxval_numeric")
  colnames(t5)[6] <- c("source")
  colnames(t5)[5] <- c("source_url")
  colnames(t5)[7] <- c("species")
  t5[,"toxval_type"] <- c(rep(toxval_types[5], nrow(t5)))
  t5[,"toxval_units"] <- c(rep(toxval_units[5], nrow(t5)))
  t5[,"media"] <- "water"
  t5 <- t5[c(names(t5[1:3]),names(t5[11]), names(t5[10]), names(t5[4:9]), names(t1[12]))]


  t6 <- res1[,c(1,2,17,4,5,6,13,7,3)]
  colnames(t6)[3] <- c("toxval_numeric")
  colnames(t6)[6] <- c("source")
  colnames(t6)[5] <- c("source_url")
  colnames(t6)[7] <- c("species")
  t6[,"toxval_type"] <- c(rep(toxval_types[6], nrow(t6)))
  t6[,"toxval_units"] <- c(rep(toxval_units[6], nrow(t6)))
  t6[,"media"] <- "piscivore"
  t6 <- t6[c(names(t6[1:3]),names(t6[11]), names(t6[10]), names(t6[4:9]), names(t1[12]))]


  t7 <- res1[,c(1,2,18,4,5,6,13,7,3)]
  colnames(t7)[3] <- c("toxval_numeric")
  colnames(t7)[6] <- c("source")
  colnames(t7)[5] <- c("source_url")
  colnames(t7)[7] <- c("species")
  t7[,"toxval_type"] <- c(rep(toxval_types[7], nrow(t7)))
  t7[,"toxval_units"] <- c(rep(toxval_units[7], nrow(t7)))
  t7[,"media"] <- "oral"
  t7 <- t7[c(names(t7[1:3]),names(t7[11]), names(t7[10]), names(t7[4:9]), names(t1[12]))]


  t8 <- res1[,c(1,2,19,4,5,6,13,7,3)]
  colnames(t8)[3] <- c("toxval_numeric")
  colnames(t8)[6] <- c("source")
  colnames(t8)[5] <- c("source_url")
  colnames(t8)[7] <- c("species")
  t8[,"toxval_type"] <- c(rep(toxval_types[8], nrow(t8)))
  t8[,"toxval_units"] <- c(rep(toxval_units[8], nrow(t8)))
  t8[,"media"] <- "food"
  t8 <- t8[c(names(t8[1:3]),names(t8[11]), names(t8[10]), names(t8[4:9]), names(t1[12]))]


  t9 <- res1[,c(1,2,20,4,5,6,13,7,3)]
  colnames(t9)[3] <- c("toxval_numeric")
  colnames(t9)[6] <- c("source")
  colnames(t9)[5] <- c("source_url")
  colnames(t9)[7] <- c("species")
  t9[,"toxval_type"] <- c(rep(toxval_types[9], nrow(t9)))
  t9[,"toxval_units"] <- c(rep(toxval_units[9], nrow(t9)))
  t9[,"media"] <- "water"
  t9 <- t9[c(names(t9[1:3]),names(t9[11]), names(t9[10]), names(t9[4:9]), names(t1[12]))]

  t10 <- res1[,c(1,2,21,4,5,6,13,7,3)]
  colnames(t10)[3] <- c("toxval_numeric")
  colnames(t10)[6] <- c("source")
  colnames(t10)[5] <- c("source_url")
  colnames(t10)[7] <- c("species")
  t10[,"toxval_type"] <- c(rep(toxval_types[10], nrow(t10)))
  t10[,"toxval_units"] <- c(rep(toxval_units[10], nrow(t10)))
  t10[,"media"] <- "piscivore"
  t10 <- t10[c(names(t10[1:3]),names(t10[11]), names(t10[10]), names(t10[4:9]), names(t1[12]))]

  doe_benchmarks_types <- rbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10)
  doe_benchmarks_types <- subset(doe_benchmarks_types,doe_benchmarks_types[,3]!="")



  res <- doe_benchmarks_types

  res$exposure_route <- "-"
  res$exposure_method <- "-"
  res$year <- "-"
  res$subsource <- "-"
  res <- generate.originals(toxval.db,res)
  print(names(res))

  names.list <- c("source_hash","casrn", "name", "species", "toxval_type", "toxval_numeric", "toxval_units","source_url","toxval_type_original",
                  "toxval_numeric_original","toxval_units_original","media","media_original","exposure_route","exposure_route_original",
                  "exposure_method","exposure_method_original","year","year_original","subsource")

  res <- res[,(names(res)%in% names.list)]

  #####################################################################
  cat("checks, finds and replaces non ascii characters in res with XXX\n")
  #####################################################################

  res <- fix.non_ascii(res)

  # trims leading and trailing whitespaces from the dataframe
  res <- data.frame(lapply(res, function(x) if(class(x)=="character") trimws(x) else(x)), stringsAsFactors=F, check.names=F)
  #print(str(res))

  name.list <- names(res)
  name.list[is.element(name.list,"species")] <- "species_original"
  names(res) <- name.list
  res[,"species_original"] <- tolower(res[,"species_original"])
  res <- res[!is.na(res$toxval_numeric),]
  res <- res[res$toxval_numeric>0,]
  res$toxval_numeric <- signif(res$toxval_numeric)
  res[grep("noael",res$toxval_type),"toxval_type"] <- "NOAEL"
  res[grep("loael",res$toxval_type),"toxval_type"] <- "LOAEL"
  # print(View(res))
  print(dim(res))
  res <- unique(res)
  print(dim(res))

  #####################################################################
  cat("add other columns to res\n")
  #####################################################################
  res$source <- source
  res$exposure_route <- "oral"
  res$exposure_method <- res$media
  res$year <- "1996"
  res$subsource <- "ORNL"
  res <- fill.toxval.defaults(toxval.db,res)
  
  res$toxval_numeric_original <- res$toxval_numeric
  res <- unique(res)
  #print(View(res))
  #####################################################################
  cat("add toxval_id to res\n")
  #####################################################################
  res <- unique(res)
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
  refs <- res[,c("source","toxval_id")]

  #####################################################################
  cat("add extra columns to refs\n")
  #####################################################################
  refs$url <- "https://rais.ornl.gov/documents/tm86r3.pdf"
  refs$document_name <- "Revision_29A_Table2.pdf"
  refs$record_source_type <- "government document"
  refs$record_source_note <- "All data is on a single pdf"
  refs$record_source_level <- "primary (risk assessment values)"
  refs$year <- "1996"
  refs$long_ref <- "Sample, B.E., Opresko, D.M., Suter, G.W. (1996) Toxicological Benchmarks for Wildlife: 1996 Revision. Springfield, VA: National Technical Information Service, U.S. Department of Commerce"
  refs$title <- "Toxicological Benchmarks for Wildlife: 1996 Revision"
  refs$author <- "Sample, B.E., Opresko, D.M., Suter, G.W."
  refs$journal <- "National Technical Information Service, U.S. Department of Commerce"
  #refs$document_name <- "tm86r3.pdf"

  #####################################################################
  cat("delete unused columns from res\n")
  #####################################################################

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
