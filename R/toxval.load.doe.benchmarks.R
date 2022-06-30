
#-------------------------------------------------------------------------------------
#' Load new_doe_table and new_doe_benchmarks_table from toxval_source to toxval
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param source.db The source database to use.
#' @param verbose Whether the loaded rows should be printed to the console.
#' @export
#--------------------------------------------------------------------------------------
toxval.load.doe.benchmarks <- function(toxval.db,source.db,log=F) {
  printCurrentFunction(toxval.db)
  source <- "DOE Wildlife Benchmarks"
  source_table = "source_doe_benchmarks"
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
  names(res)[is.element(names(res),"url")] = "source_url"
  #nlist = c("chemical_id","casrn","name","document_name","source","source_hash","qc_status","source_url",
  #"test_species",
  #"test_species_noael",
  #"test_species_loael",
  #"endpoint_species",
  #"wildlife_noael",
  #"wildlife_loael",
  #"noael_food",
  #"loael_food",
  #"noael_water",
  #"loael_water",
  # "noael_piscivore",
  #"loael_piscivore")

  nlist = c("chemical_id","casrn","name","document_name","source","source_hash","qc_status","source_url",
            "test_species",
            "test_species_noael")
  t1 = res[,nlist]
  t1$toxval_type = "NOAEL"
  t1$toxval_subtype = "test_species_noael"
  t1$toxval_units = "mg/kg-day"
  t1$media = "food"
  t1$exposure_route = "oral"
  names(t1)[is.element(names(t1),"test_species_noael")] = "toxval_numeric"
  names(t1)[is.element(names(t1),"test_species")] = "species_original"

  nlist = c("chemical_id","casrn","name","document_name","source","source_hash","qc_status","source_url",
            "test_species",
            "test_species_loael")
  t2 = res[,nlist]
  t2$toxval_type = "LOAEL"
  t2$toxval_subtype = "test_species_loael"
  t2$toxval_units = "mg/kg-day"
  t2$media = "food"
  t2$exposure_route = "oral"
  names(t2)[is.element(names(t2),"test_species_loael")] = "toxval_numeric"
  names(t2)[is.element(names(t2),"test_species")] = "species_original"

  nlist = c("chemical_id","casrn","name","document_name","source","source_hash","qc_status","source_url",
            "endpoint_species",
            "wildlife_noael")
  t3 = res[,nlist]
  t3$toxval_type = "NOAEL"
  t3$toxval_subtype = "wildlife_noael"
  t3$toxval_units = "mg/kg-day"
  t3$media = "food"
  t3$exposure_route = "oral"
  names(t3)[is.element(names(t3),"wildlife_noael")] = "toxval_numeric"
  names(t3)[is.element(names(t3),"endpoint_species")] = "species_original"

  nlist = c("chemical_id","casrn","name","document_name","source","source_hash","qc_status","source_url",
            "endpoint_species",
            "wildlife_noael")
  t4 = res[,nlist]
  t4$toxval_type = "NOAEL"
  t4$toxval_subtype = "wildlife_noael"
  t4$toxval_units = "mg/kg-day"
  t4$media = "food"
  t4$exposure_route = "oral"
  names(t4)[is.element(names(t4),"wildlife_noael")] = "toxval_numeric"
  names(t4)[is.element(names(t4),"endpoint_species")] = "species_original"

  nlist = c("chemical_id","casrn","name","document_name","source","source_hash","qc_status","source_url",
            "endpoint_species",
            "noael_food")
  t5 = res[,nlist]
  t5$toxval_type = "NOAEL"
  t5$toxval_subtype = "noael_food"
  t5$toxval_units = "mg/kg-day"
  t5$media = "food"
  t5$exposure_route = "oral"
  names(t5)[is.element(names(t5),"noael_food")] = "toxval_numeric"
  names(t5)[is.element(names(t5),"endpoint_species")] = "species_original"

  nlist = c("chemical_id","casrn","name","document_name","source","source_hash","qc_status","source_url",
            "endpoint_species",
            "loael_food")
  t6 = res[,nlist]
  t6$toxval_type = "LOAEL"
  t6$toxval_subtype = "loael_food"
  t6$toxval_units = "mg/kg-day"
  t6$media = "food"
  t6$exposure_route = "oral"
  names(t6)[is.element(names(t6),"loael_food")] = "toxval_numeric"
  names(t6)[is.element(names(t6),"endpoint_species")] = "species_original"

  nlist = c("chemical_id","casrn","name","document_name","source","source_hash","qc_status","source_url",
            "endpoint_species",
            "noael_water")
  t7 = res[,nlist]
  t7$toxval_type = "NOAEL"
  t7$toxval_subtype = "noael_water"
  t7$toxval_units = "mg/kg-day"
  t7$media = "water"
  t7$exposure_route = "oral"
  names(t7)[is.element(names(t7),"noael_water")] = "toxval_numeric"
  names(t7)[is.element(names(t7),"endpoint_species")] = "species_original"

  nlist = c("chemical_id","casrn","name","document_name","source","source_hash","qc_status","source_url",
            "endpoint_species",
            "loael_water")
  t8 = res[,nlist]
  t8$toxval_type = "LOAEL"
  t8$toxval_subtype = "loael_water"
  t8$toxval_units = "mg/kg-day"
  t8$media = "water"
  t8$exposure_route = "oral"
  names(t8)[is.element(names(t8),"loael_water")] = "toxval_numeric"
  names(t8)[is.element(names(t8),"endpoint_species")] = "species_original"

  nlist = c("chemical_id","casrn","name","document_name","source","source_hash","qc_status","source_url",
            "endpoint_species",
            "noael_piscivore")
  t9 = res[,nlist]
  t9$toxval_type = "NOAEL"
  t9$toxval_subtype = "noael_piscivore"
  t9$toxval_units = "mg/l"
  t9$media = "piscivore"
  t9$exposure_route = "oral"
  names(t9)[is.element(names(t9),"noael_piscivore")] = "toxval_numeric"
  names(t9)[is.element(names(t9),"endpoint_species")] = "species_original"

  nlist = c("chemical_id","casrn","name","document_name","source","source_hash","qc_status","source_url",
            "endpoint_species",
            "loael_piscivore")
  t10 = res[,nlist]
  t10$toxval_type = "LOAEL"
  t10$toxval_subtype = "loael_piscivore"
  t10$toxval_units = "mg/l"
  t10$media = "piscivore"
  t10$exposure_route = "oral"
  names(t10)[is.element(names(t10),"loael_piscivore")] = "toxval_numeric"
  names(t10)[is.element(names(t10),"endpoint_species")] = "species_original"


  res1 = rbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10)

  res1 = res1[!is.element(res1$toxval_numeric,c("day-old white","-","0")),]
  res1$toxval_numeric = as.numeric(res1$toxval_numeric )
  res = res1

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
  res$source_url = "https://rais.ornl.gov/documents/tm86r3.pdf"
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
