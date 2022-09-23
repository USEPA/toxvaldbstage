#-------------------------------------------------------------------------------------
#' Load ECOTOX from the web services output to toxval
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param source.db The version of toxval source - used to manage chemicals
#' @param verbose Whether the loaded rows should be printed to the console.
#' @param log If TRUE, send output to a log file
#' @param do.load If TRUE, load the data from the input file and put into a global variable
#' @export
#--------------------------------------------------------------------------------------
toxval.load.ecotox <- function(toxval.db,source.db,log=F,do.load=F) {
  printCurrentFunction(toxval.db)
  source <- "ECOTOX"
  source_table = "direct_load"

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
  if(!exists("ECOTOX")) do.load=T
  if(do.load) {
    cat("load ECOTOX data\n")
    file <- paste0(toxval.config()$datapath,"ecotox/ecotox_files/ecotox_web_services_vw_090121.xlsx")
    print(file)
    temp <- read.xlsx(file)
    print(dim(temp))
    #print(View(temp))
    temp <- unique(temp)
    print(dim(temp))
    ECOTOX <<- temp
  }
  res <- ECOTOX

  #res = res[1:10000,]
  # cat("SH:",length(unique(res$source_hash)),"\n")
  # browser()
  res$source = source

  dict <- unique(res[,c("SPECIES_SCIENTIFIC_NAME","SPECIES_COMMON_NAME","SPECIES_GROUP","HABITAT")])
  file <- paste0(toxval.config()$datapath,"ecotox/ecotox_files/ECOTOX_dictionary_",Sys.Date(),".xlsx")
  write.xlsx(dict,file)

  # "HABITAT"                       "CAS_NUMBER"                    "CHEMICAL_NAME"                 "CHEMICAL_GRADE"                "CHEMICAL_PURITY"
  # "SPECIES_SCIENTIFIC_NAME"       "SPECIES_COMMON_NAME"           "SPECIES_GROUP"                 "ORGANISM_AGE"                  "ORGANISM_AGE_UNITS"
  # "ORGANISM_LIFESTAGE"            "EXPOSURE_TYPE"                 "CONTROL_TYPE"                  "CHEMICAL_ANALYSIS_METHOD"      "MEDIA_TYPE"
  # "TEST_LOCATION"                 "NUMBER_DOSES"                  "OBSERVED_DURATION_STD"         "OBSERVED_DURATION_UNITS_STD"   "ENDPOINT"
  # "EFFECT"                        "EFFECT_MEASUREMENT"            "RESPONSE_SITE"                 "CONC1_TYPE_STD"                "CONC1_MEAN_STD"
  # "CONC1_UNITS_STD"               "CONC1_AUTHOR"                  "CONC1_UNITS_AUTHOR"            "REFERENCE_NUMBER"              "AUTHOR"
  # "TITLE"                         "SOURCE"                        "PUBLICATION_YEAR"              "SUMMARY_ADDITIONAL_PARAMETERS"
  #
  name.list <- c("HABITAT" ,"casrn","name","CHEMICAL_GRADE","CHEMICAL_PURITY",
                 "species_original","species_common","population","ORGANISM_AGE","ORGANISM_AGE_UNITS",
                 "lifestage","exposure_route","quality","CHEMICAL_ANALYSIS_METHOD","media",
                 "TEST_LOCATION","NUMBER_DOSES","study_duration_value","study_duration_units","toxval_type",
                 "study_type","critical_effect1","critical_effect2","CONC1_TYPE_STD","toxval_numeric",
                 "toxval_units","CONC1_AUTHOR","CONC1_UNITS_AUTHOR","source_study_id","author",
                 "title","long_ref","year","SUMMARY_ADDITIONAL_PARAMETERS")

  names(res) <- name.list
  name.list <- c("casrn","name",
                 "species_original","species_common",
                 "exposure_route",
                 "quality","media",
                 "study_duration_value","study_duration_units","toxval_type",
                 "study_type","critical_effect1","critical_effect2","lifestage",
                 "toxval_numeric","toxval_units","source_study_id","author", "title",
                 "long_ref","year")
  res <- res[,name.list]
  print(dim(res))
  res <- unique(res)
  print(dim(res))
  cat("get the combined critical effect\n")
  nlist = c("study_type","critical_effect1","critical_effect2")
  res$critical_effect = apply(res[,nlist],1,paste,collapse=":")
  res = subset(res,select=-c(critical_effect1,critical_effect2))

  cat("collapse rows differing by only the critical effect\n")
  temp = subset(res,select = -c(critical_effect))
  res$key = NA
  for (i in 1:nrow(temp)){
    row <- temp[i,]
    res[i,"key"] = digest(paste0(row,collapse=""), serialize = FALSE)
    if(i%%1000==0) cat("add key to res:,",i," out of ",nrow(res),"\n")
  }

  res2 <- res
  res2$critical_effect <- "-"
  res2 <- unique(res2)
  cat("Before removing critical effect: ",dim(res),"\n")
  cat("After removing critical effect: ",dim(res2),"\n")
  for(i in 1:nrow(res2)) {
    key = res2[i,"key"]
    temp = res[is.element(res$key,key),"critical_effect"]
    res2[i,"critical_effect"] <- paste(temp,collapse="|")
    if(i%%1000==0) cat("collapse critical effect:",i," out of ",nrow(res),"\n")
  }
  res = subset(res2,select = -c(key))

  #browser()
  cat("set the source_hash\n")
  res$source_hash = NA
  for (i in 1:nrow(res)){
    row <- res[i,]
    res[i,"source_hash"] = digest(paste0(row,collapse=""), serialize = FALSE)
    if(i%%1000==0) cat(i," out of ",nrow(res),"\n")
  }

  #browser()
  # cat("SH:",length(unique(res$source_hash)),"\n")
  # browser()
  res = source_chemical.ecotox(toxval.db,source.db,res,source,chem.check.halt=FALSE,
                               casrn.col="casrn",name.col="name",verbose=F)
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

  # cat("SH:",length(unique(res$source_hash)),"\n")
  # browser()
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

  # cat("SH:",length(unique(res$source_hash)),"\n")
  # browser()
  #####################################################################
  cat("Add the code from the original version from Aswani\n")
  #####################################################################
  cremove = c("chemical_index","","","")
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

  # cat("SH:",length(unique(res$source_hash)),"\n")
  # browser()
  # examples ...
  # names(res)[names(res) == "source_url"] = "url"
  # colnames(res)[which(names(res) == "phenotype")] = "critical_effect"

  #####################################################################
  cat("Generic steps \n")
  #####################################################################
  res = unique(res)
  res = fill.toxval.defaults(toxval.db,res)
  res = res[!is.na(res$toxval_numeric),]
  res = res[res$toxval_numeric>0,]
  res = generate.originals(toxval.db,res)
  if(is.element("species_original",names(res))) res[,"species_original"] = tolower(res[,"species_original"])
  res$toxval_numeric = as.numeric(res$toxval_numeric)
  print(dim(res))
  res=fix.non_ascii.v2(res,source)
  res = data.frame(lapply(res, function(x) if(class(x)=="character") trimws(x) else(x)), stringsAsFactors=F, check.names=F)
  res = unique(res)
  res = res[,!is.element(names(res),c("casrn","name"))]
  print(dim(res))

  # cat("SH:",length(unique(res$source_hash)),"\n")
  # browser()
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

  # cat("SH:",length(unique(res$source_hash)),"\n")
  # browser()
  #####################################################################
  cat("load res and refs to the database\n")
  #####################################################################
  res = unique(res)
  refs = unique(refs)
  res$datestamp = Sys.Date()
  res$source_table = source_table
  res$subsource <- "EPA ORD"
  res$source_url <- "https://cfpub.epa.gov/ecotox/"
  res$details_text = paste(source,"Details")
  #for(i in 1:nrow(res)) res[i,"toxval_uuid"] = UUIDgenerate()
  #for(i in 1:nrow(refs)) refs[i,"record_source_uuid"] = UUIDgenerate()
  runInsertTable(res, "toxval", toxval.db, verbose)
  runInsertTable(refs, "record_source", toxval.db, verbose)
  print(dim(res))
  # cat("SH:",length(unique(res$source_hash)),"\n")
  # browser()
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
}
