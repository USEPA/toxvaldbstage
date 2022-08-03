
#-------------------------------------------------------------------------------------
#' Load ecotox data from datahub to toxval
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param verbose Whether the loaded rows should be printed to the console.
#' @export
#--------------------------------------------------------------------------------------
toxval.load.new_ecotox <- function(toxval.db, verbose=T) {
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
  query <- "select * from cdip.ecotox_webserv_vw_nofilters"
  db <- "prod_datahub"
  res1 <- runQuery_psql(query,db)

  res1 <- unique(res1)
  ECOTOX_NEW <<- res1

  res <- unique(ECOTOX_NEW)
  print(dim(res))

  dict <- unique(res[,c("species_scientific_name","species_common_name","species_group","habitat")])
  file <- paste0(toxval.config()$datapath,"ecotox/ecotox_files/ECOTOX_dictionary_",Sys.Date(),".xlsx")
  #write.xlsx(dict,file)

  name.list <- c("dsstox_substance_id","result_number","habitat" ,"cas_number","chemical_name","chemical_grade",
                 "chemical_purity","species_scientific_name","species_common_name","species_group",
                 "organism_age","organism_age_units","organism_lifestage","exposure_type",
                 "control_type","chemical_analysis_method","media_type","test_location",
                 "number_doses","asobserved_duration_std","observed_duration_units_std","endpoint",
                 "effect","effect_measurement","response_site","conc1_type_std",
                 "conc1_mean_std","conc1_units_std","conc1_author","conc1_units_author","reference_number","author", "title",
                 "source","publication_year","summary_additional_parameters","exposure_group","effect_group_level",
                 "test_method_comment","result_sample_unit","result_sample_unit_desc")

  res <- res[,name.list]

  name.list <- c("dtxsid","source_source_id","habitat" ,"casrn","name","chemical_grade",
                 "chemical_purity","species_original","species_common","population",
                 "organism_age","organism_age_units","lifestage","exposure_method",
                 "quality","chemical_analysis_method","media","test_location",
                 "number_doses","study_duration_value","study_duration_units","toxval_type",
                 "effect","critical_effect","response_site","conc1_type_std",
                 "toxval_numeric_standard","toxval_numeric_standard_units","toxval_numeric","toxval_units","source_study_id","author", "title",
                 "long_ref","year","summary_additional_parameters","exposure_route","study_type",
                 "guideline","result_sample_unit","sex")

  names(res) <- name.list
  #print(View(res[1:1000,]))


  name.list <- c("dtxsid","source_source_id","habitat","casrn","name",
                 "species_original","exposure_method","quality","media",
                 "study_duration_value","study_duration_units","toxval_type",
                 "study_type","critical_effect","lifestage",
                 "toxval_numeric","toxval_units","source_study_id","author", "title",
                 "long_ref","year","exposure_route","toxval_numeric_standard","toxval_numeric_standard_units",
                 "guideline","sex")
  res <- res[,name.list]
  print(dim(res))
  res <- unique(res)
  #print(View(res))

  # assign types with "LT" the numeric and units values are taken from study_duration variables
  tt.list <- res$toxval_type
  #tt.list <- substr(tt.list,1,2)
  tt.list <- gsub("(^[a-zA-Z]+)([0-9]*)(.*)","\\1",tt.list)

  res1 <- res[tt.list!="LT",]
  res2 <- res[tt.list=="LT",]
  dose.units <- res2$toxval_numeric_standard_units
  time.value <- res2$study_duration_value
  dose.value <- res2$toxval_numeric_standard
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
  res3$toxval_numeric_standard <- time.value
  res3$toxval_numeric_standard_units <- time.units

  res <- rbind(res1,res3)

  #fix casrn
  x <- res[,"casrn"]
  for(i in 1:length(x)) x[i] <- fix.casrn(x[i])
  res[,"casrn"] <- x


  # prefix quality with control type: as it represents the field from ecotox from which it was derived.
  res$quality <- paste("Control type:",res$quality)

  # assign both NR and NA characters in toxval_numeric as NA
  res[is.element(res[,"toxval_numeric"],"NR"),"toxval_numeric"] <- NA
  res[is.element(res[,"toxval_numeric"],"NA"),"toxval_numeric"] <- NA

  # assign both NR and NA characters in toxval_numeric_standard as NA
  res[is.element(res[,"toxval_numeric_standard"],"NR"),"toxval_numeric_standard"] <- NA
  res[is.element(res[,"toxval_numeric_standard"],"NA"),"toxval_numeric_standard"] <- NA


  # extract all numeric values from toxval_numeric and round upto 4 significant digits
  res$toxval_numeric_standard <- as.numeric(str_extract_all(res$toxval_numeric_standard, "[0-9.E+-]+", simplify = TRUE)[,1])
  res[!is.na(res$toxval_numeric_standard),"toxval_numeric_standard"] <- signif(res[!is.na(res$toxval_numeric_standard),"toxval_numeric_standard"], digits = 4)

  # replace non numeric and range values in toxval numeric with toxval_numeric_standard values
  non_num <- unique(res[grep("^[^[:alnum:]]", res$toxval_numeric),"toxval_numeric"])
  non_num <- non_num[grep("^\\.\\d+$",non_num,invert = T)]

  res[which(non_num %in% res$toxval_numeric),"toxval_numeric"] <- res[which(non_num %in% res$toxval_numeric),"toxval_numeric_standard"]

  res[grep("\\-", res$toxval_numeric),"toxval_numeric"] <-  res[grep("\\-", res$toxval_numeric),"toxval_numeric_standard"]
  res[grep("^[^[:alnum:]]", res$toxval_numeric),"toxval_numeric"] <- res[grep("^[^[:alnum:]]", res$toxval_numeric),"toxval_numeric_standard"]



  #[1] NA          "NR/"       "NR/NR/NR/" "NRNRNR"    ""          "NANRNR"    "ca"
  res[grep("\\d+",res$toxval_numeric, invert = T),"toxval_numeric"] <- res[grep("\\d+",res$toxval_numeric, invert = T),"toxval_numeric_standard"]



  #"3.882.875.93"          "0.74NRNR"
  non_numeric_vals <- grep("\\d+\\.+\\d+\\.+\\d+|[0-9]+[a-zA-Z]{2,}", res$toxval_numeric)
  res[non_numeric_vals,"toxval_numeric"] <- res[non_numeric_vals, "toxval_numeric_standard"]

  #0.0001/NR/NR/  NR0.15<160
  non_numeric_vals <- grep("^NR.*|\\/", res$toxval_numeric)
  res[non_numeric_vals,"toxval_numeric"] <- res[non_numeric_vals, "toxval_numeric_standard"]

  # 3990000*NRNR   222018502660
  non_numeric_vals <- grep("\\d{6,}", res$toxval_numeric)
  res[non_numeric_vals,"toxval_numeric"] <- res[non_numeric_vals, "toxval_numeric_standard"]

  #1.08E+049.56E+031.22E+04  2400*2000*3000*
  non_numeric_vals <- grep("\\+.*\\+|\\*", res$toxval_numeric)
  res[non_numeric_vals,"toxval_numeric"] <- res[non_numeric_vals, "toxval_numeric_standard"]


  #2.3~09.1  4129>45
  non_numeric_vals <- grep("\\~|\\>|\\<", res$toxval_numeric)
  res[non_numeric_vals,"toxval_numeric"] <- res[non_numeric_vals, "toxval_numeric_standard"]


  # extract all numeric values from toxval_numeric and round upto 4 significant digits
  res$toxval_numeric <- as.numeric(str_extract_all(res$toxval_numeric, "[0-9.E+-]+", simplify = TRUE)[,1])
  res[!is.na(res$toxval_numeric),"toxval_numeric"] <- signif(res[!is.na(res$toxval_numeric),"toxval_numeric"], digits = 4)


  # print(str(res))
  #####################################################################
  cat("add other columns to res\n")
  #####################################################################
  res$source <- source
  res$subsource <- "EPA ORD"
  res$source_url <- "https://cfpub.epa.gov/ecotox/"
  res$details_text <- "ECOTOX Details"

  # trims leading and trailing whitespaces from the dataframe
  res <- data.frame(lapply(res, function(x) if(class(x)=="character") trimws(x) else(x)), stringsAsFactors=F, check.names=F)
  #print(str(res))

  res$datestamp <- Sys.time()
  res[,"species_original"] <- tolower(res[,"species_original"])
  res <- res[,!is.element(names(res),"species_common")]
  res <- fill.toxval.defaults(toxval.db,res)
  res <- generate.originals(toxval.db,res)


  # extract gender info from sex(result_sample_unit_desc)

  res[grep("^\\bmale\\b",res$sex, ignore.case = T),"sex"] <- "male"
  res[grep("^\\bfemale\\b",res$sex, ignore.case = T),"sex"] <- "female"
  res[grep("Pregnant female",res$sex, ignore.case = T),"sex"] <- "female"
  res[grep("\\bmale\\b|\\bfemale\\b", res$sex, invert = T),"sex"] <- "-"



  # remove * and / trailing toxval_type
  res[grep(".*\\*$",res$toxval_type),"toxval_type"] <- gsub("(.*)(\\*$)", "\\1",res[grep(".*\\*$",res$toxval_type),"toxval_type"])
  res[grep(".*\\/$",res$toxval_type),"toxval_type"] <- gsub("(.*)(\\/$)", "\\1",res[grep(".*\\/$",res$toxval_type),"toxval_type"])


  # extract numeric values from study duration and replace na and nr values with -999
  res[is.element(res[,"study_duration_value"],"NR"),"study_duration_value"] <- NA
  res[grep("^[^0-9A-Za-z\\.]+",res$study_duration_value),"study_duration_value"] <- gsub("(^[^0-9A-Za-z\\.]+)(.*)","\\2",res[grep("^[^0-9A-Za-z\\.]+",res$study_duration_value),"study_duration_value"])
  res$study_duration_value <- str_replace_all(res$study_duration_value,fixed("NR"),"-999")
  res$study_duration_value[is.na(res$study_duration_value)] <- -999
  res$study_duration_value <- as.numeric(res$study_duration_value)


  # use ecotox duration units dict derived from ecotox website to map abbreviations to expanded forms
  dur_units_dict <- read.xlsx("./ecotox/ecotox_files/ECOTOX-dur-units-Appendix-I.xlsx")
  dur_units_dict[grep("Pretreatment, time unknown",dur_units_dict[,2]),1] <- "-X"

  for(i in 1:nrow(dur_units_dict)) {
    valold <- dur_units_dict[i,1]
    valnew <- dur_units_dict[i,2]
    res[is.element(res$study_duration_units,valold),"study_duration_units"] <- valnew
  }

  # replace Day(s) to day in study_duration_units
  res[grep("Day\\(s\\)\\s*", res$study_duration_units),"study_duration_units"] <- gsub("(Day\\(s\\))(.*)","Day\\2",res[grep("Day\\(s\\)\\s*", res$study_duration_units),"study_duration_units"])


  # use ecotox exp route dict derived from ecotox website to map abbreviations to expanded forms
  exp_route_dict <- read.xlsx("./ecotox/ecotox_files/ECOTOX-exp_route-Appendix-J.xlsx")


  for(i in 1:nrow(exp_route_dict)) {
    valold <- exp_route_dict[i,1]
    valnew <- exp_route_dict[i,2]
    res[is.element(res$exposure_route,valold),"exposure_route"] <- valnew
  }

  # assign na toxval_units as hyphen
  res[which(res$toxval_units == "NA"),"toxval_units"] <- "-"


  #print(View(res[1:1000,]))

  print(dim(res))
  res <- unique(res)
  print(dim(res))


  res <- res[which(res$toxval_numeric != '-999' & res$toxval_numeric_standard != '-999'),]
  print(dim(res))


  # #####################################################################
  # cat("checks, finds and replaces non ascii characters in res with XXX\n")
  # #####################################################################
  # res <- fix.non_ascii(res)



  #####################################################################
  cat("map chemicals\n")
  #####################################################################
  res <- res[!is.na(res[,"casrn"]),]
  name.list <- c("casrn","name",names(res)[!is.element(names(res),c("casrn","name"))])
  res <- res[,name.list]
  casrn.list <- res[,c("casrn","name","dtxsid")]
  cid.list <- get.cid.list.toxval.ecotox(toxval.db, casrn.list,source)
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
  refs <- res[,c("toxval_id","source","author","title","long_ref","year","source_study_id","quality","guideline")]

  #####################################################################
  cat("add extra columns to refs\n")
  #####################################################################
  refs$page = str_trim(gsub("p.","",word(refs$long_ref,-1,sep=":")))
  refs$volume = as.numeric(gsub("\\(.*\\)","",word(gsub("^.*\\.","",gsub(":[^:]*$","",refs$long_ref)),-1)))
  refs$volume[is.na(refs$volume)] = ""
  refs$journal = gsub("[0-9\\(\\)]*:[^:]*$","",refs$long_ref)
  refs$long_ref = paste0(refs$author," (",refs$year,"). ",refs$title,". ",refs$long_ref)
  refs$record_source_type <- "journal article"
  refs$record_source_note <- "-"
  refs$record_source_level <- "primary"

  #####################################################################
  cat("delete unused columns from res\n")
  #####################################################################
  res <- res[,!is.element(names(res),c("author","title","long_ref","guideline","quality","source_study_id"))]
  res <- unique(res)

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
  toxval.load.chemical.list.ecotox(toxval.db, source)


  #####################################################################
  cat("map chemicals to dsstox\n")
  #####################################################################
  map.chemical.to.dsstox.ecotox(toxval.db, source)
  table.list <- c("toxval","cancer_summary","genetox_summary","genetox_details","skin_eye","chemical_list","bcfbaf")
  for(table in table.list) set.dtxsid.by.source(toxval.db,table,source)

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

#   #####################################################################
#   cat("fix risk assessment class by source\n")
#   #####################################################################
#
#   fix.risk_assessment_class.by.source(toxval.db, source)
#
  #####################################################################
  cat("fill chemical by source\n")
  #####################################################################
  fill.chemical.by.source(toxval.db, source)

  # #####################################################################
  # cat("export missing rac by source\n")
  # #####################################################################
  # export.missing.rac.by.source(toxval.db, source)

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