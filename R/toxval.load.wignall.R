
#--------------------------------------------------------------------------------------
#' Load Wignall from toxval_source to toxval
#'
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param source.db The version of toxval_source from which the tables are loaded.
#' @param verbose If TRUE, print out extra diagnostic messages
#' @export
#--------------------------------------------------------------------------------------
toxval.load.wignall <- function(toxval.db,source.db, verbose=F){
  printCurrentFunction(toxval.db)
  
  #####################################################################
  cat("start output log, log files for each source can be accessed from output_log folder\n")
  #####################################################################
  source <- "Wignall"
  
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
  query <- "select
  casrn,
  name,
  toxval_numeric,
  toxval_units,
  toxval_type,
  subsource,
  reference as long_ref,
  critical_effect,
  POD_numeric,
  POD_units,
  POD_type,
  UF as numUF
  from new_wignall_table"
  
  query <- "select * from original_wignall_table"
  res <- runQuery(query,source.db,T,F)
  res <- res[ , !(names(res) %in% c("source_id","clowder_id"))]  
  #print(names(res))
  res1 <- res
  print(View(res1))
  #pull toxval units and toxval type from toxicity value type field
  res1[,"new_toxval_units"] <- gsub(".*\\(|\\)", "",res1$Toxicity.value.type )
  res1[,"new_toxval_type"] <- gsub("\\(.*|\\)", "",res1$Toxicity.value.type )
  res1[,"UF"] <- res1$POD / res1$Toxicity.Value
  # alter names for new_toxval_units and new_toxval_type as toxval_units and toxval_type
  colnames(res1) <- c("source_hash", "index","casrn","name","toxval_type_abbr","toxval_type_original", "subsource",
                      "toxval_numeric", "POD_numeric", "POD_units","POD_type","organ","effect",
                      "effect_description","dose_number","dose_values","dose_units","dose_converted",
                      "DR_type","mean_response","response_units","SD_of_response",
                      "total_number_of_animals","incidence_in_number_of_animals","BMR",
                      "BMD","BMDL","BMD/L_WIZARD_notes","action_taken","BMD'","BMDL'","BMD/L'_WIZARD_notes",
                      "comments","url","reference","document_name","toxval_units","toxval_type","UF")


  res1 <- res1[,c(1,3,4,8,37,38,5,6,7,9:11,12:36,39)]
  #print(View(res1))

  for(i in 1:length(res1$casrn)){
    res1$casrn[i] <- fix.casrn(res1$casrn[i],cname="",verbose=F)

  }
  
  res1$critical_effect <- paste(res1$effect, res1$effect_description, sep = ";")
  res1$toxval_type <- gsub("\\s+$","", res1$toxval_type)

  res <- res1
  
  names.list <- c("source_hash","casrn","name","toxval_numeric","toxval_units","toxval_type",
                  "subsource","reference","critical_effect",
                  "POD_numeric","POD_units","POD_type","UF","document_name","url")
  
  
  res <- res[,(names(res)%in% names.list)]
  print(names(res))
  
  res <- res[,c(1:7,13:15,8:12)]
  
  names(res)[names(res) == "reference"] <- "long_ref"
  names(res)[names(res)=="UF"] <- "numUF"

  #####################################################################
  cat("checks, finds and replaces non ascii characters in res with XXX\n")
  #####################################################################
  res <- fix.non_ascii(res)

  # trims leading and trailing whitespaces from the dataframe
  res <- data.frame(lapply(res, function(x) if(class(x)=="character") trimws(x) else(x)), stringsAsFactors=F, check.names=F)
  #print(str(res))

  res$numUFa <- as.numeric("")
  res$numUFh <- as.numeric("")
  res$numUFs <- as.numeric("")
  res$numUFl <- as.numeric("")
  res$numUFd <- as.numeric("")
  res$numUFother <- as.numeric("")
  res$year <- -1


  res$species <- "-"
  res$strain <- "-"
  res$sex <- "-"
  res$exposure_route <- "-"
  res$exposure_method <- "-"
  res$study_duration_value <- ""
  res$study_duration_units <- "-"


  res <- generate.originals(toxval.db,res)

  for(i in 1:nrow(res)) {
    val <- res[i,"long_ref"]
    for(year in 1980:2018) {
      syear <- as.character(year)
      if(contains(val,syear)) res[i,"year"] <- year
    }
  }



  #fix species
  # v8 has multiple field information that were not defined in source document, these field info extracted from long_ref
  # key word vector created based on v8
  res$species <- "-"
  sps <- c("rat","mouse","rabbit","dog","monkey","non-human primate","hamster","mice","rats","rabbits","dogs","hamsters")
  res$species <- str_extract_all(res$long_ref, regex(paste(sps, collapse = "|"), ignore_case = T)) %>%
    sapply(., paste, collapse = ", ")
  res$species <- tolower(res$species)
  res[grep("mouse", res$species),"species"] <- "mice"

  for (i in 1:nrow(res)){
    res$species[i] <- paste(unique(unlist(str_split(res$species[i], ", "))), collapse = ", ")

  }

  res[grep("^rat, mice$", res$species),"species"] <- "mice, rat"
  res[grep("^rat, dog$", res$species),"species"] <- "dog, rat"
  res[grep("^rat, rabbit$", res$species),"species"] <- "rabbit, rat"
  res[which(res$species == ""),"species"] <- "-"
  res$species_original <- res$species

  # fix strain
  res$strain <- "-"
  strain <- c("CD","Fischer 344","Sprague-Dawley","B6C3F1","Wistar","Albino","New Zealand","Beagle","CD-1","Long-Evans","Osborne-Mendel",
              "ICR","Crl:CD","Rhesus","JCL-ICR","CFE","NMRI","Sherman","Carworth farm","FDRL","CF-1","F344/N","F344/N/N",
              "B6C3F1/CrlBR","BDF1","C57/B1","CF-N","C57BL/6","Golden Syrian","Swiss","Balb/c","Harlan","Dutch","Sprague-Dawley (CD)")
  res$strain <- str_extract_all(res$long_ref, regex(paste(strain, collapse = "|"), ignore_case = T)) %>% sapply(., paste, collapse = ", ")
  res[which(res$strain == ""),"strain"] <- "-"

  # fix sex
  res$sex <- "-"
  res[grep("\\bFemale\\b", res$long_ref, ignore.case = T),"sex"] <- "Female"
  res[grep("\\bMale\\b", res$long_ref, ignore.case = T),"sex"] <- paste("Male", res[grep("\\bMale\\b", res$long_ref, ignore.case = T),"sex"] , sep = '/')
  res$sex <- gsub("Male\\/NA","Male",res$sex)
  res$sex <- tolower(res$sex)


  # fix exposure_route
  res$exposure_route <- "-"
  exp_route <- c("oral","inhalation")
  res$exposure_route <- str_extract_all(res$long_ref, regex(paste(exp_route, collapse = "|"), ignore_case = T)) %>%
    sapply(., paste, collapse = ", ")
  res$exposure_route <- tolower(res$exposure_route)
  res[grep("^oral\\, oral$", res$exposure_route),"exposure_route"] <- "oral"
  res[grep("^inhalation\\, inhalation$", res$exposure_route),"exposure_route"] <- "inhalation"
  res[which(res$exposure_route == ""),"exposure_route"] <- "-"


  # fix exposure_method
  res$exposure_method <- "-"
  exp_method <- c("\\bgavage\\b","\\bdiet\\b","\\bdrinking water\\b","\\bvapor\\b","\\bvapors\\b")
  res$exposure_method <- str_extract_all(res$long_ref, regex(paste(exp_method, collapse = "|"), ignore_case = T)) %>% sapply(., paste, collapse = ", ")
  res$exposure_method <- tolower(res$exposure_method)
  res[grep("^diet\\, diet$", res$exposure_method),"exposure_method"] <- "diet"
  res[grep("^vapors$", res$exposure_method),"exposure_method"] <- "vapor"
  res[which(res$exposure_method == ""),"exposure_method"] <- "-"


  # fix study_duration value and study_duration_units
  res$study_duration_value <- "-"
  res$study_duration_units <- "-"
  dur <- c("[a-zA-Z0-9]*\\-*\\s*[a-zA-Z0-9]+\\s+year","[a-zA-Z0-9]*\\-*\\s*[a-zA-Z0-9]+\\s+month","[a-zA-Z0-9]*\\-*\\s*[a-zA-Z0-9]+\\s+week","[a-zA-Z0-9]*\\-*\\s*[a-zA-Z0-9]+\\s+day","[a-zA-Z0-9]*\\-*\\s*[a-zA-Z0-9]+\\s+generations")
  res$study_duration_value <- str_extract_all(res$long_ref, regex(paste(dur, collapse = "|"), ignore_case = T)) %>% sapply(., paste, collapse = ", ")
  res$study_duration_value <- tolower(res$study_duration_value)
  res$study_duration_value <- gsub("(^for\\s+|^or\\s+|^to\\s+|^fore\\s+|^after\\s+|^a\\s+)(.*)","\\2",res$study_duration_value)
  res$study_duration_value <- gsub("(^\\s+)(.*)","\\2",res$study_duration_value)

  res$study_duration_units <- gsub("(.*\\s+)(\\w+$)","\\2", res$study_duration_value)
  res[which(res$study_duration_units == ""),"study_duration_units"] <- "-"

  res$study_duration_value <- gsub("(.*)(\\s+\\w+$)","\\1",res$study_duration_value)
  res$study_duration_value <- gsub("(.*)(\\s+consecutive$)","\\1",res$study_duration_value)

  res$study_duration_value <- mgsub(res$study_duration_value, replace_number(seq_len(100)), seq_len(100))

  res$study_duration_value <- as.numeric(res$study_duration_value)

  #print(dim(res))

  res$grouping_id <- c(1:dim(res)[1])
  #res$source_source_id <- as.integer("")

  #
  #Stack em
  names(res)[10:12] <- c("toxval_numeric","toxval_units","toxval_type")
  #res$grouping_id <- res$source_source_id

  #print(View(res))
  res.stack <- rbind(res[,c(4:6,1:3,9,13:31,33:34,36:42)],res[,c(10:12,1:3,9,13:31,33:34,36:42)])
  res <- res.stack
  #print(View(res))
  #####################################################################
  cat("add other columns to res\n")
  #####################################################################
  res <- unique(res)
  res$source <- source
  names(res)[names(res) == 'species'] <- 'species_original'
  res$details_text <- "Wignall DB details"
  res$human_eco <- "human health"
  res <- fill.toxval.defaults(toxval.db,res)
  #res <- generate.originals(toxval.db,res)
  res$source_url <- "https://ehp.niehs.nih.gov/doi/10.1289/ehp.1307539"
  if(is.element("species_original",names(res))) res[,"species_original"] <- tolower(res[,"species_original"])
  res$toxval_units_original <- res$toxval_units
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
  refs <- res[,c("toxval_id","source","year","long_ref","url","document_name")]

  #####################################################################
  cat("add extra columns to refs\n")
  #####################################################################
  refs$record_source_type <- "journal article"
  refs$record_source_note <- ""
  refs$record_source_level <- "primary"
  refs$url <- "https://ehp.niehs.nih.gov/doi/10.1289/ehp.1307539"

  #####################################################################
  cat("delete unused columns from res\n")
  #####################################################################
  res.uf <- res
  res <- res[,!is.element(names(res),c("long_ref","url","grouping_id","numUF","numUFa","numUFh","numUFs","numUFl","numUFd","numUFother","document_name"))]

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
  cat("perform extra steps if any\n")
  #####################################################################
  res.uf <- res.uf[order(res.uf$grouping_id),]

  res1 <- res.uf[,c("toxval_id","chemical_id","toxval_type","grouping_id",
                    "numUF","numUFa","numUFh","numUFs","numUFl","numUFd","numUFother")]

  resA <- res1[is.element(res1$toxval_type,c("RfD","RfC","Chronic RfD","Chronic Inhalation REL","Oral Slope Factor","Inhalation Unit Risk","Chronic Oral MRL","Chronic Inhalation MRL")),]
  resB <- res1[is.element(res1$toxval_type,c("NOAEL","LEL","LOAEL","BMCL","BMC","NOEL","BMDL","BMD")),]

   ### add NAs to shorter dataframe , so both dataframes have equal number of rows (using base R).

  resB <- resB[,c("toxval_id","toxval_type","grouping_id")]
  resA <- resA[,c("toxval_id","toxval_type","grouping_id","numUF","numUFa","numUFh","numUFs","numUFl","numUFd","numUFother")]
  bind_dfs <- function(a, b) {
    rows.a <- nrow(a)
    rows.b <- nrow(b)
    if (rows.a == rows.b) {
      cbind(b,a)
    } else if (rows.a > rows.b) {
      diff <- rows.a - rows.b
      df.na <- matrix(NA, diff, ncol(b))
      colnames(df.na) <- colnames(b)
      cbind(rbind(b, df.na),a)
    } else {
      diff <- rows.b - rows.a
      df.na <- matrix(NA, diff, ncol(a))
      colnames(df.na) <- colnames(a)
      cbind(b,rbind(a, df.na))
    }
  }

  resC <- bind_dfs(resA,resB)

  ### add NAs to shorter dataframe , so both dataframes have equal number of rows (using library gdata).

  # resB <- resB[,c("toxval_id","toxval_type","grouping_id")]
  # resA <- resA[,c("toxval_id","toxval_type","grouping_id","numUF","numUFa","numUFh","numUFs","numUFl","numUFd","numUFother")]
  # resC <- cbindX(resB,resA)

  ### Original toxval.load.wignall resC
  #resC <- cbind(resB[,c("toxval_id","toxval_type","grouping_id")],resA[,c("toxval_id","toxval_type","grouping_id","numUF","numUFa","numUFh","numUFs","numUFl","numUFd","numUFother")])

  names(resC) <- c("parent_id","toxval_typea","grouping_ida","toxval_id","toxval_typeb","grouping_idb","UCF.composite","UCF.interspecies","UCF.intraspecies","UCF.subchronic.to.chronic","UCF.LOAEL.vs.NOAEL","UCF.database.incomplete","UCF.other")
  name.list <- c("parent_id","toxval_id","UCF.composite","UCF.interspecies","UCF.intraspecies","UCF.subchronic.to.chronic","UCF.LOAEL.vs.NOAEL","UCF.database.incomplete","UCF.other")
  resC <- resC[,name.list]
  toxval_uf <- gather(resC, "uf_type","uf", 3:9, na.rm=TRUE)
  runInsertTable(toxval_uf, "toxval_uf", toxval.db)

  relationship <- rbind(
    data.frame(toxval_id_1 = toxval_uf$toxval_id, toxval_id_2 = toxval_uf$parent_id, relationship = "RfD derived from"),
    data.frame(toxval_id_1 = toxval_uf$parent_id, toxval_id_2 = toxval_uf$toxval_id, relationship = "used to derive RfD")
  )
  runInsertTable(relationship, "toxval_relationship", toxval.db, verbose)


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
