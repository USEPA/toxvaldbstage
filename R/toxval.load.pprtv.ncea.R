
#-------------------------------------------------------------------------------------
#' Load pprtv from dev_pprtv to toxval
#' There is a known bug here - some of the POD values are repeated because they produce
#' two kinds of RfD values (chronic and subchronic) - dealing with htis will require
#' some work
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param source.db The version of toxval_source from which the tables are loaded.
#' @param verbose Whether the loaded rows should be printed to the console.
#' @export
#--------------------------------------------------------------------------------------
toxval.load.pprtv.ncea <- function(toxval.db, source.db, verbose=F){
  
  #####################################################################
  cat("start output log, log files for each source can be accessed from output_log folder\n")
  #####################################################################
  source <- "PPRTV (NCEA)"
  
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
  printCurrentFunction(toxval.db)
  query <- "select source_hash,casrn, name, RFV_ID, toxval_type, toxval_numeric, toxval_units, study_type, toxval_subtype,
  phenotype, POD_numeric, POD_type, POD_units, UF_A, UF_D, UF_H, UF_L, UF_S, UF_C, year, author,
  title, long_ref, species, strain, sex, exposure_route, exposure_method, study_duration_class, 
  study_duration_value, study_duration_units, document_name from new_pprtv_ncea"
  res <- runQuery(query,source.db,T,F)
  #####################################################################
  cat("checks, finds and replaces non ascii characters in res with XXX\n")
  #####################################################################
  res <- fix.non_ascii(res)

  
  names(res)[4] <- "source_source_id"
  res <- unique(res)

  #####################################################################
  cat("add other columns to res\n")
  #####################################################################
  res$source <- source
  res[res$name == "Inorganic Phosphates",4] = "98059-61-1"
  names(res)[is.element(names(res),"phenotype")] <- "critical_effect"
  res$subsource <- "EPA ORD"
  name.list <- names(res)
  name.list[is.element(name.list,"species")] <- "species_original"
  names(res) <- name.list
  if(is.element("species_original",names(res))) res[,"species_original"] <- tolower(res[,"species_original"])
  res1 <- res[,!is.element(names(res),c("POD_numeric","POD_type","POD_units"))]
  res2 <- res[,!is.element(names(res),c("toxval_numeric","toxval_type","toxval_units"))]
  names(res2)[is.element(names(res2),"POD_numeric")] <- "toxval_numeric"
  names(res2)[is.element(names(res2),"POD_type")] <- "toxval_type"
  names(res2)[is.element(names(res2),"POD_units")] <- "toxval_units"
  res2 <- res2[,names(res1)]
  res <- rbind(res1,res2)
  res = fill.toxval.defaults(toxval.db,res)
  res = generate.originals(toxval.db,res)

  #####################################################################
  cat("***** Fix the uncertainty factors - something is wrong with these in the source\n")
  #####################################################################
  col.list <- c("UF_A","UF_D","UF_H","UF_L","UF_S","UF_C")
  for(col in col.list) res[,col] <- -1
  res[,"source_source_id"] <- -1
  res[is.na(res$critical_effect),"critical_effect"] <- "-"
  res[is.element(res$critical_effect,""),"critical_effect"] <- "-"
  res[is.na(res$toxval_subtype),"toxval_subtype"] <- "-"
  res[is.element(res$toxval_subtype,""),"toxval_subtype"] <- "-"
  res[is.na(res$toxval_subtype_original),"toxval_subtype_original"] <- "-"
  res[is.element(res$toxval_subtype_original,""),"toxval_subtype_original"] <- "-"
  res$source_url <- "https://www.epa.gov/pprtv/basic-information-about-provisional-peer-reviewed-toxicity-values-pprtvs"
  
  res$toxval_numeric <- as.numeric(res$toxval_numeric)
  res[grep("\\-", res$study_duration_value),"study_duration_value"] <- gsub("(\\d+\\s*)(\\-\\s*)(\\d+)","\\3",res[grep("\\-", res$study_duration_value),"study_duration_value"])
  res$study_duration_value <- as.numeric(res$study_duration_value)
  # trims leading and trailing whitespaces from the dataframe
  res <- data.frame(lapply(res, function(x) if(class(x)=="character") trimws(x) else(x)), stringsAsFactors=F, check.names=F)
  #print(str(res))
  res$toxval_numeric_original <- res$toxval_numeric
  res[is.na(res[,"document_name"]),"document_name"] <- "-"
  
  res <- unique(res)
  #####################################################################
  cat("add toxval_id to res\n")
  #####################################################################

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
  #res <- merge(res,cid.list)
  res <- res[,!is.element(names(res),c("casrn","name"))]


  #####################################################################
  cat("pull out record source to refs\n")
  #####################################################################
  refs <- res[,c("toxval_id","source","author","long_ref","year","title","document_name")]

  #####################################################################
  cat("add extra columns to refs\n")
  #####################################################################
  refs$record_source_type <- "journal article"
  refs$record_source_note <- "All data is on single web page for ATSDR"
  refs$record_source_level <- "primary"

  #####################################################################
  cat("delete unused columns from res\n")
  #####################################################################
  ufs <- res[,c("toxval_id","UF_A","UF_D","UF_H","UF_L","UF_S","UF_C")]
  res <- res[,!is.element(names(res),c("author","title","long_ref","UF_A","UF_D","UF_H","UF_L","UF_S","UF_C","document_name"))]

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

  toxids <- runQuery("select toxval_id, source_source_id from toxval where source = 'PPRTV (NCEA)' and toxval_type like '%Reference%'",toxval.db,T,F)
  parids <- runQuery("select toxval_id as parent_id, source_source_id from toxval where source = 'PPRTV (NCEA)' and toxval_type not like '%Reference%'",toxval.db,T,F)
  toxval_uf <- merge(toxids, parids)
  toxval_uf <- merge(toxval_uf, unique(ufs))
  names(toxval_uf)[4:9] <- c("UCF.interspecies","UCF.database.incomplete","UCF.intraspecies","UCF.LOAEL.vs.NOAEL","UCF.subchronic.to.chronic","UCF.composite")

  toxval_uf <- gather(toxval_uf, "uf_type","uf",4:9)
  toxval_uf <- toxval_uf[,c("toxval_id","parent_id","uf_type","uf")]
  toxval_uf <- toxval_uf[!is.na(toxval_uf$uf),]
  runInsertTable(toxval_uf,"toxval_uf",toxval.db)
  toxval_relationship <- merge(toxids, parids)

  toxval_relationship <- rbind(
    data.frame(toxval_id_1 = toxval_relationship$toxval_id,
               toxval_id_2 = toxval_relationship$parent_id,
               relationship = "RFD derived from"
    ),
    data.frame(toxval_id_1 = toxval_relationship$parent_id,
               toxval_id_2 = toxval_relationship$toxval_id,
               relationship = "used to derive RFD"
    )
  )
  runInsertTable(toxval_relationship,"toxval_relationship",toxval.db)
  
  
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
