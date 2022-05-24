
#--------------------------------------------------------------------------------------
#' Load new_heast_table and new_heast_rfd_rfc_table from toxval_source to toxval
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param source.db The source database to use.
#' @param verbose Whether the loaded rows should be printed to the console.
#' @export
#--------------------------------------------------------------------------------------
toxval.load.heast <- function(toxval.db,source.db,verbose=F) {
  printCurrentFunction(toxval.db)
  
  #####################################################################
  cat("start output log, log files for each source can be accessed from output_log folder\n")
  #####################################################################
  source <- "HEAST"

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
  
  query <- paste0("
                  select source_hash,casrn, name,toxval_type,toxval_numeric,toxval_units,
                  species,exposure_route,exposure_route_original,
                  exposure_method,exposure_method_original,study_duration_class,
                  study_duration_value,study_duration_value_original,study_duration_units,
                  study_duration_units_original,year,heast_id as source_source_id,critical_effect,
                  ornl_table as toxval_subtype
                  from new_heast
                  UNION
                  select h_rfd.source_hash,h_rfd.casrn,h_rfd.name,h_rfd.toxval_type,h_rfd.toxval_numeric,h_rfd.toxval_units,
                  species,exposure_route,exposure_route_original,exposure_method,exposure_method_original,
                  study_duration_class,study_duration_value,study_duration_value_original,study_duration_units,study_duration_units_original,year,
                  h_rfd.heast_id as grouping_id, critical_effect as phenotype, ornl_table
                  from new_heast_rfd_rfc h_rfd
                  left join new_heast on h_rfd.heast_id = new_heast.heast_id
                  ")
  
  res <- runQuery(query,source.db,T,F)
  #####################################################################
  cat("checks, finds and replaces non ascii characters in res with XXX\n")
  #####################################################################
  res <- fix.non_ascii(res)
  
  # trims leading and trailing whitespaces from the dataframe
  res <- data.frame(lapply(res, function(x) if(class(x)=="character") trimws(x) else(x)), stringsAsFactors=F, check.names=F)
  #print(str(res))
  
  #Fix NAs, long names, and uppercase
  for (i in c(6:12,15:17,19)){res[is.na(res[,i]),i] = "-"}

  res[nchar(res[,19])>255,19] <- substr(res[nchar(res[,19])>255,19],1,255)

  for (i in 7:12){res[,i] <- tolower(res[,i])}
  
  # fix casrn
  for(i in 1:nrow(res)) res[i,"casrn"] <- fix.casrn(res[i,"casrn"])
  

  
  #####################################################################
  cat("add other columns to res\n")
  #####################################################################
  res$source <- source
  res$subsource <- "EPA/ORNL/OLEM"
  res$details_text <- "HEAST details"
  res$source_url  <- "https://epa-heast.ornl.gov/heast.php"
  res$human_eco <- "human health"
  names(res)[is.element(names(res),"species")] <- "species_original"
  if(is.element("species_original",names(res))) res[,"species_original"] <- tolower(res[,"species_original"])
  res <- unique(res)
  res <- fill.toxval.defaults(toxval.db,res)
  res <- generate.originals(toxval.db,res)
  res$toxval_numeric_original <- res$toxval_numeric
  res <- unique(res)
  
  #####################################################################
  cat("map chemicals\n")
  #####################################################################
   
  
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
  refs <- res[,c("toxval_id","source","year")]

  #####################################################################
  cat("add extra columns to refs\n")
  #####################################################################
  refs$url <- "https://epa-heast.ornl.gov/heast.php"
  refs$record_source_type <- "website"
  refs$record_source_note <- "all values on this web site"
  refs$record_source_level <- "primary (risk assessment values)"

  #####################################################################
  cat("delete unused columns from res\n")
  #####################################################################

  #####################################################################
  cat("load res and refs to the database\n")
  #####################################################################
  res <- unique(res)
  refs <- unique(refs)
  for(i in 1:nrow(res)) res[i,"toxval_uuid"] <- UUIDgenerate()
  for(i in 1:nrow(refs)) refs[i,"record_source_uuid"] <- UUIDgenerate()
  runInsertTable(res, "toxval", toxval.db, verbose)
  runInsertTable(refs, "record_source", toxval.db, verbose)

  #####################################################################
  cat("perform extra steps if any\n")
  #####################################################################
  
  query2 = paste0("
                  select toxval.toxval_id, toxval.toxval_numeric, new_heast_rfd_rfc.toxval_numeric, heast_id, toxval_uf, case when toxval.toxval_type like 'RF%' then 1 else 0 end as state
                  from new_heast_rfd_rfc
                  left join ",toxval.db,".toxval on new_heast_rfd_rfc.heast_id = toxval.source_source_id
                  where source = 'HEAST'")
  
  res2 = runQuery(query2,source.db,T,F)

  res2[!(res2[,2]!=res2[,3] & res2[,6] ==1),] -> res2
  res2 <- res2[,c(1,3:6)]
  spread(res2, state, toxval_id)->res2

  #First the toxval_uf table
  
  
  res2$uf_type <- "UCF.composite"
  res2 <- res2[,3:6]
  names(res2)<- c("uf", "parent_id", "toxval_id", "uf_type")
  runInsertTable(res2, "toxval_uf", toxval.db)

  #Then the toxval_relationship table
  res2 = res2[,2:3]
  res2$relationship <- "RFD derived from"
  names(res2)<- c("toxval_id_2", "toxval_id_1", "relationship")
  runInsertTable(res2, "toxval_relationship", toxval.db)

  res2$relationship = "used to derive RFD"
  names(res2)<- c("toxval_id_1", "toxval_id_2", "relationship")
  runInsertTable(res2, "toxval_relationship", toxval.db)

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
