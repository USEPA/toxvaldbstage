
#--------------------------------------------------------------------------------------
#' Load new_iris_noncancer and new_iris_cancer from toxval_source to toxval
#'
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param source.db The source database to use.
#' @param verbose If TRUE, print out extra diagnostic messages
#' @export
#--------------------------------------------------------------------------------------
toxval.load.iris = function(toxval.db,source.db, verbose=F){
  printCurrentFunction(toxval.db)
  
  #####################################################################
  cat("start output log, log files for each source can be accessed from output_log folder\n")
  #####################################################################
  source <- "IRIS"
  
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
  query <- "select * from new_iris"
  
  res <- runQuery(query,source.db,T,F)
  res <- res[ , !(names(res) %in% c("source_id","clowder_id"))]
  
  #####################################################################
  cat("checks, finds and replaces non ascii characters in res with XXX\n")
  #####################################################################
  res <- fix.non_ascii(res)
  
  # trims leading and trailing whitespaces from the dataframe
  res <- data.frame(lapply(res, function(x) if(class(x)=="character") trimws(x) else(x)), stringsAsFactors=F, check.names=F)
  #print(str(res))
  
  res$source <- "IRIS"
  res$subsource <- "EPA NCEA"
  res$toxval_numeric_qualifier <- "="
  res$source_url <- "https://cfpub.epa.gov/ncea/iris_drafts/simple_list.cfm"
  res$human_eco="human health"
  res <- res[!is.na(res[,"toxval_numeric"]),]
  res <- res[!is.na(res[,"casrn"]),]
  if(is.element("species_original",names(res))) res[,"species_original"] <- tolower(res[,"species_original"])

  #####################################################################
  cat("add other columns to res\n")
  #####################################################################
  res$source <- source
  res <- fill.toxval.defaults(toxval.db,res)
  res <- generate.originals(toxval.db,res)
  res$toxval_numeric_original <- res$toxval_numeric
  res[is.na(res[,"document_name"]),"document_name"] <- "-"
  res <- unique(res)
  #####################################################################
  cat("add toxval_id to res\n")
  #####################################################################
  res <- unique(res)
  count <- runQuery("select count(*) from toxval",toxval.db)[1,1]
  if(count==0) tid0 <- 1
  else tid0 <- runQuery("select max(toxval_id) from toxval",toxval.db)[1,1] + 1
  tids <- seq(from=tid0,to=tid0+nrow(res)-1)
  res$toxval_id <- tids
  res <- unique(res)

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
  refs <- res[,c("toxval_id","source","record_url","document_name")]
  names(refs)[3] <- "url"

  #####################################################################
  cat("add extra columns to refs\n")
  #####################################################################
  refs$record_source_type <- "url"
  refs$record_source_note <- "-"
  refs$record_source_level <- "primary (risk assessment values)"
  refs$year <- "2020"

  #####################################################################
  cat("delete unused columns from res\n")
  #####################################################################
  res <- res[,!is.element(names(res),c("record_url","document_name"))]

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
  temp1 <- runQuery("select a.source_casrn as casrn,b.toxval_id,b.toxval_units,b.exposure_route,b.critical_effect,b.risk_assessment_class
                    from source_chemical a, toxval b
                    where a.chemical_id=b.chemical_id
                    and b.source='IRIS'
                    and b.risk_assessment_class in ('acute','subchronic','chronic')
                    and b.toxval_type in ('RfD','RfC')",toxval.db,T,F)
  
  temp2 <- runQuery("select a.source_casrn as casrn,b.toxval_id,b.toxval_units,b.exposure_route,b.critical_effect,b.risk_assessment_class
                    from source_chemical a, toxval b
                    where a.chemical_id=b.chemical_id
                    and b.source='IRIS'
                    and b.risk_assessment_class in ('acute','subchronic','chronic')
                    and b.toxval_type not in ('RfD','RfC')",toxval.db,T,F)
  name.list <- names(temp1)
  name.list <- name.list[!is.element(name.list,"toxval_id")]
  temp3 <- merge(temp1,temp2,by=name.list)
  temp3a <- temp3
  temp4 <- temp3[,c("toxval_id.x","toxval_id.y")]
  temp4$relationship <- "RfD derived from"
  names(temp4)[1:2] <- c("toxval_id_1","toxval_id_2")
  temp5 <- temp3[,c("toxval_id.y","toxval_id.x")]
  temp5$relationship <- "used to derive RFD"
  names(temp5)[1:2] <- c("toxval_id_1","toxval_id_2")
  temp6 <- rbind(temp4,temp5)
  runInsertTable(temp6, "toxval_relationship", toxval.db,verbose)
  query <- "select casrn,name,record_url,System,critical_effect,PoD,uf_composite,confidence,
  toxval_units,rfd_type,exposure_route,risk_assessment_class, rfd_numeric, pod_type, pod_numeric
  from new_iris_noncancer" 
  noncancer <- runQuery(query,source.db,T,F)
  print(dim(noncancer))
  noncancer <- unique(noncancer)
  temp7 <- noncancer[,c("casrn","toxval_units","exposure_route","critical_effect","risk_assessment_class","uf_composite")]
  name.list <- c("casrn","toxval_units","exposure_route","critical_effect","risk_assessment_class")
  temp8 <- merge(temp3a,temp7,by=name.list)
  temp8 <- temp8[,c("toxval_id.x","toxval_id.y","uf_composite")]
  names(temp8) <- c("toxval_id","parent_id","uf")
  temp8$uf_type <- "composite"
  runInsertTable(temp8, "toxval_uf", toxval.db,verbose)

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
