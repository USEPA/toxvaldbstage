#-------------------------------------------------------------------------------------
#' Load DOE Wildlife Benchmarks data from toxval_source to toxval
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param source.db The source database to use.
#' @param log If TRUE, send output to a log file
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

  # nlist = c("chemical_id","casrn","name","document_name","source","source_hash","qc_status","source_url",
  #           "endpoint_species",
  #           "wildlife_noael")
  # t3 = res[,nlist]
  # t3$toxval_type = "NOAEL"
  # t3$toxval_subtype = "wildlife_noael"
  # t3$toxval_units = "mg/kg-day"
  # t3$media = "food"
  # t3$exposure_route = "oral"
  # names(t3)[is.element(names(t3),"wildlife_noael")] = "toxval_numeric"
  # names(t3)[is.element(names(t3),"endpoint_species")] = "species_original"

  # nlist = c("chemical_id","casrn","name","document_name","source","source_hash","qc_status","source_url",
  #           "endpoint_species",
  #           "wildlife_noael")
  # t4 = res[,nlist]
  # t4$toxval_type = "NOAEL"
  # t4$toxval_subtype = "wildlife_noael"
  # t4$toxval_units = "mg/kg-day"
  # t4$media = "food"
  # t4$exposure_route = "oral"
  # names(t4)[is.element(names(t4),"wildlife_noael")] = "toxval_numeric"
  # names(t4)[is.element(names(t4),"endpoint_species")] = "species_original"

  # nlist = c("chemical_id","casrn","name","document_name","source","source_hash","qc_status","source_url",
  #           "endpoint_species",
  #           "noael_food")
  # t5 = res[,nlist]
  # t5$toxval_type = "NOAEL"
  # t5$toxval_subtype = "noael_food"
  # t5$toxval_units = "mg/kg-day"
  # t5$media = "food"
  # t5$exposure_route = "oral"
  # names(t5)[is.element(names(t5),"noael_food")] = "toxval_numeric"
  # names(t5)[is.element(names(t5),"endpoint_species")] = "species_original"

  # nlist = c("chemical_id","casrn","name","document_name","source","source_hash","qc_status","source_url",
  #           "endpoint_species",
  #           "loael_food")
  # t6 = res[,nlist]
  # t6$toxval_type = "LOAEL"
  # t6$toxval_subtype = "loael_food"
  # t6$toxval_units = "mg/kg-day"
  # t6$media = "food"
  # t6$exposure_route = "oral"
  # names(t6)[is.element(names(t6),"loael_food")] = "toxval_numeric"
  # names(t6)[is.element(names(t6),"endpoint_species")] = "species_original"

  # nlist = c("chemical_id","casrn","name","document_name","source","source_hash","qc_status","source_url",
  #           "endpoint_species",
  #           "noael_water")
  # t7 = res[,nlist]
  # t7$toxval_type = "NOAEL"
  # t7$toxval_subtype = "noael_water"
  # t7$toxval_units = "mg/kg-day"
  # t7$media = "water"
  # t7$exposure_route = "oral"
  # names(t7)[is.element(names(t7),"noael_water")] = "toxval_numeric"
  # names(t7)[is.element(names(t7),"endpoint_species")] = "species_original"

  # nlist = c("chemical_id","casrn","name","document_name","source","source_hash","qc_status","source_url",
  #           "endpoint_species",
  #           "loael_water")
  # t8 = res[,nlist]
  # t8$toxval_type = "LOAEL"
  # t8$toxval_subtype = "loael_water"
  # t8$toxval_units = "mg/kg-day"
  # t8$media = "water"
  # t8$exposure_route = "oral"
  # names(t8)[is.element(names(t8),"loael_water")] = "toxval_numeric"
  # names(t8)[is.element(names(t8),"endpoint_species")] = "species_original"

  # nlist = c("chemical_id","casrn","name","document_name","source","source_hash","qc_status","source_url",
  #           "endpoint_species",
  #           "noael_piscivore")
  # t9 = res[,nlist]
  # t9$toxval_type = "NOAEL"
  # t9$toxval_subtype = "noael_piscivore"
  # t9$toxval_units = "mg/l"
  # t9$media = "piscivore"
  # t9$exposure_route = "oral"
  # names(t9)[is.element(names(t9),"noael_piscivore")] = "toxval_numeric"
  # names(t9)[is.element(names(t9),"endpoint_species")] = "species_original"

  # nlist = c("chemical_id","casrn","name","document_name","source","source_hash","qc_status","source_url",
  #           "endpoint_species",
  #           "loael_piscivore")
  # t10 = res[,nlist]
  # t10$toxval_type = "LOAEL"
  # t10$toxval_subtype = "loael_piscivore"
  # t10$toxval_units = "mg/l"
  # t10$media = "piscivore"
  # t10$exposure_route = "oral"
  # names(t10)[is.element(names(t10),"loael_piscivore")] = "toxval_numeric"
  # names(t10)[is.element(names(t10),"endpoint_species")] = "species_original"


  #res1 = rbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10)
  res1 = rbind(t1,t2)

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
  res = res[!is.na(res$toxval_numeric),]
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
}
