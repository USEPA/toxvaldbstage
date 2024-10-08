
#-------------------------------------------------------------------------------------
#' Load ToxRefdb data to toxval
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param verbose Whether the loaded rows should be printed to the console.
#' @param log If TRUE, send output to a log file
#' @param do.init if TRUE, read the data in from the toxrefdb database and set up the matrix
#--------------------------------------------------------------------------------------
toxval.load.toxrefdb3 <- function(toxval.db,source.db,log=F,do.init=F) {
  printCurrentFunction(toxval.db)
  source <- "ToxRefDB"
  source_table = "direct load"
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
  if(!exists("TOXREFDB")) do.init=T
  if(do.init) {
    query <- "SELECT DISTINCT
    chemical.dsstox_substance_id AS dtxsid,
    chemical.casrn AS casrn,
    chemical.preferred_name AS name,
    chemical.chemical_id AS chemical_id,
    study.study_id AS study_id,
    study.study_type AS study_type,
    group_name AS phenotype,
    pod.qualifier AS toxval_numeric_qualifier,
    pod.pod_value AS toxval_numeric,
    pod.pod_unit AS toxval_units,
    pod.dose_level AS value_dose_level,
    pod.max_dose_level AS max_dose_level,
    pod.pod_type AS toxval_type,
    study.study_id AS source_study_id,
    study.study_citation AS long_ref,
    study.study_year AS year,
    study.study_source AS study_source,
    study.study_type AS study_type,
    study.study_type_guideline AS guideline,
    study.species AS species,
    study.strain_group AS strain_group,
    study.strain AS strain,
    study.admin_route AS exposure_route,
    study.admin_method AS exposure_method,
    study.substance_source_name AS substance_source_name,
    study.substance_purity AS substance_purity,
    study.substance_lot_batch AS substance_lot_batch,
    study.substance_comment AS substance_comment,
    study.dose_start AS dose_start,
    study.dose_start_unit as dose_start_unit,
    study.dose_end AS dose_end,
    study.dose_end_unit AS dose_end_unit,
    study.study_comment AS study_comment,
    effect.effect_desc AS critical_effect

    FROM
    prod_toxrefdb_2_0.study INNER JOIN prod_toxrefdb_2_0.chemical ON chemical.chemical_id=study.chemical_id
    INNER JOIN prod_toxrefdb_2_0.pod ON study.study_id=pod.study_id AND pod.chemical_id=study.chemical_id
    LEFT JOIN prod_toxrefdb_2_0.pod_tg_effect ON pod_tg_effect.pod_id=pod.pod_id
    LEFT JOIN prod_toxrefdb_2_0.tg_effect ON tg_effect.tg_effect_id=pod_tg_effect.tg_effect_id
    LEFT JOIN prod_toxrefdb_2_0.effect ON effect.effect_id=tg_effect.effect_id
    LEFT JOIN prod_toxrefdb_2_0.endpoint ON endpoint.endpoint_id=effect.endpoint_id
    LEFT JOIN prod_toxrefdb_2_0.effect_profile_group ON effect_profile_group.group_id=pod.group_id AND effect_profile_group.effect_profile_id=pod.effect_profile_id
    WHERE pod.effect_profile_id=1
    ORDER BY chemical.chemical_id,study.study_id,group_name,pod_type
    "
    #db <- "dev_toxrefdb_2_0"
    db <- "prod_toxrefdb_2_0"
    mat.in <- runQuery(query,db)

    name.list <- c(
      "dtxsid",
      "casrn",
      "name",
      "study_type",
      "toxval_type",
      "toxval_numeric_qualifier",
      "toxval_numeric",
      "toxval_units",
      "species",
      "strain",
      "exposure_route",
      "exposure_method",
      "dose_end",
      "dose_end_unit",
      "source_study_id",
      "long_ref",
      "year",
      "study_source",
      "guideline",
      "critical_effect"
    )
    print(name.list[!is.element(name.list,names(mat.in))])
    mat <- mat.in[,name.list]
    mat <- unique(mat)
    MAT <<- mat

    res <- unique(MAT)

    res2 <- res
    res2[,"critical_effect"] <- "-"
    res2 <- unique(res2)
    cat("Before removing critical effect: ",dim(res),"\n")
    cat("After removing critical effect: ",dim(res2),"\n")
    res2$critical_effect <- "-"

    n <- nrow(res2)
    for(i in 1:n) {
      row <- res2[i,]
      res3 <- res[which(res$dtxsid==res2[i,"dtxsid"] &
                          res$casrn==res2[i,"casrn"] &
                          res$name==res2[i,"name"] &
                          res$study_type==res2[i,"study_type"] &
                          res$toxval_type==res2[i,"toxval_type"] &
                          res$toxval_numeric_qualifier==res2[i,"toxval_numeric_qualifier"] &
                          res$toxval_numeric==res2[i,"toxval_numeric"] &
                          res$toxval_units==res2[i,"toxval_units"] &
                          res$species==res2[i,"species"] &
                          res$strain==res2[i,"strain"] &
                          res$exposure_route==res2[i,"exposure_route"] &
                          res$exposure_method==res2[i,"exposure_method"] &
                          res$dose_end==res2[i,"dose_end"] &
                          res$dose_end_unit==res2[i,"dose_end_unit"] &
                          res$source_study_id==res2[i,"source_study_id"] &
                          res$long_ref==res2[i,"long_ref"] &
                          res$year==res2[i,"year"]
      ),]
      res2[i,"critical_effect"] <- paste(res3[,"critical_effect"],collapse="|")
      if(i%%1000==0) cat("collapsed ",i," rows out of ",n,"\n")
    }
    TOXREFDB <<- res2
  }
  res = TOXREFDB

  cat("set the source_hash\n")
  res$source_hash = NA
  for (i in 1:nrow(res)){
    row <- res[i,]
    res[i,"source_hash"] = digest(paste0(row,collapse=""), serialize = FALSE)
    if(i%%1000==0) cat(i," out of ",nrow(res),"\n")
  }

  res$source = source

  res = source_chemical.toxrefdb(toxval.db,source.db,res,source,chem.check.halt=FALSE,
                                       casrn.col="casrn",name.col="name",verbose=F)
  #browser()
  nlist = names(res)
  nlist = nlist[!is.element(nlist,"dtxsid")]
  res = res[,nlist]
  res <- res[!is.na(res[,"toxval_units"]),]
  res[,"toxval_type"] <- toupper(res[,"toxval_type"])
  x <- res[,"toxval_numeric_qualifier"]
  x[is.element(x,"'='")] <- "="
  res[,"toxval_numeric_qualifier"] <- x

  x <- res[,"toxval_units"]
  x[is.element(x,"mg/kg/day")] <- "mg/kg-day"
  x[is.element(x,"mg/kg/wk")] <- "mg/kg-wk"
  x[is.element(x,"mg/m^3")] <- "mg/m3"
  x[is.element(x,"mg/L/day")] <- "mg/L"
  res[,"toxval_units"] <- x
  res[,"exposure_route"] <- tolower(res[,"exposure_route"])
  res[,"exposure_method"] <- tolower(res[,"exposure_method"])

  x <- res[,"study_type"]
  x[is.element(x,"DEV")] <- "developmental"
  x[is.element(x,"MGR")] <- "multigenerational reproductive"
  x[is.element(x,"CHR")] <- "chronic"
  x[is.element(x,"DNT")] <- "developmental neurotoxicity"
  x[is.element(x,"SUB")] <- "subchronic"
  x[is.element(x,"NEU")] <- "neurotoxicity"
  x[is.element(x,"REP")] <- "reproductive"
  x[is.element(x,"OTH")] <- "other"
  x[is.element(x,"SAC")] <- "subacute"
  x[is.element(x,"ACU")] <- "acute"
  res[,"study_type"] <- x

  x <- res[,"dose_end_unit"]
  x[is.element(x,"GD")] <- "days"
  x[is.element(x,"PND")] <- "days"
  x[is.element(x," day (PND)")] <- "days"
  x[is.element(x,"days (premating)")] <- "days"
  x[is.element(x,"weeks (premating)")] <- "weeks"
  res[,"dose_end_unit"] <- x
  res[,"dose_end"] <- as.numeric(res[,"dose_end"])

  name.list <- names(res)
  name.list[is.element(name.list,"dose_end_unit")] <- "study_duration_units"
  name.list[is.element(name.list,"dose_end")] <- "study_duration_value"
  names(res) <- name.list

  #####################################################################
  cat("Add the code from the original version from Aswani\n")
  #####################################################################
  #browser()
  cremove = c("study_source","chemical_index")
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

  # examples ...
  # names(res)[names(res) == "source_url"] = "url"
  # colnames(res)[which(names(res) == "phenotype")] = "critical_effect"

  #####################################################################
  cat("Generic steps \n")
  #####################################################################
  res = unique(res)
  res = res[!is.na(res$toxval_numeric),]
  res = res[res$toxval_numeric>0,]
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
  refs$record_source_type = "-"
  refs$record_source_note = "-"
  refs$record_source_level = "-"
  print(dim(res))

  #####################################################################
  cat("load res and refs to the database\n")
  #####################################################################
  res = unique(res)
  refs = unique(refs)
  res$datestamp = Sys.Date()
  res$source_table = source_table
  res$source_url = "-"
  res$subsource_url = "-"
  res$details_text = paste(source,"Details")
  #for(i in 1:nrow(res)) res[i,"toxval_uuid"] = UUIDgenerate()
  #for(i in 1:nrow(refs)) refs[i,"record_source_uuid"] = UUIDgenerate()
  runInsertTable(res, "toxval", toxval.db, verbose)
  runInsertTable(refs, "record_source", toxval.db, verbose)
  print(dim(res))

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

