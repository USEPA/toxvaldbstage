#--------------------------------------------------------------------------------------
#' Do all of the post-processing steps for a source
#' @param toxval.db The database version to use
#' @param sourcedb The source database name
#' @param source The source name
#' @param do.convert.units If TRUE, convert uints, mainly from ppm to mg/kg-day. THis code is not debugged
#--------------------------------------------------------------------------------------
toxval.load.postprocess <- function(toxval.db, source.db,source, do.convert.units=F){
  printCurrentFunction(toxval.db)

  #####################################################################
  cat("check that the dictionaries are loaded\n")
  #####################################################################
  n1 = runQuery("select count(*) from toxval_dictionary",toxval.db)[1,1]
  n2 = runQuery("select count(*) from toxval_type_dictionary",toxval.db)[1,1]
  if(n1==0 || n2==0) import.dictionary(toxval.db)

  #####################################################################
  cat("load chemical info to source_chemical\n")
  #####################################################################
  toxval.load.source_chemical(toxval.db,source.db,source,verbose=T)

  #####################################################################
  cat("fill chemical by source\n")
  #####################################################################
  fill.chemical.by.source(toxval.db, source)

  #####################################################################
  cat("fix species by source\n")
  #####################################################################
  fix.species.v2(toxval.db, source)

  #####################################################################
  cat("fix human_eco by source\n")
  #####################################################################
  fix.human_eco.by.source(toxval.db, source, reset = T)

  #####################################################################
  cat("fix all.parameters(exposure_method, exposure_route, sex,strain,
    study_duration_class, study_duration_units, study_type,toxval_type,
    exposure_form, media, toxval_subtype) by source\n")
  #####################################################################
  if(source!="ECOTOX") fix.all.param.by.source(toxval.db, source)

  #####################################################################
  cat("get MW if needed for unit conversion\n")
  #####################################################################
  if(do.convert.units) toxval.set.mw(toxval.db, source)

  #####################################################################
  cat("fix units by source\n")
  #####################################################################
  fix.units.by.source(toxval.db, source,do.convert.units)

  #####################################################################
  cat("fix strain by source\n")
  #####################################################################
  fix.strain.v2(toxval.db, source)

  #####################################################################
  cat("fix priority_id by source\n")
  #####################################################################
  fix.priority_id.by.source(toxval.db, source)

  #####################################################################
  cat("fix critical_effect by source\n")
  #####################################################################
  doit = T
  if(is.element(source,c("ToxRefDB","ECOTOX"))) doit = F
  if(doit) fix.critical_effect.icf.by.source(toxval.db, source)

  #####################################################################
  cat("fix risk assessment class by source\n")
  #####################################################################
  fix.risk_assessment_class.by.source(toxval.db, source)

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
  fix.qc_status.by.source(toxval.db, source)

  #####################################################################
  #cat("set hash toxval by source\n")
  # currently not set - may not be needed for future dashboard releases
  #####################################################################
  #set.hash.toxval.by.source(toxval.db, source)
  #set.hash.record_source.by.source(toxval.db, source)
  #map.hash.record_source.by.source(toxval.db, source )

  #####################################################################
  cat("export by source\n")
  #####################################################################
  export.all.by.source(toxval.db, source)
}
