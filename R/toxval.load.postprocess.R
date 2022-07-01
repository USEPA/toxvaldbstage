#--------------------------------------------------------------------------------------
#' Do all of the post-processing steps for a source
#' @param toxval.db The database version to use
#' @param sourcedb The source database namename
#' @param source The source name
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
  runQuery(paste0("delete from source_chemical where source='",source,"'"),toxval.db)
  chems = runQuery(paste0("select * from source_chemical where source='",source,"'"),source.db)
  chems$casrn = chems$cleaned_casrn
  chems$name = chems$cleaned_cname
  runInsertTable(chems, "source_chemical", toxval.db)

  #####################################################################
  cat("map chemicals to dsstox\n")
  #####################################################################
  map.chemical.to.dtxsid(toxval.db, source)

  #####################################################################
  cat("get MW\n")
  #####################################################################
  toxval.set.mw(toxval.db, source)

  #####################################################################
  cat("fix species by source\n")
  #####################################################################
  # species_original
  # this is the more sophisticated species setting code
  fix.species.v2(toxval.db, source)
  #fix.species.ecotox.by.source(toxval.db, source)

  #####################################################################
  cat("fix human_eco by source\n")
  #####################################################################
  # human_eco
  fix.human_eco.by.source(toxval.db, source, reset = T)

  #####################################################################
  cat("fix all.parameters(exposure_method, exposure_route, sex,strain,
    study_duration_class, study_duration_units, study_type,toxval_type,
    exposure_form, media, toxval_subtype) by source\n")
  #####################################################################
  # exposure_route_original
  # exposure_method_original
  # sex_original
  # strain_original
  # study_duration_class_original
  # study_duration_units_original
  # study_type_original
  # toxval_type_original
  # toxval_subtype_original
  # media_original
  fix.all.param.by.source(toxval.db, source)

  #####################################################################
  cat("fix strain by source\n")
  #####################################################################
  fix.strain.v2(toxval.db, source)

  #####################################################################
  cat("fix exposure_form by source\n")
  #####################################################################
  # exposure_form_original
  fix.exposure_form.by.source(toxval.db, source)

  #####################################################################
  cat("fix exposure_route by type and source\n")
  #####################################################################
  fix.exposure_route.by.source(toxval.db, source)

  #####################################################################
  cat("fix toxval_numeric_qualifier by source\n")
  #####################################################################
  # toxval_numeric_qualifier_original
  fix.toxval_numeric_qualifier.by.source(toxval.db, source)

  #####################################################################
  cat("fix priority_id by source\n")
  #####################################################################
  fix.priority_id.by.source(toxval.db, source)

  #####################################################################
  cat("fix generation by source\n")
  #####################################################################
  fix.generation.by.source(toxval.db, source)

  #####################################################################
  cat("fix critical_effect by source\n")
  #####################################################################
  # critical_effect_original
  fix.critical_effect.icf.by.source(toxval.db, source)

  #####################################################################
  cat("fix units by source\n")
  #####################################################################
  # toxval_units_original
  fix.units.by.source(toxval.db, source,do.convert.units)

  #####################################################################
  cat("fill chemical by source\n")
  #####################################################################
  fill.chemical.by.source(toxval.db, source)

  #####################################################################
  cat("fix risk assessment class by source\n")
  #####################################################################
  # risk_assessment_class
  fix.risk_assessment_class.by.source(toxval.db, source)

  #####################################################################
  cat("export missing rac by source\n")
  #####################################################################
  #export.missing.rac.by.source(toxval.db, source)

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
  #####################################################################
  #set.hash.toxval.by.source(toxval.db, source)

  #####################################################################
  #cat("set hash record_source by source\n")
  #####################################################################
  #set.hash.record_source.by.source(toxval.db, source)

  #####################################################################
  #cat("map hash record_source by source\n")
  #####################################################################
  #map.hash.record_source.by.source(toxval.db, source )

  #####################################################################
  cat("export by source\n")
  #####################################################################
  export.all.by.source(toxval.db, source)
}
