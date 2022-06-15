#--------------------------------------------------------------------------------------
#' Load and process all information into ToxValDB. The entire process can be run with
#' one command: toxval.load.all(toxval.db=...,source.db=..., do.all=T)
#' It can also be run in stages, but needs to be run in the order of the do.X parameters
#' listed here. If any earlier step is run, all of the subsequent steps need to be rerun.
#'
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param source.db The version of toxvalsource database from which information is pulled.
#' @param do.init If True, clean out all of the database tables
#' @param do.dict If TRUE,  the initial dictionaries
#' @param do.load If TRUE, load all of the in vivo data
#' @param do.load.chemical If TRUE, load the chemical lists
#' @param do.dsstox, If TRUE, do the mapping to DSSTox to get the DSSTOx generic substance IDs
#' @param do.fix.species If TRUE, update all ofthe species information
#' @param do.fix.1 If TRUE, clean up a series of database fields using dictionaries
#' @param do.fix.units If TRUE, fix the toxval_units field
#' @param fix.final If TRUE, run the final steps, including setting the record_id field
#' @param do.all If TRUE, run all steps.
#' @param do.post.load If TRUE, to all steps past loading
#' @param do.hash If TRUE, set all of the hashes
#' @param verbose if TRUE, print disgnostic messages along the way
#' @param reset.hash If TRUE, reset the hashes instead of only setting values ='-'
#' @param do.export If TRUE, run all export scripts
#' @export
#'
#--------------------------------------------------------------------------------------
toxval.load.all <- function(toxval.db,
                            source.db ,
                            do.init=F,
                            do.dict=F,
                            do.load=F,
                            do.load.chemical=F,
                            do.dsstox=F ,
                            do.fix.species=F,
                            do.fix.1=F,
                            do.fix.units=F,
                            do.fix.final=F,
                            do.all = F,
                            do.post.load = F,
                            do.hash=T,
                            verbose=T,
                            reset.hash=T,
                            do.export=F) {
  printCurrentFunction(toxval.db)

  if(do.all || do.init)  {
    toxval.load.initial(toxval.db)
  }
  if(do.all || do.dict)  {
    import.source.info(toxval.db)
    import.dictionary(toxval.db)
  }

  
  if(do.all || do.load)  {
    toxval.load.atsdr(toxval.db,source.db,verbose)
    toxval.load.atsdr.pfas(toxval.db,source.db, verbose)
    toxval.load.atsdr.pfas.2021(toxval.db,source.db, verbose)
    toxval.load.caloehha(toxval.db,source.db,verbose)
    toxval.load.chiu(toxval.db,source.db,verbose)
    toxval.load.copper(toxval.db,source.db, verbose)
    toxval.load.cosmos(toxval.db,source.db,verbose)
    toxval.load.dod(toxval.db,source.db,verbose)
    toxval.load.dod.ered(toxval.db,source.db,verbose)
    toxval.load.doe(toxval.db,source.db,verbose)
    toxval.load.echa(toxval.db,source.db,verbose)
    toxval.load.echa.echemportal(toxval.db,source.db,verbose)
    toxval.load.echa.echemportal.api(toxval.db,source.db,verbose)
    toxval.load.echa.iuclid(toxval.db,source.db,verbose)
    toxval.load.echa3(toxval.db,source.db,verbose)
    toxval.load.ecotox(toxval.db,verbose,do.load=T)
    toxval.load.efsa(toxval.db,source.db,verbose)
    toxval.load.efsa2(toxval.db,source.db,verbose)
    toxval.load.flex(toxval.db,verbose,only.new=F) # Alaska, Cal DPH, DOD MEG, DOE PAC, EPA AEGL, FDA CEDI, Maaa MMCL, OEHHA, OSHA Air limits, Penn DEP, USGS NAWQA, USGL HBSL, WHO IPCS
    toxval.load.hawc(toxval.db,source.db,verbose)
    toxval.load.hawc.pfas.150(toxval.db,source.db,verbose)
    toxval.load.hawc.pfas.430(toxval.db,source.db,verbose)
    toxval.load.healthcanada(toxval.db,source.db,verbose)
    toxval.load.heast(toxval.db,source.db,verbose)
    toxval.load.hess(toxval.db,source.db,verbose)
    toxval.load.hpvis(toxval.db,source.db,verbose)
    toxval.load.iris(toxval.db,source.db,verbose)
    toxval.load.lanl(toxval.db,source.db,verbose)
    toxval.load.niosh(toxval.db,source.db,verbose)
    toxval.load.opp(toxval.db,source.db,verbose)
    toxval.load.oppt(toxval.db,source.db,verbose)
    toxval.load.penn(toxval.db,source.db,verbose)
    toxval.load.pfas.150.sem(toxval.db,source.db,verbose)
    toxval.load.pfas.summary.pods(toxval.db,source.db,verbose)
    toxval.load.pprtv.ncea(toxval.db,source.db,verbose)
    toxval.load.pprtv.ornl(toxval.db,source.db,verbose)
    toxval.load.rsl(toxval.db,source.db,verbose)
    toxval.load.test(toxval.db,source.db,verbose)
    toxval.load.toxrefdb3(toxval.db,verbose,do.init=T)
    toxval.load.wignall(toxval.db,source.db,verbose)
    #############################################################################
    toxval.load.cancer(toxval.db)
    toxval.load.skin.eye(toxval.db,verbose)
    toxval.load.genetox(toxval.db, verbose,do.read=T)
    toxval.load.genetox_details(toxval.db,verbose)
    toxval.load.bcfbaf(toxval.db,verbose)
  }
  if(do.all || do.load.chemical) {
    toxval.load.chemical.list(toxval.db,verbose=T)
  }
  if(do.all || do.dsstox || do.post.load) {
    cat("=================================================\n")
    cat("Update the links to DSSTox\n")
    cat("=================================================\n")
    runQuery("analyze table toxval",toxval.db)
    map.chemical.to.dsstox(toxval.db,verbose=T)
    table.list <- c("toxval","cancer_summary","genetox_summary","genetox_details","skin_eye","chemical_list","bcfbaf")
    for(table in table.list) set.dtxsid(toxval.db,table)
  }
  if(do.all || do.fix.species || do.post.load) {
    cat("=================================================\n")
    cat("Run the species fixes\n")
    cat("=================================================\n")
    runQuery("analyze table toxval",toxval.db)
    fix.species(toxval.db)
    fix.human_eco(toxval.db)
  }
  if(do.all || do.fix.1 || do.post.load) {
    cat("=================================================\n")
    cat("Run the first fixes\n")
    cat("=================================================\n")
    runQuery("analyze table toxval",toxval.db)
    #reset.numeric(toxval.db)
    fix.toxval_numeric_qualifier(toxval.db)
    fix.exposure_route.by.type.new(toxval.db)
    fix.exposure_form(toxval.db)
    fix.priority_id(toxval.db)
    fix.all.param.new(toxval.db)
    fix.critical_effect.icf(toxval.db)
  }
  if(do.all || do.fix.units || do.post.load) {
    cat("=================================================\n")
    cat("Fix units\n")
    cat("=================================================\n")
    fix.units.new(toxval.db,do.convert=F)
  }
  if(do.all || do.fix.final || do.post.load) {
    cat("=================================================\n")
    cat("Fix risk_assessment_class\n")
    cat("=================================================\n")
    fix.risk_assessment_class(toxval.db,restart=F)
    fill.chemical(toxval.db,verbose=T)
    export.missing.rac(toxval.db)
    runQuery("analyze table toxval",toxval.db)
    fix.empty(toxval.db)
    fix.empty.record_source(toxval.db)
    fill.toxval.defaults.global(toxval.db)
    fix.qa_status(toxval.db)
  }
  if(do.all || do.hash || do.post.load) {
    set.hash.toxval(toxval.db,reset.hash)
    set.hash.skin_eye(toxval.db,reset.hash)
    set.hash.record_source(toxval.db,reset.hash)
    map.hash.record_source(toxval.db)
    set.hash.genetox_details(toxval.db,reset.hash)
    set.hash.bcfbaf(toxval.db,reset.hash)
  }
  if(do.export) {
    dir = paste0("./export/",Sys.Date())
    dir.create(dir,showWarnings=F)
    export.all.with.references(toxval.db,dir=dir)
    export.pod.summary(toxval.db,human_eco='human health',dir=dir)
    export.pod.summary(toxval.db,human_eco='eco',dir=dir)
    export.bcfbaf(toxval.db,dir=dir)
    export.cancer.summary(toxval.db,dir=dir)
    export.genetox_details(toxval.db,dir=dir)
    export.genetox_summary(toxval.db,dir=dir)
    export.record_source(toxval.db,dir=dir)
    export.skin_eye(toxval.db,dir=dir)
    export.toxval_dictionary(toxval.db,dir=dir)
  }
}


