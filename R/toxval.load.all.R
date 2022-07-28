#--------------------------------------------------------------------------------------
#' Load and process all information into ToxValDB. The entire process can be run with
#' one command: toxval.load.all(toxval.db=...,source.db=..., do.all=T)
#' It can also be run in stages, but needs to be run in the order of the do.X parameters
#' listed here. If any earlier step is run, all of the subsequent steps need to be rerun.
#'
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @param source.db The version of toxvalsource database from which information is pulled.
#' @param log If TRUE write the output from each load script to a log file
#' @param do.init If True, clean out all of the database tables
#' @param do.reset If TRUE, empty the database to restart
#' @param do.load If TRUE, load all of the source
#' @export
#'
#--------------------------------------------------------------------------------------
toxval.load.all <- function(toxval.db,
                            source.db,
                            log=F,
                            do.init=F,
                            do.reset=F,
                            do.load=F) {
  printCurrentFunction(toxval.db)

  if(do.init)  {
    toxval.init.db(toxval.db,reset=do.reset)
  }

  if(do.load)  {
    # flex / actor
    # toxval.load.alaska_dec(toxval.db,source.db,log)
    # toxval.load.cal_dph(toxval.db,source.db,log)
    # toxval.load.epa_aegl(toxval.db,source.db,log)
    # toxval.load.fda_cedi(toxval.db,source.db,log)
    # toxval.load.mass_mmcl(toxval.db,source.db,log)
    # toxval.load.osha_air_limits(toxval.db,source.db,log)
    # toxval.load.ow_dwsha(toxval.db,source.db,log)
    # toxval.load.penn_dep(toxval.db,source.db,log)
    # toxval.load.usgs_hbsl(toxval.db,source.db,log)
    # toxval.load.who_ipcs(toxval.db,source.db,log)

    doit=T
    if(doit) {
      # toxval.load.atsdr.pfas.2021(toxval.db,source.db,log)
      # toxval.load.atsdr.pfas(toxval.db,source.db,log)
      # toxval.load.atsdr(toxval.db,source.db,log)
      # toxval.load.caloehha(toxval.db,source.db,log)
      # toxval.load.chiu(toxval.db,source.db,log)
      # toxval.load.copper(toxval.db,source.db,log)
      # toxval.load.cosmos(toxval.db,source.db,log)
      # toxval.load.dod.ered(toxval.db,source.db,log)
      # toxval.load.dod(toxval.db,source.db,log)
      # toxval.load.doe.benchmarks(toxval.db,source.db,log)
      # toxval.load.doe.ecorisk(toxval.db,source.db,log)
      # toxval.load.doe.pac(toxval.db,source.db,log)
      # toxval.load.efsa2(toxval.db,source.db,log)
      # toxval.load.efsa(toxval.db,source.db,log)
      # toxval.load.envirotox(toxval.db,source.db,log)
      # toxval.load.hawc_pfas_150(toxval.db,source.db,log)
      # toxval.load.hawc_pfas_430(toxval.db,source.db,log)
      # toxval.load.hawc(toxval.db,source.db,log)
      # toxval.load.healthcanada(toxval.db,source.db,log)
      # toxval.load.heast(toxval.db,source.db,log)
      # toxval.load.hess(toxval.db,source.db,log)
      # toxval.load.hpvis(toxval.db,source.db,log)
      # toxval.load.iris(toxval.db,source.db,log)
      # toxval.load.niosh(toxval.db,source.db,log)
      # toxval.load.opp(toxval.db,source.db,log)
      # toxval.load.oppt(toxval.db,source.db,log)
      # toxval.load.penn(toxval.db,source.db,log)
      # toxval.load.pfas_150_sem(toxval.db,source.db,log)
      # toxval.load.pfas_summary_pods(toxval.db,source.db,log)
      # toxval.load.pprtv.ncea(toxval.db,source.db,log)
      # toxval.load.pprtv.ornl(toxval.db,source.db,log)
      # toxval.load.rsl(toxval.db,source.db,log)
      # toxval.load.wignall(toxval.db,source.db,log)
      # toxval.load.toxrefdb3(toxval.db,source.db,log)
      toxval.load.echa.echemportal.api(toxval.db,source.db,log)
      toxval.load.ecotox(toxval.db,source.db,log)
      toxval.load.test(toxval.db,source.db,log)
    }
  }
}


