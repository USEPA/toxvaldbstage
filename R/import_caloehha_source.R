#--------------------------------------------------------------------------------------
#' @description Load caloehha Source file into toxval_source
#' The raw data can be exported as an Excel sheet from the web site
#' https://oehha.ca.gov/chemicals, selecting the link "Export database as .CSV file"
#'
#' This method parses that file and prepares for loading into toxval source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile The input file ="../caloehha/caloehha_files/OEHHA-chemicals_2018-10-30T08-50-47.xlsx",
#' @param chem.check.halt If TRUE and there are problems with chemicals CASRN checks, halt the program
#' @title FUNCTION_TITLE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[openxlsx]{read.xlsx}}
#' @rdname import_caloehha_source
#' @export 
#' @importFrom openxlsx read.xlsx
#--------------------------------------------------------------------------------------
import_caloehha_source <- function(db,
                                   infile="OEHHA-chemicals_2022-06-22T13-42-44.xlsx",
                                   chem.check.halt=F) {
  printCurrentFunction(db)

  #####################################################################
  cat("Build original_caloehha table from source file\n")
  #####################################################################
  indir = paste0(toxval.config()$datapath,"caloehha/caloehha_files/")
  infile = paste0(indir,infile)
  mat <- openxlsx::read.xlsx(infile)
  #print(names(mat))

  nlist = c(
    "CAS.Number",
    "Title",

    "Inhalation.Unit.Risk",
    "Inhalation.Unit.Risk.units",

    "Inhalation.Slope.Factor",
    "Inhalation.Slope.Factor.Units",

    "Oral.Slope.Factor",
    "Oral.Slope.Factor.Units",


    "Acute.REL.(μg/m3)",
    "Species",
    "Acute.REL.Toxicologic.Endpoint",
    "Acute.REL.Target.Organs",
    "Severity",
    "Last.Acute.REL.Revision",


    "8-Hour.Inhalation.REL.(μg/m3)",

    "Chronic.Inhalation.REL",
    "Chronic.Inhalation.REL.units",
    "Chronic.oral.REL",
    "Chronic.oral.REL.units",
    "Chronic.Toxicologic.Endpoint",
    "Chronic.Target.Organs",


    "MCL.value.(mg/L)",
    "MCL.value.units",

    "Public.Health.Goal",
    "Public.Health.Goal.Units",


    "No.Significant.Risk.Level.(NSRL).-.Inhalation",
    "NSRL.Inhalation.Units",
    "No.Significant.Risk.Level.(NSRL).-.Oral",
    "NSRL.Oral.Units",
    "Maximum.Allowable.Dose.Level.(MADL).for.chemicals.causing.reproductive.toxicity.-.Inhalation",
    "MADL.Inhalation.Units",
    "Maximum.Allowable.Dose.Level.(MADL).for.chemicals.causing.reproductive.toxicity.-.Oral",
    "MADL.Oral.Units",


    "Child-specific.refence.dose.(chRD)",
    "chRfD.units",

    "Last.Cancer.Potency.Revision",
    "Last.PHG.Revision",
    "Last.NSRL/MADL.Revision",
    "Last.chRD.revision"
  )

  mat = mat[,nlist]
  nlist = c("casrn","name",
            "inhalation_unit_risk","inhalation_unit_risk_units",
            "inhalation_slope_factor","inhalation_slope_factor_units",
            "oral_slope_factor","oral_slope_factor_units",
            "acute_rel","acute_rel_species","acute_rel_critical_effect","acute_rel_target_organ","acute_rel_severity","acute_rel_year",
            "rel_8_hour_inhalation",
            "chronic_inhalation_rel", "chronic_inhalation_rel_units","chronic_oral_rel","chronic_oral_rel_units","chronic_rel_critical_effect","chronic_rel_target_organ",
            "mcl","mcl_units",
            "phg","phg_units",
            "nsrl_inhalation","nsrl_inhalation_units","nsrl_oral","nsrl_oral_units","madl_inhlation_reprotox","madl_inhlation_reprotox_units","madl_oral_reprotox","madl_oral_reprotox_units",
            "chrfd","chrfd_units",
            "cancer_potency_year",
            "phg_year",
            "madl_nsrl_year",
            "chrd_year"
  )

  names(mat) = nlist
  mat$acute_rel_units = "um/m3"
  mat$rel_8_hour_inhalation_units = "um/m3"
  mat = mat[!generics::is.element(mat$casrn,"n/a"),]
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="Cal OEHHA",table="source_caloehha",res=mat,F,T,T)
}
