#-------------------------------------------------------------------------------------
#' Do all of the fixes to units
#'
#' \enumerate{
#'   \item All of these steps operate on the toxval_units column.
#'   \item To allow this to be run multiple times during debugging, the first step
#'   is to copy toxval_units_original into toxval_units
#'   \item Replace variant unit names with standard ones, running fix.single.param.
#'   This fixes issues like variant names for mg/kg-day and uses the dictionary
#'   file dictionary/toxval_units_5.xlsx
#'   \item Fix special characters in toxval_units
#'   \item Fix issues with units containing extra characters for some ECOTOX records
#'   \item Convert units that are multiples of standard ones (e.g. ppb to ppm). This
#'   uses the dictionary file dictionary/toxval_units conversions 2018-09-12.xlsx
#'   \item Run conversions from molar to mg units, using MW. This uses the dictionary file
#'    dictionary/MW conversions.xlsx
#'   \item Convert ppm to mg/m3 for inhalation studies. This uses the conversion Concentration
#'   (mg/m3) = 0.0409 x concentration (ppm) x molecular weight. See
#'   https://cfpub.epa.gov/ncer_abstracts/index.cfm/fuseaction/display.files/fileID/14285.
#'   This function requires htat the DSSTox external chemical_id be set
#'   \item Convert ppm to mg/kg-day in toxval according to a species-specific
#'   conversion factor for oral exposures. This uses the dictionary file
#'   dictionary/ppm to mgkgday by animal.xlsx
#'   See: www10.plala.or.jp/biostatistics/1-3.doc
#'   This probbaly assumes feed rather than water
#'   \item Make sure that eco studies are in mg/L and human health in mg/m3
#' }
#'
#' @param toxval.db The version of toxvaldb to use.
#' @param do.convert If TRUE, so unit conversions, as opposed to just cleaning
#' @export
#-------------------------------------------------------------------------------------
fix.units <- function(toxval.db, do.convert=T) {
  printCurrentFunction(toxval.db)
  dsstox.db <- toxval.config()$dsstox.db
  #
  # first copy toxval_units_original to toxval_units, in case this method has already been run
  #
  runQuery("update toxval set toxval_units_original='-' where toxval_units_original=''",toxval.db)
  
  
  runQuery("update toxval set toxval_units=toxval_units_original",toxval.db)
  runQuery("update toxval set toxval_numeric=toxval_numeric_original",toxval.db)

  #
  # Remove special characters
  #
  cat(">>> Fix special characters in units\n")
  unit.list <- sort(runQuery("select distinct toxval_units_original from toxval group by toxval_units_original",toxval.db)[,1])
  for(i in 1:length(unit.list)) {
    input <- unit.list[i]
    output <- input
    temp <- str_replace_all(output,"\uFFFD","u")
    if(temp!=output) cat(output,":",temp,"\n")
    output <- temp
    output <- str_trim(output)
    query <- paste0("update toxval set toxval_units_original='",output,"' where toxval_units_original='",input,"'")
    #query <- paste0("update toxval set toxval_units_original='",output,"' where toxval_units_original='",input,"' and source not like 'ECOTOX'")
    runInsert(query,toxval.db,T,F,T)
  }
  #
  # Replace variant unit names with standard ones,

  cat(">>> Transform variant unit names to standard ones\n")
  fix.single.param(toxval.db,"toxval_units", ignore = F)

  #
  # Replace mg/kg with mg/kg-day where toxval type is NOAEL or NOEL
  #
  cat(">>> Replace mg/kg with mg/kg-day where toxval type is NOAEL or NOEL\n")
  query <- paste0("update toxval set toxval_units='mg/kg-day' where toxval_type in ('BMDL','BMDL05','BMDL10','HNEL','LOAEC','LOAEL','LOEC','LOEL','NEL','NOAEC','NOAEL','NOEC','NOEL') and toxval_units='mg/kg'")
  runInsert(query,toxval.db,T,F,T)

  #
  # Convert units to standard denominator (e.g. ppb to ppm by dividing by 1000)
  #
  if(do.convert) {
    c("   Convert units that are simple multiples of standard units\n")
    convos = read.xlsx(paste0(toxval.config()$datapath,"dictionary/toxval_units conversions 2018-09-12.xlsx"))
    nrows = dim(convos)[1]
    for (i in 1:nrows){
      query <- paste0("update toxval set toxval_units = '",convos[i,2],"', toxval_numeric = toxval_numeric*",convos[i,3]," where toxval_units = '",convos[i,1],"'")
      runInsert(query,toxval.db,T,F,T)
    }
  }
  #
  # Run conversions from molar to mg units, using MW
  #
  if(do.convert) {
    cat(">>> Run conversions from molar to mg units, using MW\n")
    convos <- read.xlsx(paste0(toxval.config()$datapath,"dictionary/MW conversions.xlsx"))
    nrows <- dim(convos)[1]
    for (i in 1:nrows){
      query <- paste0("
                      update ",toxval.db,".toxval
                      inner join ",toxval.db,".chemical on ",toxval.db,".chemical.chemical_id = ",toxval.db,".toxval.chemical_id
                      inner join ",dsstox.db,".source_substances on ",toxval.db,".chemical.chemical_id_external = ",dsstox.db,".source_substances.external_id
                      inner join ",dsstox.db,".source_generic_substance_mappings on ",dsstox.db,".source_generic_substance_mappings.fk_source_substance_id = ",dsstox.db,".source_substances.id
                      inner join ",dsstox.db,".generic_substance_compounds on ",dsstox.db,".generic_substance_compounds.fk_generic_substance_id = ",dsstox.db,".source_generic_substance_mappings.fk_generic_substance_id
                      inner join ",dsstox.db,".compounds on ",dsstox.db,".compounds.id = ",dsstox.db,".generic_substance_compounds.fk_compound_id
                      set toxval_units = '",convos[i,2],"', toxval_numeric = toxval_numeric*mol_weight
                      where toxval_units = '",convos[i,1],"'"
      )
      runInsert(query,toxval.db,T,F,T)
    }
  }
  #
  # Convert ppm to mg/m3 for inhalation studies
  #
  # https://cfpub.epa.gov/ncer_abstracts/index.cfm/fuseaction/display.files/fileID/14285
  if(do.convert) {
    cat("   Convert ppm to mg/m3 for inhalation studies\n")
    query <- paste0("
           update ",toxval.db,".toxval
           inner join ",toxval.db,".chemical on ",toxval.db,".chemical.chemical_id = ",toxval.db,".toxval.chemical_id
           inner join ",dsstox.db,".source_substances on ",toxval.db,".chemical.chemical_id_external = ",dsstox.db,".source_substances.external_id
           inner join ",dsstox.db,".source_generic_substance_mappings on ",dsstox.db,".source_generic_substance_mappings.fk_source_substance_id = ",dsstox.db,".source_substances.id
           inner join ",dsstox.db,".generic_substance_compounds on ",dsstox.db,".generic_substance_compounds.fk_generic_substance_id = ",dsstox.db,".source_generic_substance_mappings.fk_generic_substance_id
           inner join ",dsstox.db,".compounds on ",dsstox.db,".compounds.id = ",dsstox.db,".generic_substance_compounds.fk_compound_id
           set toxval_units = 'mg/m3', toxval_numeric = toxval_numeric*mol_weight*0.0409
           where toxval_units like 'ppm%' and exposure_route = 'inhalation'")
    runInsert(query,toxval.db)
  }
  #
  # Do the conversion from ppm to mg/kg-day on a species-wise basis for oral exposures
  #
  if(do.convert) {
    cat(">>> Do the conversion from ppm to mg/kg-day on a species-wise basis\n")
    conv = read.xlsx(paste0(toxval.config()$datapath,"dictionary/ppm to mgkgday by animal.xlsx"))
    for (i in 1: nrow(conv)){
      species <- conv[i,1]
      factor <- conv[i,2]
      sid.list <- runQuery(paste0("select species_id from species where species_common='",species,"'"),toxval.db)[,1]
      for(sid in sid.list) {
        cat(species, sid,":",length(sid.list),"\n")
        query = paste0("update toxval
                       set toxval_numeric = toxval_numeric_original*",
                       factor,
                       ", toxval_units = 'mg/kg-day'
                       where exposure_route='oral'
                       and toxval_units_original='ppm'
                       and species_id = ",sid,"" )
        runInsert(query,toxval.db,T,F,T)
      }
    }
  }
  #
  # make sure that eco studies are in mg/L and human health in mg/m3
  #
  if(do.convert) {
    cat(">>> Make sure that eco studies are in mg/L and human health in mg/m3\n")
    query <- "
    update toxval
    set toxval_numeric = toxval_numeric/1000, toxval_units = 'mg/L'
    where toxval_units = 'mg/m3' and human_eco = 'eco'"
    runInsert(query,toxval.db,T,F,T)

    query <- "
    update toxval set
    toxval_units = 'mg/m3', toxval_numeric = toxval_numeric*1000
    where toxval_units = 'mg/L' and human_eco = 'human health'"
    runInsert(query,toxval.db,T,F,T)
  }
}
