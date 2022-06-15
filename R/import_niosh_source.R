#--------------------------------------------------------------------------------------
#' Load niosh Source into dev_toxval_source_v4.
#' @param db The version of toxval into which the source is loaded.
#' @param infile The input file ./niosh/niosh_files/niosh_IDLH_2020.xlsx
#--------------------------------------------------------------------------------------
import_niosh_source <- function(db,
                                infile="../niosh/niosh_files/niosh_IDLH_2020.xlsx",
                                chem.check.halt=T) {
  printCurrentFunction(db)

  #####################################################################
  cat("Build new_niosh table\n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile)
  res["niosh_id"] <- c(1:length(res[,1]))
  res <- res[c('niosh_id', names(res[-8]))]

  #####################################################################
  cat("Do the chemical checking\n")
  #####################################################################
  source = "NIOSH"
  res = as.data.frame(res)
  res = res[res$casrn!="-",]
  res = res[res$casrn!="- ",]
  res$clowder_id = "-"
  res = fix.non_ascii.v2(res,source)
  res = source_chemical.process(db,res,source,chem.check.halt,casrn.col="casrn",name.col="name",verbose=F)

  #####################################################################
  cat("Set the default values for missing data\n")
  #####################################################################
  res = source_set_defaults(res,source)

  #####################################################################
  cat("Set the clowder_id and document name\n")
  #####################################################################
  res = set_clowder_id(res,source)

  #####################################################################
  cat("Build the hash key and load the data \n")
  #####################################################################
  toxval_source.hash.and.load(db,source,"new_niosh",F,F,res)
  browser()
  return(1)

  runInsertTable(res,"new_niosh",db,do.halt=T,verbose=F)
  # #####################################################################
  # cat("Build niosh_chemical_information table from res\n")
  # #####################################################################
  # chemical_information <- res[,2:3]
  # chemical_information <- unique(chemical_information[,1:2])
  # chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  # chemical_information <- chemical_information[c('chemical_id','name','casrn')]
  #
  # runInsertTable(chemical_information,"niosh_chemical_information",db,do.halt=T,verbose=F)


}

