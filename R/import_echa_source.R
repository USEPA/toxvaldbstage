#--------------------------------------------------------------------------------------
#' Load ECHA Source from dev_toxval_source_v4(used in v8) saved as infile to dev_toxval_source_v4
#' @param db The version of toxval into which the source is loaded.
#' @param infile The input file ./echa/echa_files/echa_raw.xlsx
#--------------------------------------------------------------------------------------
import_echa_source <- function(db,
                               infile="../echa/echa_files/echa_raw.xlsx",
                               verbose = T,
                               chem.check.halt=T) {
  printCurrentFunction(db)

  #####################################################################
  cat("Build echa2 table\n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile, 1)

  #####################################################################
  cat("Do the chemical checking\n")
  #####################################################################
  source = "ECHA"
  res = as.data.frame(res)
  x = substr(res$casrn,1,5)
  mask = vector(length(x),mode="integer")
  mask[] = 1
  mask[x=="NOCAS"] = 0
  res = res[mask==1,]


  res$clowder_id = "-"
  res = fix.non_ascii.v2(res,source)
  res = source_chemical.process(db,res,source,chem.check.halt,casrn.col="casrn",name.col="name",verbose=F)
  #####################################################################
  cat("Build the hash key and load the data \n")
  #####################################################################
  res = subset(res,select=-c(chemical_index))
  toxval_source.hash.and.load(db,source,"echa2",F,F,res)
  browser()
  return(1)
  runInsertTable(res,"echa2",db,do.halt=T,verbose=F)

  # #####################################################################
  # cat("Build echa_chemical_information table from res\n")
  # #####################################################################
  # chemical_information <- res[,c("casrn","name")]
  # chemical_information <- unique(chemical_information[,1:2])
  # chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  # chemical_information <- chemical_information[c('chemical_id','name','casrn')]
  #
  # runInsertTable(chemical_information,"echa_chemical_information",db,do.halt=T,verbose=F)

}
