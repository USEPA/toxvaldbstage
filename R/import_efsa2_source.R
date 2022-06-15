#--------------------------------------------------------------------------------------
#' Load efsa2 Source into dev_toxval_source_v2.
#' @param db The version of toxval into which the source is loaded.
#' @param infile The input file ./efsa2/efsa2_files/merge2/EFSA_combined_new.xlsx
#--------------------------------------------------------------------------------------
import_efsa2_source <- function(db,
                                infile="../efsa2/efsa2_files/merge2/EFSA_combined_new.xlsx",
                                chem.check.halt=T) {
  printCurrentFunction(db)

  #####################################################################
  cat("Build new_efsa2 table\n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile)
  res <- lapply(res, function(x) type.convert(as.character(x),as.is = T))
  res <- data.frame(res,stringsAsFactors = F)
  res["new_efsa2_id"] <- c(1:length(res[,1]))
  res <- res[c('new_efsa2_id', names(res[-19]))]
  #####################################################################
  cat("Do the chemical checking\n")
  #####################################################################
  source = "EFSA2"
  res = as.data.frame(res)
  x = substr(res$casrn,1,5)
  mask = vector(length(x),mode="integer")
  mask[] = 1
  mask[x=="ACToR"] = 0
  res = res[mask==1,]

  res$clowder_id = "-"
  res = fix.non_ascii.v2(res,source)
  res = source_chemical.process(db,res,source,chem.check.halt,casrn.col="casrn",name.col="name",verbose=F)
  #####################################################################
  cat("Build the hash key and load the data \n")
  #####################################################################
  res = subset(res,select=-c(chemical_index))
  toxval_source.hash.and.load(db,source,"new_efsa2",F,F,res)
  browser()
  return(1)

  runInsertTable(res,"new_efsa2",db,do.halt=T,verbose=F)


  # #####################################################################
  # cat("Build efsa2_chemical_information table from res\n")
  # #####################################################################
  # chemical_information <- res[,2:3]
  # chemical_information <- unique(chemical_information[,1:2])
  # chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  # chemical_information <- chemical_information[c('chemical_id','name','casrn')]
  #
  # runInsertTable(chemical_information,"efsa2_chemical_information",db,do.halt=T,verbose=F)
  #
  #
}

