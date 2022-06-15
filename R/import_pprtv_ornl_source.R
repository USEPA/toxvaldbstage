library(openxlsx)
#--------------------------------------------------------------------------------------
#' Load pprtv_ornl Source into dev_toxval_source_v2.
#' @param db The version of toxval into which the source is loaded.
#' @param infile The input file ./pprtv_ornl/pprtv_ornl_files/new_PPRTV_ORNL cancer noncancer.xlsx
#--------------------------------------------------------------------------------------
import_pprtv_ornl_source <- function(db,
                                     infile="../pprtv_ornl/pprtv_ornl_files/new_PPRTV_ORNL cancer noncancer.xlsx",
                                     chem.check.halt=F) {
  printCurrentFunction(db)

  #####################################################################
  cat("Build new_pprtv_ornl table\n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile)
  res["pprtv_ornl_id"] <- c(1:length(res[,1]))
  res <- res[c('pprtv_ornl_id', names(res[-23]))]

  #####################################################################
  cat("Do the chemical checking\n")
  #####################################################################
  source = "PPRTV (ORNL)"
  res = as.data.frame(res)
  res = res[!is.element(res$casrn,c("VARIOUS","MULTIPLE")),]
  res$clowder_id = "-"
  res = fix.non_ascii.v2(res,source)
  res = source_chemical.process(db,res,source,chem.check.halt,casrn.col="casrn",name.col="name",verbose=F)
  #####################################################################
  cat("Build the hash key and load the data \n")
  #####################################################################
  res = subset(res,select=-c(chemical_index))
  toxval_source.hash.and.load(db,source,"new_pprtv_ornl",F,F,res)
  browser()
  return(1)
  runInsertTable(res,"new_pprtv_ornl",db,do.halt=T,verbose=F)



  # #####################################################################
  # cat("Build pprtv_ornl_chemical_information table from res\n")
  # #####################################################################
  # chemical_information <- res[,2:3]
  # chemical_information <- unique(chemical_information[,1:2])
  # chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  # chemical_information <- chemical_information[c('chemical_id','name','casrn')]
  #
  # runInsertTable(chemical_information,"pprtv_ornl_chemical_information",db,do.halt=T,verbose=F)


}

