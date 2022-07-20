library("openxlsx")
library("stringr")
#--------------------------------------------------------------------------------------
#' Load ECHA IUCLID Source into dev_toxval_source_v4.
#' @param db The version of toxval into which the source is loaded.
#' @param infile The input file ./echa_iuclid/echa_iuclid_files/echa_iuclid_v8.xlsx

#--------------------------------------------------------------------------------------
import_echa_iuclid_source <- function(db,
                                      infile="../echa_iuclid/echa_iuclid_files/echa_iuclid_v8.xlsx",
                                      verbose = T,
                                      chem.check.halt=T) {
  printCurrentFunction(db)

  #####################################################################
  cat("Build new_echa_iuclid table\n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile)

  #####################################################################
  cat("Do the chemical checking\n")
  #####################################################################
  source = "ECHA IUCLID"
  res = as.data.frame(res)
  temp = res$casrn
  temp=substr(temp,1,5)
  mask = vector(length=nrow(res),mode="integer")
  mask[] = 1
  mask[is.element(temp,"NOCAS")] = 0
  res = res[mask==1,]
  res = res[res$casrn!="unknown",]

  res$clowder_id = "-"
  res = fix.non_ascii.v2(res,source)
  res = source_chemical.process(db,res,source,chem.check.halt,casrn.col="casrn",name.col="name")
  #####################################################################
  cat("Build the hash key and load the data \n")
  #####################################################################
  res = subset(res,select=-c(chemical_index))
  toxval_source.hash.and.load(db,source,"new_echa_iuclid",F,F,res)
  browser()
  return(1)

  #
  #
  #
  # #####################################################################
  # cat("Do the chemical checking\n")
  # #####################################################################
  # source = "ECHA IUCLID"
  # res = as.data.frame(res)
  # temp = res$casrn
  # temp=substr(temp,1,5)
  # mask = vector(length=nrow(res),mode="integer")
  # mask[] = 1
  # mask[is.element(temp,"NOCAS")] = 0
  # res = res[mask==1,]
  # res = res[res$casrn!="unknown",]
  # res = fix.non_ascii.v2(res,source)
  # res$chemical_index = paste(res$casrn,res$name)
  # result = chem.check(res,name.col="name",casrn.col="casrn",verbose=F,source)
  # if(chem.check.halt) if(!result$name.OK || !result$casrn.OK || !result$checksum.OK) browser()
  #
  # #####################################################################
  # cat("Build the chemical table\n")
  # #####################################################################
  # chems = cbind(res[,c("casrn","name")],result$res0[,c("casrn","name")])
  # names(chems) = c("raw_casrn","raw_name","cleaned_casrn","cleaned_name")
  # chems = unique(chems)
  # chems$source = source
  # prefix = runQuery(paste0("select chemprefix from chemical_source_index where source='",source,"'"),db)[1,1]
  # ilist = seq(from=1,to=nrow(chems))
  # chems$chemical_id = paste0(prefix,"_",ilist)
  # chems$chemical_index = paste(chems$raw_casrn,chems$raw_name)
  # res$chemical_id = NA
  # for(i in 1:nrow(chems)) {indx=chems[i,"chemical_index"]; cid=chems[i,"chemical_id"];res[is.element(res$chemical_index,indx),"chemical_id"]=cid}
  # chems = subset(chems,select=-c(chemical_index))
  # runQuery(paste0("delete from source_chemical where source='",source,"'"),db)
  # runInsertTable(chems,"source_chemical",db,do.halt=T,verbose=F)
  #
  # #####################################################################
  # cat("Build the hash key and load the data \n")
  # #####################################################################
  # res=subset(res,select=-c(chemical_index))
  # toxval_source.hash.and.load(db,source,"new_echa_iuclid",F,F,res)
  #
  # ###runInsertTable(res,"new_echa_iuclid",db,do.halt=T,verbose=F)
  #
  # browser()
  # return(1)





  #####################################################################
  cat("Build echa_iuclid_chemical_information table from res\n")
  #####################################################################
  chemical_information <- res[,c("casrn","name")]
  chemical_information <- unique(chemical_information[,1:2])
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  chemical_information <- chemical_information[c('chemical_id','name','casrn')]

  runInsertTable(chemical_information,"echa_iuclid_chemical_information",db,do.halt=T,verbose=F)

}
