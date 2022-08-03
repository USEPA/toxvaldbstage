library("openxlsx")
#--------------------------------------------------------------------------------------
#' Load ECHA echemportal 2020 Source into dev_toxval_source_v4.
#' @param db The version of toxval into which the source is loaded.
#' @param infile The input file ./echa_echemportal/echa_echemportal_files/eChemPortal mammalian data 2020 step 3.xlsx ,build from echemportal.prep.v2.step3.R

#--------------------------------------------------------------------------------------
import_echa_echemportal_source <- function(db,
                                           infile="../echa_echemportal/echa_echemportal_files/eChemPortal mammalian data 2020 step 3.xlsx",
                                           chem.check.halt=T) {
  printCurrentFunction(db)

  #####################################################################
  cat("Build new_echa table\n")
  #####################################################################

  echa_table <- openxlsx::read.xlsx(infile,1)

  # assign generation info from critical effect to generation
  echa_table$generation_1 <- echa_table$generation
  gen_info <- c("p0","p1","f1","f2","\\bp\\b","\\bmaternal\\b","\\bfetal\\b","f3a")
  echa_table$generation <- str_extract_all(echa_table$critical_effect, regex(paste(gen_info, collapse = "|"), ignore_case = T)) %>% sapply(., paste, collapse = ", ")

  echa_table[which(echa_table$generation == ""),"generation"] <- echa_table[which(echa_table$generation == ""),"generation_1"]
  echa_table$generation <- tolower(echa_table$generation)
  echa_table[which(echa_table$generation == "fetus"),"generation"] <-"fetal"
  echa_table[which(is.na(echa_table$generation)|(echa_table$generation == "")), "generation"] <- "-"
  echa_table <- echa_table[,names(echa_table)[(names(echa_table)!= "generation_1")]]

  #####################################################################
  cat("Do the chemical checking\n")
  #####################################################################
  source = "ECHA eChemPortal 2020"
  res = as.data.frame(echa_table)
  res = res[res$casrn!="unknown",]
  res$clowder_id = "-"
  res = fix.non_ascii.v2(res,source)
  res = source_chemical.process(db,res,source,chem.check.halt,casrn.col="casrn",name.col="name")
  #####################################################################
  cat("Build the hash key and load the data \n")
  #####################################################################
  res = subset(res,select=-c(chemical_index))
  toxval_source.hash.and.load(db,source,"new_echa",F,F,res)
  browser()
  return(1)
  #
  #
  #
  #
  #
  # #####################################################################
  # cat("Do the chemical checking\n")
  # #####################################################################
  # source = "ECHA eChemPortal 2020"
  # res = as.data.frame(echa_table)
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
  # toxval_source.hash.and.load(db,source,"new_echa",F,F,res)
  #
  # ###runInsertTable(echa_table,"new_echa",db,do.halt=T,verbose=F)
  #
  # browser()
  # return(1)









  #####################################################################
  cat("Build echa_chemical_information table from echa_table\n")
  #####################################################################
  chemical_information <- echa_table[,c("casrn","name")]
  chemical_information <- unique(chemical_information[,1:2])
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  chemical_information <- chemical_information[c('chemical_id','name','casrn')]

  runInsertTable(chemical_information,"echa_chemical_information",db,do.halt=T,verbose=F)

}
