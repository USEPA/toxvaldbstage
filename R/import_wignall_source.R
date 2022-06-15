library("openxlsx")

#--------------------------------------------------------------------------------------
#' Load wignall Source data into dev_toxval_source_v2.
#' @param db The version of toxval into which the source info is loaded.
#' @param infile The input file ./wignall/wignall_files/BMD_Results_2014-06-17_reviewed Mar 2018.xlsx


#--------------------------------------------------------------------------------------
import_wignall_source <- function(db,
                                  infile="../wignall/wignall_files/BMD_Results_2014-06-17_reviewed Mar 2018.xlsx",
                                  chem.check.halt=F) {
  printCurrentFunction(db)

  #####################################################################
  cat("Build original_wignall_table and new_wignall_table \n")
  #####################################################################
  res1 <- openxlsx::read.xlsx(infile,1,startRow = 1)
  runInsertTable(res1,"original_wignall_table",db,do.halt=T,verbose=F)
  res1["new_toxval_units"] <- gsub(".*\\(|\\)", "",res1$Toxicity.value.type )
  res1["new_toxval_type"] <- gsub("\\(.*|\\)", "",res1$Toxicity.value.type )
  res1["UF"] <- res1$POD / res1$Toxicity.Value

  colnames(res1) <- c("source_id", "casrn","name","original_toxval_type","original_toxval_units", "subsource",
                      "toxval_numeric", "POD_numeric", "POD_units","POD_type","organ","critical_effect",
                      "effect_description","dose_number","dose_values","dose_units","dose_converted",
                      "DR_type","mean_response","response_units","SD_of_response",
                      "total_number_of_animals","incidence_in_number_of_animals","BMR",
                      "BMD","BMDL","BMD/L_WIZARD_notes","action_taken","BMD'","BMDL'","BMD/L'_WIZARD_notes",
                      "comments","hyperlink","reference","toxval_units","toxval_type","UF")

  res1 <- res1[,c(1,2,3,7,35,36,4,5,6,8:10,37,11:34)]
  res1$critical_effect <- paste(res1$effect, res1$effect_description, sep = ";")
  res1$toxval_type <- gsub("\\s+$","", res1$toxval_type)

  #####################################################################
  cat("Do the chemical checking\n")
  #####################################################################
  source = "Wignall"
  res = as.data.frame(res1)
  res$clowder_id = "-"
  res = fix.non_ascii.v2(res,source)
  res = source_chemical.process(db,res,source,chem.check.halt,casrn.col="casrn",name.col="name")
  #####################################################################
  cat("Build the hash key and load the data \n")
  #####################################################################
  res = subset(res,select=-c(chemical_index))
  toxval_source.hash.and.load(db,source,"new_wignall_table",F,T,res)
  browser()
  return(1)

  # #####################################################################
  # cat("Do the chemical checking\n")
  # #####################################################################
  #
  # res = as.data.frame(res1)
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
  # toxval_source.hash.and.load(db,source,"new_wignall_table",F,F,res)
  #
  # ###runInsertTable(res1,"new_wignall_table",db,do.halt=T,verbose=F)
  #
  # browser()
  # return(1)

  for(i in 1:length(res1$casrn)){
    res1$casrn[i] <- fix.casrn(res1$casrn[i],cname="",verbose=F)

  }
  #res1$casrn <- fix.casrn(res1$casrn,cname="",verbose=F)
  runInsertTable(res1,"new_wignall_table",db,do.halt=T,verbose=F)



  #####################################################################
  cat("Build wignall_chemical_information table from res1\n")
  #####################################################################
  chemical_information <- res1[,2:3]
  chemical_information <- unique(chemical_information[,1:2])
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  chemical_information <- chemical_information[c('chemical_id','name','casrn')]
  runInsertTable(chemical_information,"wignall_chemical_information",db,do.halt=T,verbose=F)

  #####################################################################
  #cat("Build new_wignall_table  \n")
  #####################################################################

  #query <- "select wwt.*, ci.chemical_id from whole_wignall_table wwt inner join wignall_chemical_information ci on ci.name = wwt.name and ci.casrn =wwt.casrn"
  #res2 <- runQuery(query,db)
  #res2_var <- names(res2) %in% c("name","casrn")
  #res2 <- res2[!res2_var]
  #runInsertTable(res2,"new_wignall_table",db,do.halt=T,verbose=F)

}
