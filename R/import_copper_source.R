#--------------------------------------------------------------------------------------
#' Load copper manufacturers Source into dev_toxval_source_v4.
#' @param db The version of toxval into which the source is loaded.
#' @param infile The input file ./copper/copper_files/Copper Data Entry - Final.xlsx
#--------------------------------------------------------------------------------------
import_copper_source <- function(db,
                                 infile="../copper/copper_files/Copper Data Entry - Final.xlsx",
                                 chem.check.halt=F) {
  printCurrentFunction(db)

  #####################################################################
  cat("create original_copper_table from source file\n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile)
  res = res[!is.na(res$casrn),]
  names(res)[c(7,34)]<- c("toxval_subtype1","year1")
  res$toxval_numeric_qualifier <- as.character(res$toxval_numeric_qualifier)

  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="Copper Manufacturers",table="source_copper",res=res,F,T,T)
}
#
#   #####################################################################
#   cat("Do the chemical checking\n")
#   #####################################################################
#   source = "Copper Manufacturers"
#   res = as.data.frame(res)
#   res$clowder_id = "-"
#   res = fix.non_ascii.v2(res,source)
#   res = source_chemical.process(db,res,source,chem.check.halt,casrn.col="casrn",name.col="name")
#   #####################################################################
#   cat("Build the hash key and load the data \n")
#   #####################################################################
#   res = subset(res,select=-c(chemical_index))
#   toxval_source.hash.and.load(db,source,"original_copper_table",F,F,res)
#   return(1)
#
#
#   # #####################################################################
#   # cat("Do the chemical checking\n")
#   # #####################################################################
#   # source = "Copper Manufacturers"
#   # res = as.data.frame(res)
#   # res = fix.non_ascii.v2(res,source)
#   # res$chemical_index = paste(res$casrn,res$name)
#   #
#   # result = chem.check(res,name.col="name",casrn.col="casrn",verbose=F,source)
#   # if(chem.check.halt) if(!result$name.OK || !result$casrn.OK || !result$checksum.OK) browser()
#   #
#   # #####################################################################
#   # cat("Build the chemical table\n")
#   # #####################################################################
#   # chems = cbind(res[,c("casrn","name")],result$res0[,c("casrn","name")])
#   # names(chems) = c("raw_casrn","raw_name","cleaned_casrn","cleaned_name")
#   # chems = unique(chems)
#   # chems$source = source
#   # prefix = runQuery(paste0("select chemprefix from chemical_source_index where source='",source,"'"),db)[1,1]
#   # ilist = seq(from=1,to=nrow(chems))
#   # chems$chemical_id = paste0(prefix,"_",ilist)
#   # chems$chemical_index = paste(chems$raw_casrn,chems$raw_name)
#   # res$chemical_id = NA
#   # for(i in 1:nrow(chems)) {indx=chems[i,"chemical_index"]; cid=chems[i,"chemical_id"];res[is.element(res$chemical_index,indx),"chemical_id"]=cid}
#   # chems = subset(chems,select=-c(chemical_index))
#   # runQuery(paste0("delete from source_chemical where source='",source,"'"),db)
#   # runInsertTable(chems,"source_chemical",db,do.halt=T,verbose=F)
#   #
#   # #####################################################################
#   # cat("Build the hash key and load the data \n")
#   # #####################################################################
#   # res=subset(res,select=-c(chemical_index))
#   # toxval_source.hash.and.load(db,source,"original_copper_table",F,F,res)
#   #
#   # ###runInsertTable(res,"original_copper_table",db,do.halt=T,verbose=F)
#   #
#   # browser()
#   # return(1)
#   #####################################################################
#   cat("Build new_copper_table from res1 \n")
#   #####################################################################
#   res1 <- res
#   res1[which(!is.na(res1[,"toxval_subtype1"])),"toxval_subtype"] <- res1[which(!is.na(res1[,"toxval_subtype1"])),"toxval_subtype1"]
#   res1 <- res1[ , !(names(res1) %in% c("toxval_id","toxval_subtype1","year1"))]
#   res1[grep("^[^[:alnum:]]",res1$toxval_numeric), "toxval_numeric_qualifier"] <- res1[grep("^[^[:alnum:]]",res1$toxval_numeric), "toxval_numeric"]
#   res1[grep("^[^[:alnum:]]",res1$toxval_numeric), "toxval_numeric_qualifier"] <- gsub("(^[^[:alnum:]])(.*)","\\1",res1[grep("^[^[:alnum:]]",res1$toxval_numeric), "toxval_numeric_qualifier"])
#   res1[grep("^[^[:alnum:]]",res1$toxval_numeric), "toxval_numeric"] <- gsub("(^[^[:alnum:]])(.*)","\\2",res1[grep("^[^[:alnum:]]",res1$toxval_numeric), "toxval_numeric"])
#   res1[grep("\\-",res1$toxval_numeric), "toxval_numeric"] <- gsub("(.*)(\\s*\\-\\s*)(.*)","\\1",res1[grep("\\-",res1$toxval_numeric), "toxval_numeric"])
#
#   res1[grep("^[a-zA-Z]+",res1$study_duration_value), "study_duration_value"] <- gsub("(^[a-zA-Z]+\\s*[a-zA-Z]+\\s*)(.*)","\\2",res1[grep("^[a-zA-Z]+",res1$study_duration_value), "study_duration_value"])
#   res1[grep("\\-",res1$study_duration_value), "study_duration_value"] <- gsub("(.*)(\\s*\\-\\s*)(.*)","\\3",res1[grep("\\-",res1$study_duration_value), "study_duration_value"])
#   res1[which(res1$volume == "-"),"volume"] <- ""
#
#   res1["copper_id"] <- c(1:length(res1[,1]))
#   res1 <- res1[c("copper_id",names(res1[-34]))]
#
#   res1 <- lapply(res1, function(x) type.convert(as.character(x), as.is = T))
#   res1 <- data.frame(res1, stringsAsFactors = F)
#
#   runInsertTable(res1,"new_copper_table",db,do.halt=T,verbose=F)
#   #####################################################################
#   cat("Build copper_chemical_information table from res1\n")
#   #####################################################################
#   chemical_information <- res1[,c("name","casrn")]
#   chemical_information <- unique(chemical_information[,1:2])
#   chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
#   chemical_information <- chemical_information[c('chemical_id','name','casrn')]
#
#   runInsertTable(chemical_information,"copper_chemical_information",db,do.halt=T,verbose=F)
#
#
# }
