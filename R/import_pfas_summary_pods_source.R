library(openxlsx)
#library(readxl)
#--------------------------------------------------------------------------------------
#' Load PFAS Summary PODs into dev_toxval_source_v2.
#' @param db The version of toxval into which the source is loaded.
#' @param infile1 The input file ./PFAS Summary PODs/PFAS Summary PODs_files/PFAS 150 Study Level PODs_061920.xlsx
#' @param infile2 The input file ./PFAS Summary PODs/PFAS Summary PODs_files/CompToxChemicalsDashboard-Batch-Search_2020-07-20_17_18_42.xls

#--------------------------------------------------------------------------------------
import_pfas_summary_pods_source <- function(db,
                                            infile1="../PFAS Summary PODs/PFAS Summary PODs_files/PFAS 150 Study Level PODs_061920.xlsx",
                                            infile2="../PFAS Summary PODs/PFAS Summary PODs_files/CompToxChemicalsDashboard-Batch-Search_2020-07-20_17_18_42.xls",
                                            chem.check.halt=T) {
  printCurrentFunction(db)

  #####################################################################
  cat("Build whole_pfas_summary_pods from infile1 sheet 2\n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile1,2)
  #print(dim(res))
  #res <- res[rowSums(is.na(res)) != ncol(res), ]
  pod_summary <- res
  pod_summary <- lapply(pod_summary, function(x) type.convert(as.character(x), as.is = T))
  pod_summary <- data.frame(pod_summary, stringsAsFactors = F)
  runInsertTable(pod_summary,"whole_pfas_summary_pods",db,do.halt=T,verbose=F)

  #####################################################################
  cat("Build pfas_summary_pods_study_dict from infile1 sheet 1 \n")
  #####################################################################
  study_summary <- openxlsx::read.xlsx(infile1,1)
  study_summary <- lapply(study_summary, function(x) type.convert(as.character(x), as.is = T))
  study_summary <- data.frame(study_summary, stringsAsFactors = F)
  runInsertTable(study_summary,"pfas_summary_pods_study_dict",db,do.halt=T,verbose=F)

  #####################################################################
  cat("Build pfas_summary_pods_casrn_dict by batch searching casrns and name using DTXSIDs from CompTox dashboard \n")
  #####################################################################
  casrn_dict <- read_excel(infile2,sheet = 1, col_names = T)
  casrn_dict <- data.frame(casrn_dict,stringsAsFactors = F)
  runInsertTable(casrn_dict,"pfas_summary_pods_casrn_dict",db,do.halt=T,verbose=F)

  #####################################################################
  cat("Build new_pfas_summary_pods table from res \n")
  #####################################################################
  route <- grep("\\:", res$EndpointCategory)
  res[route,"exposure_route"] <- gsub("(.*\\:\\s+)(.*)", "\\2", res[route,"EndpointCategory"])

  duration <- grep("\\(", res$AdminData_Endpoint)
  res[duration,"study_duration_value"] <- gsub("(.*\\()(\\d+)(\\s+.*)", "\\2", res[duration, "AdminData_Endpoint"])
  res[duration,"study_duration_units"] <- gsub("(.*\\(\\d+\\s+)(.*)(\\))", "\\2", res[duration, "AdminData_Endpoint"])

  species <- grep("\\:", res$Results_Sex_or_Species)
  res[species,"species"] <- gsub("(.*)(\\:.*)", "\\1", res[species,"Results_Sex_or_Species"])

  res$sex <- res$Results_Sex_or_Species
  res$sex <- gsub("(.*\\s+)(.*)", "\\2", res$sex)

  res$study_type <- gsub("(.*)(\\s+toxicity.*)","\\1", res$AdminData_Endpoint)
  repro_dev <- grep("\\/", res$study_type)
  res[repro_dev,"study_type"] <- "reproductive developmental"

  generation <- grep("generation", res$study_type)
  res[generation,"study_duration_units"] <- "generation"
  res[grep("one-generation reproductive", res$study_type), "study_duration_value"] <- 1

  res[generation, "study_type"] <- gsub("(.*generation\\s+)(.*)", "\\2", res[generation, "study_type"])

  names(res)
  res1 <- res[,c("HEROID","Citation.Information","DSSTox_Substance_ID","ToxCategory","EndpointCategory",
                 "AdminData_Endpoint","Results_Sex_or_Species","Results_DoseDescriptor_1","Results_EffectLevel",
                 "Units","Results_Remarks","ToxVal.ECHA.Source.Match","exposure_route","study_duration_value",
                 "study_duration_units","species", "sex","study_type")]
  res2 <- res[,c("HEROID","Citation.Information","DSSTox_Substance_ID","ToxCategory","EndpointCategory",
                 "AdminData_Endpoint","Results_Sex_or_Species","Results_DoseDescriptor_2","Results_EffectLevel_2",
                 "Units","Results_Remarks","ToxVal.ECHA.Source.Match","exposure_route","study_duration_value",
                 "study_duration_units","species", "sex","study_type")]
  names(res1) <- c("HEROID","long_ref","DSSTox_Substance_ID","ToxCategory","EndpointCategory",
                   "AdminData_Endpoint","Results_Sex_or_Species","Results_DoseDescriptor","Results_EffectLevel",
                   "Units","Results_Remarks","ToxVal.ECHA.Source.Match","exposure_route","study_duration_value",
                   "study_duration_units","species", "sex","study_type")

  names(res2) <- c("HEROID","long_ref","DSSTox_Substance_ID","ToxCategory","EndpointCategory",
                   "AdminData_Endpoint","Results_Sex_or_Species","Results_DoseDescriptor","Results_EffectLevel",
                   "Units","Results_Remarks","ToxVal.ECHA.Source.Match","exposure_route","study_duration_value",
                   "study_duration_units","species", "sex","study_type")
  res <- rbind(res1, res2)
  res$name <- casrn_dict[match(res$DSSTox_Substance_ID,casrn_dict$INPUT),"PREFERRED_NAME"]
  res$casrn <- casrn_dict[match(res$DSSTox_Substance_ID,casrn_dict$INPUT),"CASRN"]

  res$Results_EffectLevel <- gsub("\\,","",res$Results_EffectLevel)
  qual <- grep("^[^[:alnum:]]",res$Results_EffectLevel)
  res[qual ,"toxval_numeric_qualifier"] <- gsub("(^[^[:alnum:]]+)(\\s*\\d*\\.*\\d*)","\\1",res[qual,"Results_EffectLevel"])
  res$toxval_numeric <- res$Results_EffectLevel
  res[qual,"toxval_numeric"] <- gsub("(^[^[:alnum:]]+\\s*)(\\d*\\.*\\d*)","\\2",res[qual,"toxval_numeric"])
  res$toxval_numeric <- as.numeric(res$toxval_numeric)
  res$toxval_units <- res$Units
  res$toxval_type <- res$Results_DoseDescriptor
  #assign NA exposure route values having NOAEC & LOAEC toxval types as inhalation and ones having other types as oral
  res[grep(".*C$",res$toxval_type),"exposure_route"] <- "inhalation"
  res[grep(".*L$",res$toxval_type),"exposure_route"] <- "oral"

  names.list <- c("long_ref","exposure_route","study_duration_value","study_duration_units","species","sex","study_type",
                  "name","casrn","toxval_numeric_qualifier","toxval_numeric","toxval_units","toxval_type")
  res <- res[,names.list]
  res <- lapply(res, function(x) type.convert(as.character(x), as.is = T))
  res <- data.frame(res, stringsAsFactors = F)
  res["pfas_summary_pods_id"] <- c(1:length(res[,1]))
  res <- res[c("pfas_summary_pods_id",names(res[-14]))]

  #####################################################################
  cat("Do the chemical checking\n")
  #####################################################################
  source = "PFAS Summary PODs"
  res = as.data.frame(res)
  res$clowder_id = "-"
  res = fix.non_ascii.v2(res,source)
  res = source_chemical.process(db,res,source,chem.check.halt,casrn.col="casrn",name.col="name",verbose=F)
  #####################################################################
  cat("Build the hash key and load the data \n")
  #####################################################################
  res = subset(res,select=-c(chemical_index))
  toxval_source.hash.and.load(db,source,"new_pfas_summary_pods",F,T,res)
  browser()
  return(1)



  runInsertTable(res,"new_pfas_summary_pods",db,do.halt=T,verbose=F)
  #####################################################################
  cat("Build  pfas_summary_pods_chemical_information table from res\n")
  #####################################################################
  chemical_information <- res[,c("name","casrn")]
  chemical_information <- unique(chemical_information[,1:2])
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  chemical_information <- chemical_information[c('chemical_id','name','casrn')]
  runInsertTable(chemical_information,"pfas_summary_pods_chemical_information",db,do.halt=T,verbose=F)


}

