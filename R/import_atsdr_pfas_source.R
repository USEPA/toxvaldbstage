#--------------------------------------------------------------------------------------
#' Load ATSDR PFAS Source files into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile1 The input file ./atsdr_pfas/atsdr_pfas_files/ATSDR_Perfluoroalkyls_Inhalation.xlsx
#' @param infile2 The input file ./atsdr_pfas/atsdr_pfas_files/ATSDR_Perfluoroalkyls_Oral.xlsx
#' @param infile3 The input file ./atsdr_pfas/atsdr_pfas_files/ATSDR_PFOA_Inhalation.xlsx
#' @param infile4 The input file ./atsdr_pfas/atsdr_pfas_files/ATSDR_PFOA_Oral.xlsx
#' @param infile5 The input file ./atsdr_pfas/atsdr_pfas_files/ATSDR_PFOS_Oral.xlsx
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#--------------------------------------------------------------------------------------
import_atsdr_pfas_source <- function(db,
                                     infile1="ATSDR_Perfluoroalkyls_Inhalation.xlsx",
                                     infile2="ATSDR_Perfluoroalkyls_Oral.xlsx",
                                     infile3="ATSDR_PFOA_Inhalation.xlsx",
                                     infile4=".ATSDR_PFOA_Oral.xlsx",
                                     infile5="ATSDR_PFOS_Oral.xlsx",
                                     chem.check.halt=F) {
  printCurrentFunction(db)

  indir = paste0(toxval.config()$datapath,"atsdr_pfas/atsdr_pfas_files/")
  infile1 = paste0(indir,infile1)
  infile2 = paste0(indir,infile2)
  infile3 = paste0(indir,infile3)
  infile4 = paste0(indir,infile4)
  infile5 = paste0(indir,infile5)
  #####################################################################
  cat("ATSDR_Perfluoroalkyls_Inhalation\n")
  #####################################################################
  res3 <- openxlsx::read.xlsx(infile1)
  name.list <- c("source_name_sid", "casrn","name","source_url","data_collection","source_name_cid","exposure", "effects","key_to_figure","species_strain","exposure_duration_frequency_route","system","NOAEL","LOAEL","LOAEL_less_serious_comment","LOAEL_serious","LOAEL_serious_comment","LOAEL_reference","chemical_form","comments")
  res3_new <- res3[8:15,]
  names(res3_new) <- name.list
  ####runInsertTable(res3_new,"atsdr_perfluoroalkyls_inhalation",db,do.halt=T,verbose=F)
  #str(res3_new)
  res3_new$exposure_route <- "inhalation"
  res3_new$long_ref <- res3_new[8,"exposure"]
  res3_keys <- res3_new[5:8,]
  res3_new <- res3_new[1:4,]
  names(res3_new)[names(res3_new) == "comments"] <- "exposure_method"
  res3_new$exposure_method <- "nasal"
  names(res3_new)[names(res3_new) == "data_collection"] <- "subsource"
  names(res3_new)[names(res3_new) == "exposure"] <- "study_type"
  res3_new[is.na(res3_new$system),"system"] <- res3_new[is.na(res3_new$system),"effects"]
  names(res3_new)[names(res3_new) == "LOAEL_reference"] <- "short_ref"
  res3_new$study_type <- tolower(gsub("(.*)\\s+(.*)","\\1",res3_new$study_type))
  res3_new$species <- tolower(gsub("(.*)\\s+\\(+(.*)\\)+","\\1",res3_new$species_strain))
  res3_new$strain <- gsub("(.*)\\s+\\(+(.*)\\)+","\\2",res3_new$species_strain)
  res3_new$study_duration_value <- gsub("(\\d+)\\s+(.*)", "\\1", res3_new$exposure_duration_frequency_route)
  res3_new$study_duration_units <- gsub("(\\d+)\\s+(.*)", "\\2", res3_new$exposure_duration_frequency_route)
  res3_new$NOAEL_comment <- NA
  res3_new$year <- gsub("(.*)\\s+(\\d{4}$)", "\\2", res3_new$short_ref)

  h1 <- res3_new[,c(2,3,4,5,7,12,13,27,18,20,21,22,23,24,25,26,28)]
  names(h1)[names(h1) == "NOAEL"] <- "toxval_numeric"
  names(h1)[names(h1) == "NOAEL_comment"] <- "toxval_detail"
  h1$toxval_type <- "NOAEL"
  h1$toxval_units <- "mg/m3"
  h2 <- res3_new[,c(2,3,4,5,7,12,14,15,18,20,21,22,23,24,25,26,28)]
  names(h2)[names(h2) == "LOAEL"] <- "toxval_numeric"
  names(h2)[names(h2) == "LOAEL_less_serious_comment"] <- "toxval_detail"
  h2$toxval_type <- "LOAEL_less_serious"
  h2$toxval_units <- "mg/m3"
  h3 <- res3_new[,c(2,3,4,5,7,12,16,17,18,20,21,22,23,24,25,26,28)]
  names(h3)[names(h3) == "LOAEL_serious"] <- "toxval_numeric"
  names(h3)[names(h3) == "LOAEL_serious_comment"] <- "toxval_detail"
  h3$toxval_type <- "LOAEL_serious"
  h3$toxval_units <- "mg/m3"

  res3_new1 <- rbind(h1,h2,h3)
  res3_new1[res3_new1$system == "Resp","system"] <- "Respiratory"
  res3_new1[res3_new1$system == "Bd Wt","system"] <- "Body weight"
  res3_new1$critical_effect <- paste0(res3_new1$system,"-",res3_new1$toxval_detail)
  res3_new1$critical_effect <- gsub("\\-NA$","-", res3_new1$critical_effect)
  res3_new1$critical_effect <- gsub("\\-$","", res3_new1$critical_effect)
  res3_new1[res3_new1$study_duration_units == "hr","study_duration_units"] <- "hour"
  res3_new1$sex <- gsub("(\\d+)\\s+(.*)","\\2",res3_new1$toxval_numeric)
  res3_new1$toxval_numeric <- as.numeric(gsub("(\\d+)\\s+(.*)","\\1",res3_new1$toxval_numeric))
  res3_new1$study_duration_value <- as.numeric(res3_new1$study_duration_value)
  res3_new1[!is.na(res3_new1$sex), "sex"] <- "Male"
  res3_new1 <- res3_new1[,-c(6,8)]

  #####################################################################
  cat("ATSDR_Perfluoroalkyls_Oral\n")
  #####################################################################
  res4 <- openxlsx::read.xlsx(infile2)
  name.list <- c("source_name_sid", "casrn","name","source_url","data_collection","source_name_cid","exposure", "effects","key_to_figure","species_strain","exposure_duration_frequency_route","system","NOAEL","LOAEL","LOAEL_less_serious_comment","LOAEL_serious","LOAEL_serious_comment","LOAEL_reference","chemical_form","comments","no_header1","no_header2","no_header3","no_header4","no_header5","no_header6","no_header7" )
  res4_new <- res4[8:129,]
  names(res4_new) <- name.list
  ####runInsertTable(res4_new,"atsdr_perfluoroalkyls_oral",db,do.halt=T,verbose=F)

  #str(res4_new)
  res4_new$exposure_route <- "oral"
  res4_key <- res4_new[118:122,]
  res4_new <- res4_new[1:117,]
  res4_new[grep("\\(.*\\)",res4_new$exposure_duration_frequency_route),"exposure_method"] <- gsub("(.*)\\s+\\((.*)\\)$","\\2",res4_new[grep("\\(.*\\)",res4_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res4_new[grep("\\bGO\\b",res4_new$exposure_method),"exposure_method"] <- "gavage with oil"
  res4_new[grep("\\bF\\b",res4_new$exposure_method),"exposure_method"] <- "feed"
  res4_new[grep("\\bGW\\b",res4_new$exposure_method),"exposure_method"] <- "gavage with water"
  res4_new[grep("\\bG\\b",res4_new$exposure_method),"exposure_method"] <- "gavage"
  names(res4_new)[names(res4_new) == "data_collection"] <- "subsource"
  names(res4_new)[names(res4_new) == "exposure"] <- "study_type"
  res4_new[is.na(res4_new$system),"system"] <- res4_new[is.na(res4_new$system),"effects"]
  names(res4_new)[names(res4_new) == "LOAEL_reference"] <- "short_ref"
  res4_new$study_type <- tolower(gsub("(.*)\\s+(.*)","\\1",res4_new$study_type))
  res4_new$species <- tolower(gsub("(.*)\\s+\\(+(.*)\\)+","\\1",res4_new$species_strain))
  res4_new$strain <- gsub("(.*)\\s+\\(+(.*)\\)+","\\2",res4_new$species_strain)
  res4_new[grep("once",res4_new$exposure_duration_frequency_route),"study_duration_value"] <- 1
  res4_new[grep("once",res4_new$exposure_duration_frequency_route),"study_duration_units"] <- "day"
  res4_new[grep("^\\d+\\-\\d+",res4_new$exposure_duration_frequency_route),"study_duration_value"] <- gsub("(^\\d+\\-)(\\d+)\\s+.*","\\2",res4_new[grep("^\\d+\\-\\d+",res4_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res4_new[grep("^\\d+\\-\\d+",res4_new$exposure_duration_frequency_route),"study_duration_units"] <- gsub("(^\\d+\\-\\d+)\\s+(\\w+)(\\s+.*)(\\s+\\(+.*\\)+)","\\2\\3",res4_new[grep("^\\d+\\-\\d+",res4_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res4_new[grep("^\\d+\\s+",res4_new$exposure_duration_frequency_route),"study_duration_value"] <- gsub("(^\\d+)(\\s+.*)","\\1",res4_new[grep("^\\d+\\s+",res4_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res4_new[grep("^\\d+\\s+",res4_new$exposure_duration_frequency_route),"study_duration_units"] <- gsub("(^\\d+)\\s+(\\w+)(\\s*.*)(\\s+\\(+.*\\)+)","\\2\\3",res4_new[grep("^\\d+\\s+",res4_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res4_new[grep("^Gd\\s+",res4_new$exposure_duration_frequency_route),"study_duration_units"] <- "gestational day 1 x/d"
  res4_new[grep("^Gd\\s+",res4_new$exposure_duration_frequency_route),"study_duration_value"] <- gsub("(^Gd)\\s+(\\d+\\-)(\\d+)(\\s+.*)","\\3",res4_new[grep("^Gd\\s+",res4_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res4_new[grep("^PND\\s+",res4_new$exposure_duration_frequency_route),"study_duration_units"] <- "post-natal day"
  res4_new[grep("^PND\\s+",res4_new$exposure_duration_frequency_route),"study_duration_value"] <- gsub("(^PND)\\s+(\\d+)\\s+(.*)","\\2",res4_new[grep("^PND\\s+",res4_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res4_new[grep("PND",res4_new$study_duration_units),"study_duration_value"] <- "72"
  res4_new[grep("PND",res4_new$study_duration_units),"study_duration_units"] <- "Post-natal day 1 x/d"
  res4_new[grep("Gd",res4_new$study_duration_units),"study_duration_value"] <- "18"
  res4_new[grep("Gd",res4_new$study_duration_units),"study_duration_units"] <- "day 1 x/d"
  res4_new[grep("^P0",res4_new$exposure_duration_frequency_route),"study_duration_value"] <- gsub("(.*\\s+)(\\d+)(\\s+.*)","\\2",res4_new[grep("^P0",res4_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res4_new[grep("^P0",res4_new$exposure_duration_frequency_route),"study_duration_units"] <- gsub("(.*\\s+)(\\d+)\\s+(\\w+)(\\s+.*)","\\3",res4_new[grep("^P0",res4_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res4_new[grep("\\bd\\b",res4_new$study_duration_units) , "study_duration_units"] <- "day"
  res4_new[grep("\\bwk\\b",res4_new$study_duration_units), "study_duration_units"] <- "week"
  res4_new[grep("\\bdays\\b",res4_new$study_duration_units), "study_duration_units"] <- "day"
  res4_new[grep("\\bad lib\\b",res4_new$study_duration_units), "study_duration_units"] <- "ad libitum"
  res4_new$NOAEL_comment <- res4_new$comments
  res4_new[grep("(\\;)",res4_new$short_ref),"year"] <- gsub("(.*)(\\d{4})(\\w+\\;.*)\\s+(\\d{4})\\w*","\\2,\\4",res4_new[grep("(\\;)",res4_new$short_ref),"short_ref"])
  res4_new[grep("\\;",res4_new$short_ref, invert = T),"year"] <- gsub("(.*)(\\d{4})(.*)","\\2",res4_new[grep("\\;",res4_new$short_ref, invert = T),"short_ref"])

  h1 <- res4_new[,c(2,3,4,5,7,12,13,34,18,28,29,30,31,32,33,35)]
  names(h1)[names(h1) == "NOAEL"] <- "toxval_numeric"
  names(h1)[names(h1) == "NOAEL_comment"] <- "toxval_detail"
  h1$toxval_type <- "NOAEL"
  h1$toxval_units <- "mg/kg/day"
  h2 <- res4_new[,c(2,3,4,5,7,12,14,15,18,28,29,30,31,32,33,35)]
  names(h2)[names(h2) == "LOAEL"] <- "toxval_numeric"
  names(h2)[names(h2) == "LOAEL_less_serious_comment"] <- "toxval_detail"
  h2$toxval_type <- "LOAEL_less_serious"
  h2$toxval_units <- "mg/kg/day"
  h3 <- res4_new[,c(2,3,4,5,7,12,16,17,18,28,29,30,31,32,33,35)]
  names(h3)[names(h3) == "LOAEL_serious"] <- "toxval_numeric"
  names(h3)[names(h3) == "LOAEL_serious_comment"] <- "toxval_detail"
  h3$toxval_type <- "LOAEL_serious"
  h3$toxval_units <- "mg/kg/day"

  res4_new1 <- rbind(h1,h2,h3)
  res4_new1[res4_new1$system == "Resp","system"] <- "Respiratory"
  res4_new1[res4_new1$system == "Bd Wt","system"] <- "Body weight"
  res4_new1[res4_new1$system == "Cardio","system"] <- "cardiovascular"
  res4_new1[res4_new1$system == "Endocr","system"] <- "endocrine"
  res4_new1[res4_new1$system == "Gastro","system"] <- "gastrointestinal"
  res4_new1[res4_new1$system == "Immuno/ Lymphoret","system"] <- "immunological/lymphoreticular"
  res4_new1[res4_new1$system == "Hemato","system"] <- "hematological"
  res4_new1[res4_new1$system == "Musc/skel","system"] <- "musculoskeletal"
  res4_new1$critical_effect <- paste0(res4_new1$system,"-",res4_new1$toxval_detail)
  res4_new1$critical_effect <- gsub("\\-NA$","-", res4_new1$critical_effect)
  res4_new1$critical_effect <- gsub("\\-$","", res4_new1$critical_effect)
  res4_new1$sex <- gsub("(\\d+\\.*\\d*\\s*)(\\w*)","\\2",res4_new1$toxval_numeric)
  res4_new1[grep("\\bM\\b",res4_new1$sex), "sex"] <- "Male"
  res4_new1[grep("\\bF\\b",res4_new1$sex), "sex"] <- "Female"
  res4_new1$toxval_numeric <- as.numeric(gsub("(\\d+\\.*\\d*)(\\s*\\w*)","\\1",res4_new1$toxval_numeric))
  res4_new1$study_duration_value <- as.numeric(res4_new1$study_duration_value)
  res4_new1[is.na(res4_new1$toxval_numeric),"toxval_units"] <- NA
  res4_new1 <- res4_new1[,-c(6,8)]

  #####################################################################
  cat("ATSDR_PFOA_Inhalation\n")
  #####################################################################
  res5 <- openxlsx::read.xlsx(infile3)
  name.list <- c("source_name_sid", "casrn","name","source_url","data_collection","source_name_cid","exposure", "effects","key_to_figure","species_strain","exposure_duration_frequency_route","system","NOAEL","LOAEL","LOAEL_less_serious_comment","LOAEL_serious","LOAEL_serious_comment","LOAEL_reference","chemical_form","comments")
  res5_new <- res5[7:36,]
  names(res5_new) <- name.list
  ####runInsertTable(res5_new,"atsdr_pfoa_inhalation",db,do.halt=T,verbose=F)

  #str(res5_new)
  res5_new$record_url <- "https://www.atsdr.cdc.gov/toxprofiles/tp200-c3.pdf"
  res5_new$exposure_route <- "inhalation"
  res5_keys <- res5_new[27:30,]
  res5_new <- res5_new[1:26,]
  res5_new$NOAEL_comment <- res5_new$comments
  names(res5_new)[names(res5_new) == "data_collection"] <- "subsource"
  names(res5_new)[names(res5_new) == "exposure"] <- "study_type"
  res5_new[is.na(res5_new$system),"system"] <- res5_new[is.na(res5_new$system),"effects"]
  names(res5_new)[names(res5_new) == "LOAEL_reference"] <- "short_ref"
  res5_new$study_type <- tolower(gsub("(.*)\\s+(.*)","\\1",res5_new$study_type))
  res5_new$species <- tolower(gsub("(.*)\\s+\\(+(.*)\\)+","\\1",res5_new$species_strain))
  res5_new$strain <- gsub("(.*)\\s+\\(+(.*)\\)+","\\2",res5_new$species_strain)
 res5_new$study_duration_value <- gsub("(\\d+)\\s+(.*)", "\\1", res5_new$exposure_duration_frequency_route)
  res5_new[grep("^Gd\\s+",res5_new$exposure_duration_frequency_route),"study_duration_value"] <- gsub("(^Gd)\\s+(\\d+\\-)(\\d+)(\\s+.*)","\\3",res5_new[grep("^Gd\\s+",res5_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res5_new$study_duration_units <- gsub("(\\d+)\\s+(.*)", "\\2", res5_new$exposure_duration_frequency_route)
  res5_new[grep("^Gd\\s+",res5_new$exposure_duration_frequency_route),"study_duration_units"] <- gsub("(.*\\s+)\\d\\-(.*)","gestational day \\2",res5_new[grep("^Gd\\s+",res5_new$exposure_duration_frequency_route),"study_duration_units"])
  res5_new$year <- gsub("(.*)\\s+(\\d{4}$)", "\\2", res5_new$short_ref)
  res5_new[grep("\\(",res5_new$exposure_duration_frequency_route),"study_duration_units"] <- gsub("NS","not specified",res5_new[grep("\\(",res5_new$exposure_duration_frequency_route),"study_duration_units"])

  h1 <- res5_new[,c(2,3,4,5,7,12,13,23,18,21,22,24,25,26,27,28)]
  names(h1)[names(h1) == "NOAEL"] <- "toxval_numeric"
  names(h1)[names(h1) == "NOAEL_comment"] <- "toxval_detail"
  h1$toxval_type <- "NOAEL"
  h1$toxval_units <- "mg/m3"
  h2 <-res5_new[,c(2,3,4,5,7,12,14,15,18,21,22,24,25,26,27,28)]
  names(h2)[names(h2) == "LOAEL"] <- "toxval_numeric"
  names(h2)[names(h2) == "LOAEL_less_serious_comment"] <- "toxval_detail"
  h2$toxval_type <- "LOAEL_less_serious"
  h2$toxval_units <- "mg/m3"
  h3 <- res5_new[,c(2,3,4,5,7,12,16,17,18,21,22,24,25,26,27,28)]
  names(h3)[names(h3) == "LOAEL_serious"] <- "toxval_numeric"
  names(h3)[names(h3) == "LOAEL_serious_comment"] <- "toxval_detail"
  h3$toxval_type <- "LOAEL_serious"
  h3$toxval_units <- "mg/m3"

  res5_new1 <- rbind(h1,h2,h3)
  res5_new1[res5_new1$system == "Resp","system"] <- "Respiratory"
  res5_new1[res5_new1$system == "Bd Wt","system"] <- "Body weight"
  res5_new1$critical_effect <- paste0(res5_new1$system,"-",res5_new1$toxval_detail)
  res5_new1$critical_effect <- gsub("\\-NA$","-", res5_new1$critical_effect)
  res5_new1$critical_effect <- gsub("\\-$","", res5_new1$critical_effect)
  res5_new1$sex <- gsub("(\\d+\\.*\\d*\\s*)(\\w*)","\\2",res5_new1$toxval_numeric)
  res5_new1[grep("\\bM\\b",res5_new1$sex), "sex"] <- "Male"
  res5_new1[grep("\\bF\\b",res5_new1$sex), "sex"] <- "Female"
  res5_new1$toxval_numeric <- as.numeric(gsub("(\\d+)\\s+(.*)","\\1",res5_new1$toxval_numeric))
  res5_new1$study_duration_value <- as.numeric(res5_new1$study_duration_value)
  res5_new1 <- res5_new1[,-c(6,8)]

  #####################################################################
  cat("ATSDR_PFOA_Oral\n")
  #####################################################################
  res6 <- openxlsx::read.xlsx(infile4)
  name.list <- c("source_name_sid", "casrn","name","source_url","data_collection","source_name_cid","exposure", "effects","key_to_figure","species_strain","exposure_duration_frequency_route","system","NOAEL","LOAEL","LOAEL_less_serious_comment","LOAEL_serious","LOAEL_serious_comment","LOAEL_reference","chemical_form","comments","no_header1" )
  res6_new <- res6[5:207,]
  names(res6_new) <- name.list
  #str(res6_new)
  #####runInsertTable(res6_new,"atsdr_pfoa_oral",db,do.halt=T,verbose=F)

  res6_new$exposure_route <- "oral"
  res6_key <- res6_new[193:207,]
  res6_new <- res6_new[1:192,]
  res6_new[grep("\\(.*\\)",res6_new$exposure_duration_frequency_route),"exposure_method"] <- gsub("(.*)\\s+\\((.*)\\)$","\\2",res6_new[grep("\\(.*\\)",res6_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res6_new[grep("\\bGO\\b",res6_new$exposure_method),"exposure_method"] <- "gavage with oil"
  res6_new[grep("\\bF\\b",res6_new$exposure_method),"exposure_method"] <- "feed"
  res6_new[grep("\\bGW\\b",res6_new$exposure_method),"exposure_method"] <- "gavage with water"
  res6_new[grep("\\bG\\b",res6_new$exposure_method),"exposure_method"] <- "gavage"
  res6_new[grep("\\bDW\\b",res6_new$exposure_method),"exposure_method"] <- "drinking water"
  res6_new[grep("\\bW\\b",res6_new$exposure_method),"exposure_method"] <- "water"
  res6_new[grep("\\bC\\b",res6_new$exposure_method),"exposure_method"] <- "capsule"

  names(res6_new)[names(res6_new) == "data_collection"] <- "subsource"
  names(res6_new)[names(res6_new) == "exposure"] <- "study_type"
  res6_new[is.na(res6_new$system),"system"] <- res6_new[is.na(res6_new$system),"effects"]
  names(res6_new)[names(res6_new) == "LOAEL_reference"] <- "short_ref"
  res6_new$study_type <- tolower(gsub("(.*)\\s+(.*)","\\1",res6_new$study_type))
  res6_new$species <-  tolower(gsub("(.*)\\s+\\(+(.*)\\)+.*","\\1",res6_new$species_strain))
  res6_new$strain <- gsub("(.*)\\s+\\(+(.*)\\)+(.*)","\\2\\3",res6_new$species_strain)
  res6_new[grep("once",res6_new$exposure_duration_frequency_route),"study_duration_value"] <- 1
  res6_new[grep("once",res6_new$exposure_duration_frequency_route),"study_duration_units"] <- "day"
  res6_new[grep("^\\d+\\-\\d+",res6_new$exposure_duration_frequency_route),"study_duration_value"] <- gsub("(^\\d+\\-)(\\d+)\\s+.*","\\2",res6_new[grep("^\\d+\\-\\d+",res6_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res6_new[grep("^\\d+\\-\\d+",res6_new$exposure_duration_frequency_route),"study_duration_units"] <- gsub("(^\\d+\\-\\d+)\\s+(\\w+)(.*)\\s+(\\(+.*\\)+)","\\2\\3",res6_new[grep("^\\d+\\-\\d+",res6_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res6_new[grep("^\\d+\\s+",res6_new$exposure_duration_frequency_route),"study_duration_value"] <- gsub("(^\\d+)(\\s+.*)","\\1",res6_new[grep("^\\d+\\s+",res6_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res6_new[grep("^\\d+\\s+or\\s+\\d+",res6_new$exposure_duration_frequency_route),"study_duration_value"] <- gsub("(^\\d+)(\\s+or\\s+)(\\d+)(.*)","\\3",res6_new[grep("^\\d+\\s+or\\s+\\d+",res6_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res6_new[grep("^\\d+\\s+",res6_new$exposure_duration_frequency_route),"study_duration_units"] <- gsub("(^\\d+)\\s+(\\w+)(\\s*.*)(\\s+\\(+.*\\)+)","\\2\\3",res6_new[grep("^\\d+\\s+",res6_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res6_new[grep("^\\d+\\s+or\\s+\\d+",res6_new$exposure_duration_frequency_route),"study_duration_units"] <- gsub("(or\\s+\\d+\\s+)(.*)", "\\2",res6_new[grep("^\\d+\\s+or\\s+\\d+",res6_new$exposure_duration_frequency_route),"study_duration_units"] )
  res6_new[grep("^Gd\\s+",res6_new$exposure_duration_frequency_route),"study_duration_units"] <- "gestational day"
  res6_new[grep("^Gd\\s+",res6_new$exposure_duration_frequency_route),"study_duration_value"] <- gsub("(^Gd)\\s+(\\d+\\-)(\\d+)(\\s+.*)","\\3",res6_new[grep("^Gd\\s+",res6_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res6_new[grep("^PND\\s+",res6_new$exposure_duration_frequency_route),"study_duration_units"] <- "post-natal day"
  res6_new[grep("^PND\\s+",res6_new$exposure_duration_frequency_route),"study_duration_value"] <- gsub("(^PND)\\s+(\\d+)\\s+(.*)","\\2",res6_new[grep("^PND\\s+",res6_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res6_new[grep("\\bd\\b",res6_new$study_duration_units) , "study_duration_units"] <- "day"
  res6_new[grep("\\bwk\\b",res6_new$study_duration_units), "study_duration_units"] <- "week"
  res6_new[grep("\\bdays\\b",res6_new$study_duration_units), "study_duration_units"] <- "day"
  res6_new[grep("\\byr\\b",res6_new$study_duration_units), "study_duration_units"] <- "year"
  res6_new[grep("\\bad lib\\b",res6_new$study_duration_units), "study_duration_units"] <- "ad libitum"
  res6_new$NOAEL_comment <- res6_new$comments
  res6_new$year <- gsub("(.*)\\s+(\\d{4})(.*)", "\\2", res6_new$short_ref)

  h1 <- res6_new[,c(2,3,4,5,7,12,13,28,18,22:27,29)]
  names(h1)[names(h1) == "NOAEL"] <- "toxval_numeric"
  names(h1)[names(h1) == "NOAEL_comment"] <- "toxval_detail"
  h1$toxval_type <- "NOAEL"
  h1$toxval_units <- "mg/kg/day"
  h2 <- res6_new[,c(2,3,4,5,7,12,14,15,18,22:27,29)]
  names(h2)[names(h2) == "LOAEL"] <- "toxval_numeric"
  names(h2)[names(h2) == "LOAEL_less_serious_comment"] <- "toxval_detail"
  h2$toxval_type <- "LOAEL_less_serious"
  h2$toxval_units <- "mg/kg/day"
  h3 <- res6_new[,c(2,3,4,5,7,12,16,17,18,22:27,29)]
  names(h3)[names(h3) == "LOAEL_serious"] <- "toxval_numeric"
  names(h3)[names(h3) == "LOAEL_serious_comment"] <- "toxval_detail"
  h3$toxval_type <- "LOAEL_serious"
  h3$toxval_units <- "mg/kg/day"

  res6_new1 <- rbind(h1,h2,h3)
  res6_new1[grep("Resp",res6_new1$system),"system"] <- "Respiratory"
  res6_new1[grep("Bd Wt",res6_new1$system),"system"] <- "Body weight"
  res6_new1[grep("Cardio",res6_new1$system),"system"] <- "cardiovascular"
  res6_new1[grep("Endocr",res6_new1$system),"system"] <- "endocrine"
  res6_new1[grep("Gastro",res6_new1$system),"system"] <- "gastrointestinal"
  res6_new1[grep("Immuno/ Lymphoret",res6_new1$system),"system"] <- "immunological/lymphoreticular"
  res6_new1[grep("Hemato",res6_new1$system),"system"] <- "hematological"
  res6_new1[grep("Musc/skel",res6_new1$system),"system"] <- "musculoskeletal"
  res6_new1$critical_effect <- paste0(res6_new1$system,"-",res6_new1$toxval_detail)
  res6_new1$critical_effect <- gsub("\\-NA$","-", res6_new1$critical_effect)
  res6_new1$critical_effect <- gsub("\\-$","", res6_new1$critical_effect)
  res6_new1[grep("\\(b\\)",res6_new1$toxval_numeric),"critical_effect"] <- paste0(res6_new1[grep("\\(b\\)",res6_new1$toxval_numeric),"critical_effect"], "[key information on toxval_numeric value derivation - ", res6_key[2,13], "]")
  res6_new1[grep("\\(b\\)",res6_new1$toxval_numeric),"toxval_numeric"] <- gsub("(^\\d+)(\\s+\\(.*\\))(.*)\\s+", "\\1\\3",res6_new1[grep("\\(b\\)",res6_new1$toxval_numeric),"toxval_numeric"])
  res6_new1[grep("\\(.*\\)",res6_new1$toxval_numeric),"critical_effect"] <- paste0(res6_new1[grep("\\(.*\\)",res6_new1$toxval_numeric),"critical_effect"], "-", gsub("(.*)(\\(.*\\))","\\2", res6_new1[grep("\\(.*\\)",res6_new1$toxval_numeric),"toxval_numeric"]))
  res6_new1[grep("\\(.*\\)",res6_new1$toxval_numeric),"toxval_numeric"] <- gsub("\\s+$","",gsub("(.*)(\\s*\\(.*\\))","\\1",res6_new1[grep("\\(.*\\)",res6_new1$toxval_numeric),"toxval_numeric"] ))
  res6_new1$sex <- gsub("(\\d+\\.*\\d*\\s*)(\\w*)","\\2",res6_new1$toxval_numeric)
  res6_new1[grep("\\bM\\b",res6_new1$sex), "sex"] <- "Male"
  res6_new1[grep("\\bF\\b",res6_new1$sex), "sex"] <- "Female"
  res6_new1$sex <- gsub("\\s+$","",res6_new1$sex)
  res6_new1$toxval_numeric <- as.numeric(gsub("(\\d+\\.*\\d*)(\\s*\\w*)","\\1",res6_new1$toxval_numeric))
  res6_new1$study_duration_value <- as.numeric(res6_new1$study_duration_value)
  res6_new1[is.na(res6_new1$toxval_numeric),"toxval_units"] <- "-"
  res6_new1$record_url <- "https://www.atsdr.cdc.gov/toxprofiles/tp200-c3.pdf"
  res6_new1 <- res6_new1[,-c(6,8)]

  #####################################################################
  cat("ATSDR_PFOS_Oral\n")
  #####################################################################
  res7 <- openxlsx::read.xlsx(infile5)
  name.list <- c("source_name_sid", "casrn","name","source_url","data_collection","source_name_cid","key_info","exposure", "effects","key_to_figure","species_strain","exposure_duration_frequency_route","system","NOAEL","LOAEL","LOAEL_less_serious_comment","LOAEL_serious","LOAEL_serious_comment","LOAEL_reference","chemical_form","comments","no_header1" )
  res7_new <- res7[5:159,]
  names(res7_new) <- name.list
  ####runInsertTable(res7_new,"atsdr_pfos_oral",db,do.halt=T,verbose=F)

  #str(res7_new)

  res7_new$exposure_route <- "oral"
  res7_key <- res7_new[150:155,]
  res7_new <- res7_new[1:149,]
  res7_new[grep("\\(.*\\)",res7_new$exposure_duration_frequency_route),"exposure_method"] <- gsub("(.*)\\s+\\((.*)\\)$","\\2",res7_new[grep("\\(.*\\)",res7_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res7_new[grep("\\bGO\\b",res7_new$exposure_method),"exposure_method"] <- "gavage with oil"
  res7_new[grep("\\bF\\b",res7_new$exposure_method),"exposure_method"] <- "feed"
  res7_new[grep("\\bGW\\b",res7_new$exposure_method),"exposure_method"] <- "gavage with water"
  res7_new[grep("\\bG\\b",res7_new$exposure_method),"exposure_method"] <- "gavage"
  res7_new[grep("\\bDW\\b",res7_new$exposure_method),"exposure_method"] <- "drinking water"
  res7_new[grep("\\bW\\b",res7_new$exposure_method),"exposure_method"] <- "water"
  res7_new[grep("\\bC\\b",res7_new$exposure_method),"exposure_method"] <- "capsule"

  names(res7_new)[names(res7_new) == "data_collection"] <- "subsource"
  names(res7_new)[names(res7_new) == "exposure"] <- "study_type"
  res7_new[is.na(res7_new$system),"system"] <- res7_new[is.na(res7_new$system),"effects"]
  names(res7_new)[names(res7_new) == "LOAEL_reference"] <- "short_ref"
  res7_new$study_type <- tolower(gsub("(.*)\\s+(.*)","\\1",res7_new$study_type))
  res7_new[grep("\\([a-zA-Z0-9\\/]+$", res7_new$species_strain), "species_strain"] <- paste0(res7_new[grep("\\([a-zA-Z0-9\\/]+$", res7_new$species_strain), "species_strain"], ")")
  res7_new$species <-  tolower(gsub("(.*)\\s+\\(+(.*)\\)+.*","\\1",res7_new$species_strain))
  res7_new$strain <- gsub("(.*)\\s+\\(+(.*)\\)+(.*)","\\2\\3",res7_new$species_strain)
  res7_new[grep("once",res7_new$exposure_duration_frequency_route),"study_duration_value"] <- 1
  res7_new[grep("once",res7_new$exposure_duration_frequency_route),"study_duration_units"] <- "day"
  res7_new[grep("^\\d+\\s+",res7_new$exposure_duration_frequency_route),"study_duration_value"] <- gsub("(^\\d+)(\\s+.*)","\\1",res7_new[grep("^\\d+\\s+",res7_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res7_new[grep("^\\d+\\s+",res7_new$exposure_duration_frequency_route),"study_duration_units"] <- gsub("(^\\d+)(\\s+)(.*)(\\s*\\(.*\\))","\\3",res7_new[grep("^\\d+\\s+",res7_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res7_new[grep("^Gd\\s+",res7_new$exposure_duration_frequency_route),"study_duration_units"] <- "gestational day"
  res7_new[grep("^Gd\\s+",res7_new$exposure_duration_frequency_route),"study_duration_value"] <- gsub("(^Gd)\\s+(\\d+\\-)(\\d+)(\\s*.*)","\\3",res7_new[grep("^Gd\\s+",res7_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res7_new[grep("^Gd\\s+\\d+\\-\\d+$",res7_new$exposure_duration_frequency_route),"study_duration_value"] <- gsub("(.*\\s+\\d+\\-)(\\d+)","\\2",res7_new[grep("^Gd\\s+\\d+\\-\\d+$",res7_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res7_new[grep("^Gd\\s+\\d+\\-\\d+$",res7_new$exposure_duration_frequency_route),"study_duration_units"] <- gsub("(.*)(\\s+\\d+\\-\\d+)","\\1",res7_new[grep("^Gd\\s+\\d+\\-\\d+$",res7_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res7_new[grep("Gd.*PND",res7_new$exposure_duration_frequency_route),"study_duration_value"] <- gsub("(Gd\\s+[01]\\s+.*PND\\s+)(\\d+)(.*)","\\2",res7_new[grep("Gd.*PND",res7_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res7_new[grep("Gd.*PND",res7_new$exposure_duration_frequency_route),"study_duration_units"] <- gsub("(Gd\\s+[01]\\s+.*)(PND)(\\s+\\d+.*)","\\2",res7_new[grep("Gd.*PND",res7_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res7_new[grep("Gd.*Gd",res7_new$exposure_duration_frequency_route),"study_duration_value"] <- gsub("(Gd.*Gd\\s+\\d+\\-)(\\d+)(\\s+.*)","\\2",res7_new[grep("Gd.*Gd",res7_new$exposure_duration_frequency_route),"exposure_duration_frequency_route"])
  res7_new[grep("Gd.*Gd",res7_new$exposure_duration_frequency_route),"study_duration_units"] <- "gestational day"
  res7_new[grep(",",res7_new$exposure_duration_frequency_route),"study_duration_value"] <- gsub("(.*\\-)(\\d+)(\\s*.*$)","\\2",res7_new[grep(",",res7_new$exposure_duration_frequency_route),"study_duration_units"])
  res7_new[grep(",",res7_new$exposure_duration_frequency_route),"study_duration_units"] <- "gestational day"
  res7_new[grep("Gd",res7_new$exposure_duration_frequency_route),"study_duration_units"] <- "gestational day"
  res7_new[grep("PND",res7_new$exposure_duration_frequency_route),"study_duration_units"] <- "post-natal day"
  res7_new[grep("7 d ",res7_new$exposure_duration_frequency_route),"study_duration_units"] <- "day"
  res7_new[grep("60 days",res7_new$exposure_duration_frequency_route),"study_duration_units"] <- "day"
  res7_new$study_duration_units <- gsub("\\s+$","",res7_new$study_duration_units)
  res7_new[grep("\\bd\\b",res7_new$study_duration_units) , "study_duration_units"] <- "day"
  res7_new[grep("\\bwk\\b",res7_new$study_duration_units), "study_duration_units"] <- "week"
  res7_new[grep("\\bdays\\b",res7_new$study_duration_units), "study_duration_units"] <- "day"
  res7_new[grep("\\byr\\b",res7_new$study_duration_units), "study_duration_units"] <- "year"
  res7_new[grep("\\bad lib\\b",res7_new$study_duration_units), "study_duration_units"] <- "ad libitum"
  res7_new$NOAEL_comment <- res7_new$comments
  res7_new$year <- gsub("(.*)\\s+(\\d{4})(.*)", "\\2", res7_new$short_ref)

  h1 <- res7_new[,c(2,3,4,5,8,13,14,29,19,23:28,30)]
  names(h1)[names(h1) == "NOAEL"] <- "toxval_numeric"
  names(h1)[names(h1) == "NOAEL_comment"] <- "toxval_detail"
  h1$toxval_type <- "NOAEL"
  h1$toxval_units <- "mg/kg/day"
  h2 <- res7_new[,c(2,3,4,5,8,13,15,16,19,23:28,30)]
  names(h2)[names(h2) == "LOAEL"] <- "toxval_numeric"
  names(h2)[names(h2) == "LOAEL_less_serious_comment"] <- "toxval_detail"
  h2$toxval_type <- "LOAEL_less_serious"
  h2$toxval_units <- "mg/kg/day"
  h3 <- res7_new[,c(2,3,4,5,8,13,17,18,19,23:28,30)]
  names(h3)[names(h3) == "LOAEL_serious"] <- "toxval_numeric"
  names(h3)[names(h3) == "LOAEL_serious_comment"] <- "toxval_detail"
  h3$toxval_type <- "LOAEL_serious"
  h3$toxval_units <- "mg/kg/day"

  res7_new1 <- rbind(h1,h2,h3)
  res7_new1[grep("Resp",res7_new1$system),"system"] <- "Respiratory"
  res7_new1[grep("Bd Wt",res7_new1$system),"system"] <- "Body weight"
  res7_new1[grep("Cardio",res7_new1$system),"system"] <- "cardiovascular"
  res7_new1[grep("Endocr",res7_new1$system),"system"] <- "endocrine"
  res7_new1[grep("Gastro",res7_new1$system),"system"] <- "gastrointestinal"
  res7_new1[grep("Immuno/ Lymphoret",res7_new1$system),"system"] <- "immunological/lymphoreticular"
  res7_new1[grep("Hemato",res7_new1$system),"system"] <- "hematological"
  res7_new1[grep("Musc/skel",res7_new1$system),"system"] <- "musculoskeletal"
  res7_new1[grep("\\(",res7_new1$system),"exposure_method"] <- gsub("(.*\\(+)(.*)(\\)+)","\\2",res7_new1[grep("\\(",res7_new1$system),"system"])
  res7_new1[grep("GW",res7_new1$system),"exposure_method"] <- "gavage with water"
  res7_new1[grep("GW",res7_new1$system),"system"] <- gsub("(.*)(\\s+\\(+.*\\)+)","\\1",res7_new1[grep("GW",res7_new1$system),"system"])
  res7_new1$critical_effect <- paste0(res7_new1$system,"-",res7_new1$toxval_detail)
  res7_new1$critical_effect <- gsub("\\-NA$","-", res7_new1$critical_effect)
  res7_new1$critical_effect <- gsub("\\-$","", res7_new1$critical_effect)
  res7_new1[grep("\\(b\\)",res7_new1$toxval_numeric),"critical_effect"] <-  paste0(res7_new1[grep("\\(b\\)",res7_new1$toxval_numeric),"critical_effect"], "[key information on toxval_numeric value derivation - ", res7_key[2,7], "]")
  res7_new1[grep("\\(b\\)",res7_new1$toxval_numeric),"toxval_numeric"] <- gsub("(^\\d+.*)(\\s+.*)", "\\1",res7_new1[grep("\\(b\\)",res7_new1$toxval_numeric),"toxval_numeric"])
  res7_new1$sex <- gsub("(\\d+\\.*\\d*\\s*)(\\w*)","\\2",res7_new1$toxval_numeric)
  res7_new1[grep("\\bM\\b|\\bM \\b",res7_new1$sex), "sex"] <- "Male"
  res7_new1[grep("\\bF\\b|\\bF \\b",res7_new1$sex), "sex"] <- "Female"
  res7_new1[grep("E-",res7_new1$sex), "sex"] <- "-"
  res7_new1$sex <- gsub("\\s+$","",res7_new1$sex)
  res7_new1[grep("E-",res7_new1$toxval_numeric), "toxval_numeric"] <- as.numeric(res7_new1[grep("E-",res7_new1$toxval_numeric), "toxval_numeric"])
  res7_new1$toxval_numeric <- as.numeric(gsub("(\\d+\\.*\\d*)(\\s*\\w*)","\\1",res7_new1$toxval_numeric))
  res7_new1$study_duration_value <- as.numeric(res7_new1$study_duration_value)
  res7_new1[is.na(res7_new1$toxval_numeric),"toxval_units"] <- "-"
  res7_new1$record_url <- "https://www.atsdr.cdc.gov/toxprofiles/tp200-c3.pdf"
  res7_new1 <- res7_new1[,-c(6,8)]

  #####################################################################
  cat("Build combined dataframe of all atsdr pfas sources \n")
  #####################################################################

  comb_res <- list(res3_new1,res4_new1,res5_new1,res6_new1,res7_new1)
  common_cols <- Reduce(intersect, lapply(comb_res, colnames))
  reqd_cols <- c(names(res3_new1),"record_url")
  # create new_res by subsetting with the columns present in required cols
  comb_res <- lapply(comb_res, function(x) subset(x, select = intersect(reqd_cols, colnames(x))))
  # In case any required col not being present in a data frame add that column and assign it as empty
  for ( i in 1:length(comb_res)){
    comb_res[[i]][setdiff(reqd_cols,names(comb_res[[i]]))] <- ""
  }

  all_res <- do.call("rbind",comb_res)
  row.names(all_res) <- NULL
  all_res <- unique(all_res)
  all_res["atsdr_pfas_id"] <- c(1:length(all_res[,1]))
  all_res <- all_res[c("atsdr_pfas_id",names(all_res[-21]))]
  all_res <- lapply(all_res, function(x) type.convert(as.character(x), as.is = T))
  all_res <- data.frame(all_res, stringsAsFactors = F)
  names(all_res) <- tolower(names(all_res))
  # assign short ref where long ref not available
  all_res[which(all_res$long_ref == ""),"long_ref"] <- all_res[which(all_res$long_ref == ""),"short_ref"]
  # assign source_url where record_url not available
  all_res[which(all_res$record_url == ""),"record_url"] <- all_res[which(all_res$record_url == ""),"source_url"]

  all_res[grep("\\[",all_res$critical_effect),"critical_effect"] <- gsub("\\[","(",all_res[grep("\\[",all_res$critical_effect),"critical_effect"])
  all_res[grep("\\]",all_res$critical_effect),"critical_effect"] <- gsub("\\]",")",all_res[grep("\\]",all_res$critical_effect),"critical_effect"])

  all_res = subset(all_res,select=-c(atsdr_pfas_id))

  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="ATSDR PFAS",table="source_atsdr_pfas",res=all_res,F,T,T)
}
