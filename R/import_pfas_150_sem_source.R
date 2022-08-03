#--------------------------------------------------------------------------------------
#' Load PFAS 150 SEM Source data into toxval_source
#' @param db The version of toxval_source into which the source info is loaded.
#' @param infile The input file ./PFAS 150 SEM/PFAS 150 SEM_files/PFAS150 animal study template combined_clearance with DTXSID and CASRN.xlsx
#' @param chem.check.halt If TRUE and there are problems with chemicals CASRN checks, halt the program
#--------------------------------------------------------------------------------------
import_pfas_150_sem_source <- function(db,
                                       infile="PFAS150 animal study template combined_clearance with DTXSID and CASRN.xlsx",
                                       chem.check.halt=F) {
  printCurrentFunction(db)
  infile = paste0(toxval.config()$datapath,"PFAS 150 SEM/PFAS 150 SEM_files/",infile)
  #####################################################################
  cat("Build original_pfas_150_sem\n")
  #####################################################################
  print(infile)
  res <- openxlsx::read.xlsx(infile,sheet=4)
  names(res)[names(res) == "Citation"] <- "long_ref"
  names(res)[names(res) == "Chemical"] <- "name"
  names(res)[names(res) == "CASRN"] <- "casrn"
  res$year <- res$long_ref
  res$sex <- res$Animl.Group.Name
  res$strain <- res$Animl.Group.Name
  res$species <- res$Animl.Group.Name
  res$exposure_route <- res$Exposure.Route
  res$exposure_method <- res$Exposure.Route
  res$study_type <- res$Exposure.Duration
  res$study_duration_value <- res$Exposure.Duration
  res$study_duration_units <- res$Exposure.Duration
  res$dose <- res$Doses.and.responses

  # considering only study level values and critical effect based on study level rationale, so subset system level
  res1 <- res[,-c(2,4,9:13,14:20,26 )]
  res1 <- unique(res1)
  s1 <- res1[,c(1:6, 7,8,21,11, 12:20)]
  names(s1)[7] <- "toxval_type"
  names(s1)[8] <- "toxval_numeric"
  names(s1)[9] <- "toxval_units"
  names(s1)[10] <- "critical_effect"
  s2 <- res1[,c(1:6, 9,10,21,11, 12:20)]
  names(s2)[7] <- "toxval_type"
  names(s2)[8] <- "toxval_numeric"
  names(s2)[9] <- "toxval_units"
  names(s2)[10] <- "critical_effect"
  s <- rbind(s1,s2)

  # subset records having na and ndr toxval_numeric values
  s <- s[which(!is.na(s$toxval_numeric) & s$toxval_numeric != "NDr"),]
  ############################################################################
  s_num <-  s[grep("^\\d+$", s$toxval_numeric),]
  #1826
  s_num_dup <- s_num[duplicated(s_num[,-9]) | duplicated(s_num[,-9], fromLast = TRUE),]
  #1808
  # non duplicated records having numeric values
  s_num_non_dup <- setdiff(s_num,s_num_dup)
  #18
  # extracting records that have the numeric value represented in the doses
  s_num_dup_1 <- s_num_dup[mapply(grepl, s_num_dup$toxval_numeric, s_num_dup$toxval_units),]
  #1415
  s_num_dup_1$toxval_units_1 <- s_num_dup_1$toxval_units
  for (i in 1:nrow(s_num_dup_1)){
    s_num_dup_1$toxval_units_1[i] <- gsub(paste("(.*\\s+)(",s_num_dup_1$toxval_numeric[i],")(\\s+)(.*)(\\s+.*)", sep = ""), "\\4", s_num_dup_1$toxval_units[i])
    s_num_dup_1$toxval_units_1[i] <- gsub("(.*?)(\\:.*)","\\1", s_num_dup_1$toxval_units_1[i])
    s_num_dup_1$toxval_units_1[i] <- gsub("(^\\d+\\s+)(.*)","\\2", s_num_dup_1$toxval_units_1[i])
  }

  s_num_non_dup_1 <- s_num_non_dup[mapply(grepl, s_num_non_dup$toxval_numeric, s_num_non_dup$toxval_units),]
  #16

  s_num_non_dup_1_missing <- setdiff(s_num_non_dup, s_num_non_dup_1)
  #2

  s_num_non_dup_1$toxval_units_1 <- s_num_non_dup_1$toxval_units
  for (i in 1:nrow(s_num_non_dup_1)){
    s_num_non_dup_1$toxval_units_1[i] <- gsub(paste("(.*\\s+)(",s_num_non_dup_1$toxval_numeric[i],")(\\s+)(.*)(\\s+.*)", sep = ""), "\\4", s_num_non_dup_1$toxval_units[i])
    s_num_non_dup_1$toxval_units_1[i] <- gsub("(.*?)(\\:.*)","\\1", s_num_non_dup_1$toxval_units_1[i])
    s_num_non_dup_1$toxval_units_1[i] <- gsub("(.*\\d+\\s+)(mg.*)","\\2", s_num_non_dup_1$toxval_units_1[i])
  }

  s_num_non_dup_1_missing$toxval_units_1 <- s_num_non_dup_1_missing$toxval_units

  # combining subsets with 1. duplicated entries having value matches in doses,
  #2. non duplicated entries having value matches in doses,
  #3. non duplcated entries not having value matches in doses
  s_num_new <- rbind(s_num_dup_1,s_num_non_dup_1, s_num_non_dup_1_missing)
  #####################################################################################
  # testing with decimal values
  s[grep("\\E", s$toxval_numeric),"toxval_numeric"] <- as.numeric(s[grep("\\E", s$toxval_numeric),"toxval_numeric"])
  s[grep("0.042", s$toxval_numeric),"toxval_numeric"] <- 0.04
  s_dec <- s[grep("^\\d+\\.\\d+$", s$toxval_numeric),]
  #675
  s_dec_dup <- s_dec[duplicated(s_dec[,-9]) | duplicated(s_dec[,-9], fromLast = TRUE),]
  #672
  # non duplicated records having decimal values
  s_dec_non_dup <- setdiff(s_dec,s_dec_dup)
  #3
  # extracting records that have the decimal value represented in the doses
  s_dec_dup_1 <- s_dec_dup[mapply(grepl, s_dec_dup$toxval_numeric, s_dec_dup$toxval_units),]
  #298
  s_dec_dup_1[grep("0.04", s_dec_dup_1$toxval_numeric),"toxval_numeric"] <- 0.042
  s_dec_dup_1$toxval_units_1 <- s_dec_dup_1$toxval_units
  for (i in 1:nrow(s_dec_dup_1)){
    s_dec_dup_1$toxval_units_1[i] <- gsub(paste("(.*\\s+)(",s_dec_dup_1$toxval_numeric[i],")(\\s+)(.*)(\\s+.*)", sep = ""), "\\4", s_dec_dup_1$toxval_units[i])
    s_dec_dup_1$toxval_units_1[i] <- gsub("(.*?)(\\:.*)","\\1", s_dec_dup_1$toxval_units_1[i])
    s_dec_dup_1$toxval_units_1[i] <- gsub("(^\\d+\\s+)(.*)","\\2", s_dec_dup_1$toxval_units_1[i])
  }

  s_dec_non_dup_1 <- s_dec_non_dup[mapply(grepl, s_dec_non_dup$toxval_numeric, s_dec_non_dup$toxval_units),]
  #3
  s_dec_non_dup_1_missing <- setdiff(s_dec_non_dup, s_dec_non_dup_1)
  #0

  s_dec_non_dup_1$toxval_units_1 <- s_dec_non_dup_1$toxval_units
  for (i in 1:nrow(s_dec_non_dup_1)){
    s_dec_non_dup_1$toxval_units_1[i] <- gsub("(.*?)(\\:.*)","\\1", s_dec_non_dup_1$toxval_units_1[i])
    s_dec_non_dup_1$toxval_units_1[i] <- gsub("(.*\\d+\\s+)(mg.*)","\\2", s_dec_non_dup_1$toxval_units_1[i])
  }

  #s_dec_non_dup_1_missing$toxval_units_1 <- s_dec_non_dup_1_missing$toxval_units
  # combining subsets with 1. duplicated entries having value matches in doses,
  #2. non duplicated entries having value matches in doses,
  #3. non duplcated entries not having value matches in doses
  if(nrow(s_dec_non_dup_1_missing)>0)
    s_dec_new <- rbind(s_dec_dup_1,s_dec_non_dup_1, s_dec_non_dup_1_missing)
  else
    s_dec_new <- rbind(s_dec_dup_1,s_dec_non_dup_1)
  ##############################################################################
  #testing range values
  s_range <- s[grep("\\d+\\-\\d+", s$toxval_numeric),]
  #0

  ################################################################################
  res2 <- rbind(s_num_new, s_dec_new)
  names(res2)[names(res2) == "toxval_units"] <- "dose"
  names(res2)[names(res2) == "toxval_units_1"] <- "toxval_units"
  res3 <- res2[,-c(4,5,6,9)]
  # values having ppm as units when the correct units were mg/m3 as represented in the data(10064, 30596)
  res3_fix_units <- res3[grep("\\:", res3$toxval_units),"toxval_numeric"]
  res3[which(res3$toxval_numeric %in% res3_fix_units[1]),"toxval_units"] <- "mg/m3"
  res3[which(res3$toxval_numeric %in% res3_fix_units[2]),"toxval_units"] <- "mg/m3"
  # values like ± 0, 25 mg/kg-day in units
  res3[grep("^[^[:alnum:]]", res3$toxval_units),"toxval_units"] <- gsub("(.*\\s+)(mg.*)","\\2",res3[grep("^[^[:alnum:]]", res3$toxval_units),"toxval_units"])
  res3 <- unique(res3)
  res2 <- res3
  rm(res3)
  ##################################################################################

  ##################################################################################
  #fix year
  for (i in 1:nrow(res2)){
    res2$year[i] <- gsub("(.*)([0-9]{4})(.*)","\\2",res2$year[i])
  }

  res2$year <- as.integer(res2$year)
  ##################################################################################
  #fix sex
  res2$sex2 <- res2$sex

  res2[grep("\\bFemale\\b", res2$sex2, ignore.case = T),"sex"] <- "Female"
  res2[grep("\\bMale\\b", res2$sex2, ignore.case = T),"sex"] <- paste("Male", res2[grep("\\bMale\\b", res2$sex2, ignore.case = T),"sex"] , sep = '/')
  res2$sex <- gsub("(.*)(\\/\\-$)","\\1",res2$sex)
  res2$sex <- gsub("(Male)(\\/[A-Z]*\\d*\\s*Male.*$)","\\1",res2$sex)
  res2$sex[res2$sex != "Male"& res2$sex != "Female"& res2$sex != "Male/Female"] <- "-"

  res2 <- res2[,names(res2) != "sex2"]
  ####################################################################################
  #fix generation

  res2$generation <- gsub("(^[A-Z]{1}[0-9]{1})(\\s+.*)","\\1",res2$strain)
  res2[grep("^\\w+\\s+\\w+.*",res2$generation),"generation"]  <- "-"
  res2$generation[res2$generation == " "] <- "-"

  ####################################################################################
  #fix species and strain
  res2[grep("Female", res2$species, ignore.case = T),"strain"] <- gsub("(.*Female\\s*)(.*)","\\2",res2[grep("Female", res2$species, ignore.case = T),"species"])
  res2[grep("Female", res2$species, ignore.case = T),"species"] <- gsub("(.*Female\\s*)(.*)","\\2",res2[grep("Female", res2$species, ignore.case = T),"species"])
  res2[grep("Male", res2$species, ignore.case = T),"strain"] <- gsub("(.*Male\\s*)(.*)","\\2",res2[grep("Male", res2$species, ignore.case = T),"species"])
  res2[grep("Male", res2$species, ignore.case = T),"species"] <- gsub("(.*Male\\s*)(.*)","\\2",res2[grep("Male", res2$species, ignore.case = T),"species"])
  res2[grep("^F1", res2$species, ignore.case = T),"strain"] <- gsub("(F1\\s+)(.*)","\\2",res2[grep("^F1", res2$species, ignore.case = T),"species"])
  res2[grep("^F1", res2$species, ignore.case = T),"species"] <- gsub("(F1\\s+)(.*)","\\2",res2[grep("^F1", res2$species, ignore.case = T),"species"])
  res2[grep("\\s+", res2$species, ignore.case = T),"strain"] <- gsub("(.*)(\\s+\\w+$)","\\1",res2[grep("\\s+", res2$species, ignore.case = T),"species"])
  res2[grep("\\s+", res2$species, ignore.case = T),"species"] <- gsub("(.*\\s+)(\\w+$)","\\2",res2[grep("\\s+", res2$species, ignore.case = T),"species"])
  res2$strain[res2$strain == " "] <- "-"
  res2$strain[res2$strain == "Rats"] <- "-"
  res2$species[res2$species == " "] <- "-"
  res2$species[res2$species == "Crl:CD(SD)"] <- "-"
  res2$species <- tolower(res2$species)
  res2$species <- gsub("(.*)(s$)","\\1", res2$species)

  ####################################################################################
  # fix exposure route and exposure_method
  res2$exposure_route <- gsub("(.*?)(\\s+.*)","\\1", res2$exposure_route)
  res2$exposure_method <-gsub("(.*?)(\\s+\\-*\\s*)(.*)","\\3", res2$exposure_method)

  #####################################################################################
  # fix study type, study duration value and study duration units
  res2$study_type <- gsub("(.*?)(\\s+\\(.*)","\\1", res2$study_type)
  # GD values in ranges
  res2[grep(".*GD.*\\)\\)$",res2$study_duration_value),"study_duration_value"] <- gsub("(.*)(\\s+\\(+.*\\/.*\\)+)(\\)+$)","\\1\\3",res2[grep(".*GD.*\\)\\)$",res2$study_duration_value),"study_duration_value"])
  res2[grep("GD.*-LD\\s+\\d+\\)+$",res2$study_duration_value),"study_duration_units"] <- gsub("(.*\\-)(\\w+)(\\s+)(\\d+)(\\)+$)","\\2",res2[grep("GD.*-LD\\s+\\d+\\)+$",res2$study_duration_value),"study_duration_value"])
  res2[grep("GD.*-LD\\s+\\d+\\)+$",res2$study_duration_value),"study_duration_value"] <- gsub("(.*\\-)(\\w+)(\\s+)(\\d+)(\\)+$)","\\4",res2[grep("GD.*-LD\\s+\\d+\\)+$",res2$study_duration_value),"study_duration_value"])
  res2[grep(".*PND\\s+\\d+\\-\\d+\\)+$",res2$study_duration_value),"study_duration_units"] <- gsub("(.*)(PND)(\\s+\\d+\\-)(\\d+)(\\)+$)","\\2",res2[grep(".*PND\\s+\\d+\\-\\d+\\)+$",res2$study_duration_value),"study_duration_value"])
  res2[grep(".*PND\\s+\\d+\\-\\d+\\)+$",res2$study_duration_value),"study_duration_value"] <- gsub("(.*)(PND)(\\s+\\d+\\-)(\\d+)(\\)+$)","\\4",res2[grep(".*PND\\s+\\d+\\-\\d+\\)+$",res2$study_duration_value),"study_duration_value"])
  res2[grep("GD",res2$study_duration_value),"study_duration_units"] <- gsub("(.*)(GD)(\\s+\\d+\\-)(\\d+)(\\)+$)","\\2",res2[grep("GD",res2$study_duration_value),"study_duration_value"])
  res2[grep("GD",res2$study_duration_value),"study_duration_value"] <- gsub("(.*)(GD)(\\s+\\d+\\-)(\\d+)(\\)+$)","\\4",res2[grep("GD",res2$study_duration_value),"study_duration_value"])
  # range values
  res2[grep("\\d+\\-\\d+",res2$study_duration_value),"study_duration_units"] <- gsub("(.*\\(+\\d+\\-)(\\d+)(\\s+)(\\w+)(.*)","\\4",res2[grep("\\d+\\-\\d+",res2$study_duration_value),"study_duration_value"])
  res2[grep("\\d+\\-\\d+",res2$study_duration_value),"study_duration_value"] <- gsub("(.*\\(+\\d+\\-)(\\d+)(\\s+)(\\w+)(.*)","\\2",res2[grep("\\d+\\-\\d+",res2$study_duration_value),"study_duration_value"])
  # up to values
  res2[grep("up to",res2$study_duration_value),"study_duration_value"] <- gsub("(.*)(up to )(.*)","\\1\\3",res2[grep("up to",res2$study_duration_value),"study_duration_value"])
  res2[grep("\\(+",res2$study_duration_value),"study_duration_units"] <- gsub("(.*?\\(+)(\\d+)(\\s*)(\\w+)(.*)","\\4",res2[grep("\\(+",res2$study_duration_value),"study_duration_value"])
  res2[grep("\\(+",res2$study_duration_value),"study_duration_value"] <- gsub("(.*?\\(+)(\\d+)(\\s*)(\\w+)(.*)","\\2",res2[grep("\\(+",res2$study_duration_value),"study_duration_value"])
  res2[grep("\\(+",res2$study_duration_value),"study_duration_units"] <- gsub("(.*\\-)(\\w+)(\\s+\\d+)(\\)+$)","\\2",res2[grep("\\(+",res2$study_duration_value),"study_duration_units"])
  res2[grep("\\(+",res2$study_duration_value),"study_duration_value"] <- gsub("(.*\\-)(\\w+\\s+)(\\d+)(\\)+$)","\\3",res2[grep("\\(+",res2$study_duration_value),"study_duration_value"])
  res2$study_duration_value <- as.numeric(res2$study_duration_value)
  res2$study_duration_units[which(res2$study_duration_units == "d")] <- "day"
  res2$study_duration_units[which(res2$study_duration_units == "yr")] <- "year"
  res2$study_duration_units[which(res2$study_duration_units == "wk")] <- "week"

  ##################################################################################
  # assign source_url
  res2$source_url <- "https://hawcprd.epa.gov/study/assessment/100500085/"
  # res2[grep("\\-", res2$toxval_numeric), "toxval_numeric"] <- gsub("(\\d+)(\\-\\d+)","\\1",res2[grep("\\-", res2$toxval_numeric), "toxval_numeric"])
  res2$toxval_numeric <- as.numeric(res2$toxval_numeric)
  # combine critical effects in cases where duplicated records exists except in critical effct field
  res2_b <- res2
  res2_c <- res2_b[duplicated(res2_b[,names(res2_b)[names(res2_b) != "critical_effect"]]) | duplicated(res2_b[,names(res2_b)[names(res2_b) != "critical_effect"]], fromLast = TRUE),]
  res2_d <- setdiff(res2_b,res2_c)
  res2_e <- aggregate(data=res2_c,critical_effect~long_ref+name+casrn+toxval_type+toxval_numeric+year+sex+strain+species+exposure_route+exposure_method+study_type+study_duration_value+study_duration_units+toxval_units+generation+source_url,FUN=paste, collapse= "|")
  res2_e <- unique(res2_e)
  res4 <- rbind(res2_d,res2_e)

  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="PFAS 150 SEM",table="source_pfas_150_sem",res=res4,F,T,T)
}
#
