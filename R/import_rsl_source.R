#--------------------------------------------------------------------------------------
#' Load rsl Source Info into dev_toxval_source_v2.
#' @param db The version of toxval into which the source info is loaded.
#' @param infile1a The input file ./rsl/rsl_files/final_rsl_thq_combined_nov21.xlsx
#' @param infile1b The input file ./rsl/rsl_files/final_rsl_subchronic_nov21.xlsx
#' @param infile2 The input file ./rsl/rsl_files/general_info_nov_21.xlsx
#' @param infile3 The input file ./rsl/rsl_files/key_description_nov_21.xlsx
#--------------------------------------------------------------------------------------
import_rsl_source <- function(db,
                              infile1a="../rsl/rsl_files/final_rsl_thq_combined_nov21.xlsx",
                              infile1b="../rsl/rsl_files/final_rsl_subchronic_nov21.xlsx",
                              infile2="../rsl/rsl_files/general_info_nov_21.xlsx",
                              infile3="../rsl/rsl_files/key_description_nov_21.xlsx",
                              chem.check.halt=T) {
  printCurrentFunction(db)

  #####################################################################
  cat("create rsl_combined_thq_table \n")
  #####################################################################
  final_rsl_file2 <- openxlsx::read.xlsx(infile1a, 1)
  # create subset with non screening level variables removed
  cols_to_remove <- c("SFO.(mg/kg-day)-1","k.e.y._.SFO.(mg/kg-day)-1","IUR.(ug/m3)-1","k.e.y._.IUR.(ug/m3)-1","v.o.l","mutagen","GIABS","ABSd","Csat.(mg/kg)","MCL.(ug/L)","MCL-based.SSL.(mg/kg)")
  final_rsl_file3 <- final_rsl_file2[,!names(final_rsl_file2) %in% cols_to_remove]

  # create subset with the removed screening level variables, exception vol and mutagen
  non_screen_levels <- c("SFO.(mg/kg-day)-1","k.e.y._.SFO.(mg/kg-day)-1","IUR.(ug/m3)-1","k.e.y._.IUR.(ug/m3)-1","GIABS","ABSd","Csat.(mg/kg)","MCL.(ug/L)","MCL-based.SSL.(mg/kg)","Analyte","CAS.No.")
  non_sv <- final_rsl_file2[,names(final_rsl_file2) %in% non_screen_levels]
  non_sv <- unique(non_sv)
  names(non_sv) <- c("SFO_(mg/kg-day)-1","key_SFO","IUR_(ug/m3)-1","key_IUR","GIABS","ABSd","Csat_(mg/kg)", "name","casrn","MCL_(ug/L)","MCL-based_SSL_(mg/kg)")

  #####################################################################
  cat("SFO\n")
  #####################################################################
  n_1 <- non_sv[,c(1,2,8,9)]
  colnames(n_1)[3] <- c("name")
  colnames(n_1)[4] <- c("casrn")
  colnames(n_1)[1] <- c("values")
  colnames(n_1)[2] <- c("keys")
  n_1["types"] <- c(rep("SFO", nrow(n_1)))
  n_1["units"] <- c(rep("(mg/kg-day)-1", nrow(n_1)))
  n_1 <- n_1[,c(3:4,1:2,5:6)]
  #####################################################################
  cat("IUR\n")
  #####################################################################
  n_2 <- non_sv[,c(3,4,8,9)]
  colnames(n_2)[3] <- c("name")
  colnames(n_2)[4] <- c("casrn")
  colnames(n_2)[1] <- c("values")
  colnames(n_2)[2] <- c("keys")
  n_2["types"] <- c(rep("IUR", nrow(n_2)))
  n_2["units"] <- c(rep("(ug/m3)-1", nrow(n_2)))
  n_2 <- n_2[,c(3:4,1:2,5:6)]
  #####################################################################
  cat("GIABS\n")
  #####################################################################
  n_3 <- non_sv[,c(5,8,9)]
  colnames(n_3)[2] <- c("name")
  colnames(n_3)[3] <- c("casrn")
  colnames(n_3)[1] <- c("values")
  n_3["keys"] <- c(rep("-", nrow(n_3)))
  n_3["types"] <- c(rep("GIABS", nrow(n_3)))
  n_3["units"] <- c(rep("-", nrow(n_3)))
  n_3 <- n_3[,c(2:3,1,4:6)]
  #####################################################################
  cat("ABSd\n")
  #####################################################################
  n_4 <- non_sv[,c(6,8,9)]
  colnames(n_4)[2] <- c("name")
  colnames(n_4)[3] <- c("casrn")
  colnames(n_4)[1] <- c("values")
  n_4["keys"] <- c(rep("-", nrow(n_4)))
  n_4["types"] <- c(rep("ABSd", nrow(n_4)))
  n_4["units"] <- c(rep("-", nrow(n_4)))
  n_4 <- n_4[,c(2:3,1,4:6)]

  #####################################################################
  cat("Csat\n")
  #####################################################################
  n_5 <- non_sv[,c(7,8,9)]
  colnames(n_5)[2] <- c("name")
  colnames(n_5)[3] <- c("casrn")
  colnames(n_5)[1] <- c("values")
  n_5["keys"] <- c(rep("-", nrow(n_5)))
  n_5["types"] <- c(rep("Soil Saturation Limit (Csat)", nrow(n_5)))
  n_5["units"] <- c(rep("mg/kg", nrow(n_5)))
  n_5 <- n_5[,c(2:3,1,4:6)]
  #####################################################################
  cat("MCL\n")
  #####################################################################
  n_6 <- non_sv[,c(10,8,9)]
  colnames(n_6)[2] <- c("name")
  colnames(n_6)[3] <- c("casrn")
  colnames(n_6)[1] <- c("values")
  n_6["keys"] <- c(rep("OW", nrow(n_6)))
  n_6["types"] <- c(rep("MCL", nrow(n_6)))
  n_6["units"] <- c(rep("ug/L", nrow(n_6)))
  n_6 <- n_6[,c(2:3,1,4:6)]

  #####################################################################
  cat("MCL-based_SSL\n")
  #####################################################################
  n_7 <- non_sv[,c(11,8,9)]
  colnames(n_7)[2] <- c("name")
  colnames(n_7)[3] <- c("casrn")
  colnames(n_7)[1] <- c("values")
  n_7["keys"] <- c(rep("-", nrow(n_7)))
  n_7["types"] <- c(rep("Protection of Groundwater: MCL-based SSL", nrow(n_7)))
  n_7["units"] <- c(rep("mg/kg", nrow(n_7)))
  n_7 <- n_7[,c(2:3,1,4:6)]
  non_sv <- rbind(n_1,n_2,n_3,n_4,n_5,n_6,n_7)
  non_sv <- non_sv[which(!is.na(non_sv$values)),]
  non_sv$exposure_route <- "-"

  # seperate out rfd and rfc
  final_rfd_rfc <-  final_rsl_file3[,names(final_rsl_file3)[grep("Rf|Analyte|CAS", names(final_rsl_file3))]]
  final_rfd_rfc <- unique(final_rfd_rfc)
  #names(final_rfd_rfc) <- c("RfDo_(mg/kg-day)","key_RfDo", "RfCi_(mg/m3)","key_RfCi", "name", "casrn")
  #####################################################################
  cat("RfDo\n")
  #####################################################################
  c_1 <- final_rfd_rfc[,c(5,6,1,2)]
  colnames(c_1)[1] <- c("name")
  colnames(c_1)[2] <- c("casrn")
  colnames(c_1)[3] <- c("values")
  colnames(c_1)[4] <- c("keys")
  c_1["types"] <- c(rep("RfDo", nrow(c_1)))
  c_1["units"] <- c(rep("mg/kg-day", nrow(c_1)))

  #####################################################################
  cat("RfCi\n")
  #####################################################################
  c_2 <- final_rfd_rfc[,c(5,6,3,4)]
  colnames(c_2)[1] <- c("name")
  colnames(c_2)[2] <- c("casrn")
  colnames(c_2)[3] <- c("values")
  colnames(c_2)[4] <- c("keys")
  c_2["types"] <- c(rep("RfCi", nrow(c_2)))
  c_2["units"] <- c(rep("mg/m3", nrow(c_2)))
  final_rfd_rfc <- rbind(c_1,c_2)
  final_rfd_rfc <- final_rfd_rfc[which(!is.na(final_rfd_rfc$values)),]
  final_rfd_rfc$exposure_route <- "-"
  final_screening <- final_rsl_file3[,names(final_rsl_file3)[grep("Rf", names(final_rsl_file3), invert = T)]]
  final_screening <- lapply(final_screening, function(x) type.convert(as.character(x), as.is = T))
  final_screening <- data.frame(final_screening, stringsAsFactors = F)
  names(final_screening) <- c("name","casrn", "Resident_Soil_(mg/kg)","key_Resident_Soil","Industrial_Soil_(mg/kg)","key_Industrial_Soil", "Resident_Air_(ug/m3)","key_Resident_Air","Industrial_Air_(ug/m3)","key_Industrial_Air","Tapwater_(ug/L)","key_Tapwater","Risk_based_SSL_(mg/kg)","key_Risk_based_SSL","sub_type")

  #####################################################################
  cat("Resident_Soil\n")
  #####################################################################
  s_1 <- final_screening[,c(1,2,3,4,15)]
  colnames(s_1)[1] <- c("name")
  colnames(s_1)[2] <- c("casrn")
  colnames(s_1)[3] <- c("values")
  colnames(s_1)[4] <- c("risk_assessment_class")
  colnames(s_1)[5] <- c("sub_type")
  s_1["sub_type"] <- paste("Thq = ",s_1$sub_type)
  s_1["types"] <- c(rep("Screening Level (Resident Soil)", nrow(s_1)))
  s_1["units"] <- c(rep("mg/kg", nrow(s_1)))
  s_1["keys"] <- c(rep("EPA Regions", nrow(s_1)))

  #####################################################################
  cat("Industrial_Soil\n")
  #####################################################################
  s_2 <- final_screening[,c(1,2,5,6,15)]
  colnames(s_2)[1] <- c("name")
  colnames(s_2)[2] <- c("casrn")
  colnames(s_2)[3] <- c("values")
  colnames(s_2)[4] <- c("risk_assessment_class")
  colnames(s_2)[5] <- c("sub_type")
  s_2["sub_type"] <- paste("Thq = ",s_2$sub_type)
  s_2["types"] <- c(rep("Screening Level (Industrial Soil)", nrow(s_2)))
  s_2["units"] <- c(rep("mg/kg", nrow(s_2)))
  s_2["keys"] <- c(rep("EPA Regions", nrow(s_2)))

  #####################################################################
  cat("Resident_Air\n")
  #####################################################################
  s_3 <- final_screening[,c(1,2,7,8,15)]
  colnames(s_3)[1] <- c("name")
  colnames(s_3)[2] <- c("casrn")
  colnames(s_3)[3] <- c("values")
  colnames(s_3)[4] <- c("risk_assessment_class")
  colnames(s_3)[5] <- c("sub_type")
  s_3["sub_type"] <- paste("Thq = ",s_3$sub_type)
  s_3["types"] <- c(rep("Screening Level (Resident Air)", nrow(s_3)))
  s_3["units"] <- c(rep("ug/m3", nrow(s_3)))
  s_3["keys"] <- c(rep("EPA Regions", nrow(s_3)))

  #####################################################################
  cat("Industrial_Air\n")
  #####################################################################
  s_4 <- final_screening[,c(1,2,9,10,15)]
  colnames(s_4)[1] <- c("name")
  colnames(s_4)[2] <- c("casrn")
  colnames(s_4)[3] <- c("values")
  colnames(s_4)[4] <- c("risk_assessment_class")
  colnames(s_4)[5] <- c("sub_type")
  s_4["sub_type"] <- paste("Thq = ",s_4$sub_type)
  s_4["types"] <- c(rep("Screening Level (Industrial Air)", nrow(s_4)))
  s_4["units"] <- c(rep("ug/m3", nrow(s_4)))
  s_4["keys"] <- c(rep("EPA Regions", nrow(s_4)))

  #####################################################################
  cat("Tapwater\n")
  #####################################################################
  s_5 <- final_screening[,c(1,2,11,12,15)]
  colnames(s_5)[1] <- c("name")
  colnames(s_5)[2] <- c("casrn")
  colnames(s_5)[3] <- c("values")
  colnames(s_5)[4] <- c("risk_assessment_class")
  colnames(s_5)[5] <- c("sub_type")
  s_5["sub_type"] <- paste("Thq = ",s_5$sub_type)
  s_5["types"] <- c(rep("Screening Level (Tapwater)", nrow(s_5)))
  s_5["units"] <- c(rep("ug/L", nrow(s_5)))
  s_5["keys"] <- c(rep("EPA Regions", nrow(s_5)))

  #####################################################################
  cat("Risk_based_SSL\n")
  #####################################################################
  s_6 <- final_screening[,c(1,2,13,14,15)]
  colnames(s_6)[1] <- c("name")
  colnames(s_6)[2] <- c("casrn")
  colnames(s_6)[3] <- c("values")
  colnames(s_6)[4] <- c("risk_assessment_class")
  colnames(s_6)[5] <- c("sub_type")
  s_6["sub_type"] <- paste("Thq = ",s_6$sub_type)
  #s_6["sub_type"] <- c(rep("-", nrow(s_6)))
  s_6["types"] <- c(rep("Protection of Groundwater: Risk-based SSL", nrow(s_6)))
  s_6["units"] <- c(rep("mg/kg", nrow(s_6)))
  s_6["keys"] <- c(rep("EPA Regions", nrow(s_6)))

  final_screening <- rbind(s_1,s_2,s_3,s_4,s_5,s_6)
  final_screening <- final_screening[which(!is.na(final_screening$values)),]
  final_screening[grep("^cancer",final_screening$risk_assessment_class),"sub_type"] <- "TR=1E-06"
  final_screening$values <- as.numeric(final_screening$values)
  final_screening$exposure_route <- "-"
  #####################################################################
  cat("create rsl_subchronic_table \n")
  #####################################################################
  subchr_data <- openxlsx::read.xlsx(infile1b, 1)
  subchr_data2 <- subchr_data
  sd_1 <- subchr_data2[,c(1:4,7:8)]
  sd_2 <- subchr_data2[,c(1:2,5:6, 9:10)]

  #####################################################################
  cat("RfDo\n")
  #####################################################################
  sc_1 <- sd_1[,c(1,2,3,4)]
  colnames(sc_1)[3] <- c("values")
  colnames(sc_1)[4] <- c("keys")
  sc_1["types"] <- c(rep("RfDo", nrow(sc_1)))
  sc_1["units"] <- c(rep("mg/kg-day", nrow(sc_1)))
  sc_1["exposure_route"] <- c(rep("oral", nrow(sc_1)))

  #####################################################################
  cat("RfCi\n")
  #####################################################################
  sc_2 <- sd_1[,c(1,2,5,6)]
  colnames(sc_2)[3] <- c("values")
  colnames(sc_2)[4] <- c("keys")
  sc_2["types"] <- c(rep("RfCi", nrow(sc_2)))
  sc_2["units"] <- c(rep("mg/m3", nrow(sc_2)))
  sc_2["exposure_route"] <- c(rep("inhalation", nrow(sc_2)))

  #####################################################################
  cat("SRfDo\n")
  #####################################################################
  sc_3 <- sd_2[,c(1,2,3,4)]
  colnames(sc_3)[3] <- c("values")
  colnames(sc_3)[4] <- c("keys")
  sc_3["types"] <- c(rep("SRfDo", nrow(sc_3)))
  sc_3["units"] <- c(rep("mg/kg-day", nrow(sc_3)))
  sc_3["exposure_route"] <- c(rep("oral", nrow(sc_3)))

  #####################################################################
  cat("SRfCi\n")
  #####################################################################
  sc_4 <- sd_2[,c(1,2,5,6)]
  colnames(sc_4)[3] <- c("values")
  colnames(sc_4)[4] <- c("keys")
  sc_4["types"] <- c(rep("SRfCi", nrow(sc_4)))
  sc_4["units"] <- c(rep("mg/m3", nrow(sc_4)))
  sc_4["exposure_route"] <- c(rep("inhalation", nrow(sc_4)))
  subchr_data2 <-  rbind(sc_1, sc_2, sc_3, sc_4)
  subchr_data2 <- subchr_data2[which(!is.na(subchr_data2$values)),]

  #combine thq rf values with subchronic rf values
  final_rf_with_subchronic <- rbind(final_rfd_rfc,subchr_data2)
  final_rf_with_subchronic <- unique(final_rf_with_subchronic)

  # combine reference data with both chronic and subchronic with non screening level data
  final_non_screening_data <- rbind(final_rf_with_subchronic,non_sv)
  final_non_screening_data <- lapply(final_non_screening_data, function(x) type.convert(as.character(x), as.is = T))
  final_non_screening_data <- data.frame(final_non_screening_data, stringsAsFactors = F)
  final_non_screening_data$sub_type <- "-"
  final_non_screening_data$risk_assessment_class <- "-"
  final_non_screening_data <- final_non_screening_data[,c(1:3,9,8,5:6,4,7)]
  #remove (G) from values like 7.0E+06(G)
  final_non_screening_data[grep("\\(G\\)$",final_non_screening_data$values), "values"] <- gsub("(.*)(\\(G\\)$)","\\1",final_non_screening_data[grep("\\(G\\)$",final_non_screening_data$values), "values"])
  final_non_screening_data$values <- as.numeric(final_non_screening_data$values)

  # final_rsl combining both final_screening and final_non_screening_data
  final_rsl <- rbind(final_screening, final_non_screening_data)
  final_rsl$source <- "RSL"
  colnames(final_rsl)[8] <- c("subsource")

  final_rsl$study_type <- final_rsl$risk_assessment_class
  physchem_class <- c("GIABS","Soil Saturation Limit (Csat)","ABSd")
  final_rsl[which(final_rsl$types %in% physchem_class),"risk_assessment_class"] <- "PhysChem"
  chronic_class <- c("Screening Level (Resident Soil)","Screening Level (Industrial Soil)","Screening Level (Resident Air)","Screening Level (Industrial Air)","Screening Level (Tapwater)","Protection of Groundwater: Risk-based SSL",
                     "Protection of Groundwater: MCL-based SSL","RfDo","RfCi")
  final_rsl[which(final_rsl$types %in% chronic_class),"risk_assessment_class"] <- "chronic"
  cancer_class <- c("SFO","IUR")
  final_rsl[which(final_rsl$types %in% cancer_class),"risk_assessment_class"] <- "carcinogenicity"
  subchronic_class <- c("SRfDo","SRfCi")
  final_rsl[which(final_rsl$types %in% subchronic_class),"risk_assessment_class"] <- "subchronic"
  final_rsl$source_url <- "https://www.epa.gov/risk/regional-screening-levels-rsls-generic-tables"
  final_rsl <- lapply(final_rsl, function(x) type.convert(as.character(x), as.is = T))
  final_rsl <- data.frame(final_rsl, stringsAsFactors = F)
  final_rsl <- unique(final_rsl)
  names(final_rsl) <- c("name","casrn","toxval_numeric","risk_assessment_class","toxval_subtype","toxval_type","toxval_units","subsource","exposure_route","source","study_type", "source_url")
  final_rsl["source_id"] <- c(1:dim(final_rsl)[1])
  final_rsl <- final_rsl[,c("source_id",names(final_rsl[-13]))]
  final_rsl["source_hash"] <- "-"
  final_rsl <- final_rsl[,c("source_id","source_hash","name","casrn","toxval_numeric","risk_assessment_class","toxval_subtype","toxval_type","toxval_units","subsource","exposure_route","source","study_type", "source_url")]
  #  print(View(final_rsl))

  x = substr(final_rsl$casrn,1,1)
  mask = vector(length=length(x),mode="integer")
  mask[] = 1
  mask[x=="E"] = 0
  final_rsl = final_rsl[mask==1,]
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="RSL",table="source_rsl",res=final_rsl,F,T,T)
}
