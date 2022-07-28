#--------------------------------------------------------------------------------------
#' Load DOE Source into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile The input file ./doe/doe_files/Revision_29.xlsx
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#--------------------------------------------------------------------------------------
import_doe_source <- function(db,
                              infile="Revision_29.xlsx",
                              chem.check.halt=F) {
  printCurrentFunction(db)
  infile = paste0(toxval.config()$datapath,"doe/doe_files/",infile)
  #####################################################################
  cat("Build new_doe_table by combining data from all four sheets from input file \n")
  #####################################################################
  res1 <- openxlsx::read.xlsx(infile,1, rows = 4:3149, colNames = F )
  res1$original_molecular_weight_MW <- res1$X4
  res1$X4 <- gsub("\\-.*", "", res1$X4)
  res1$X4 <- gsub("(~)", "", res1$X4)
  res1$X4 <- gsub("\\s+$", "", res1$X4)

  non_numeric_mw <- grep("(kDa)", res1$X4, value = T)
  non_numeric_val <- gsub("(kDa)", "", non_numeric_mw)
  non_numeric_val <- gsub("\\s+$", "", non_numeric_val)
  non_numeric_val <- as.numeric(non_numeric_val)
  for (i in 1: length(non_numeric_val)){
    non_numeric_val[i] <- non_numeric_val[i] * 1000
  }

  res1$X4[res1$X4 %in% non_numeric_mw] <- non_numeric_val
  res1$X4 <- as.numeric(res1$X4)

  header_res1 <- openxlsx::read.xlsx(infile,1, rows = 1:3, colNames = F )
  header_res1 <- unlist(header_res1)
  header_res1 <- unname(header_res1[!is.na(header_res1)])
  header_res1[12] <-  paste0(header_res1[11], ",",header_res1[12])
  header_res1[13] <-  paste0(header_res1[11], ",",header_res1[13])
  header_res1 <-  header_res1[-11]
  header_res1[16] <-  paste0(header_res1[15], ",",header_res1[16])
  header_res1[17] <-  paste0(header_res1[15], ",",header_res1[17])
  header_res1[18] <-  paste0(header_res1[15], ",",header_res1[18])
  header_res1 <-  header_res1[-15]
  header_res1[c(2,3)]<- c("Chemical Name", "CASRN")
  names(res1)[1:17] <- header_res1
  res1 <- res1[,c(names(res1[1:4]), names(res1[18]), names(res1[5:17]))]

  res2 <- openxlsx::read.xlsx(infile,2, rows = 4:3149, colNames = F )
  key_pac1 <- grep("[*]",res2$X4, value = T)
  pac1_val <- gsub("[*]", "", key_pac1)
  pac1_key <- gsub("[^*]", "", key_pac1)
  res2$pac1_keys[res2$X4 %in% key_pac1] <- pac1_key
  res2$X4[res2$X4 %in% key_pac1] <- pac1_val

  key_pac2 <- grep("[*]",res2$X5, value = T)
  pac2_val <- gsub("[*]", "", key_pac2)
  pac2_key <- gsub("[^*]", "", key_pac2)
  res2$pac2_keys[res2$X5 %in% key_pac2] <- pac2_key
  res2$X5[res2$X5 %in% key_pac2] <- pac2_val

  key_pac3 <- grep("[*]",res2$X6, value = T)
  pac3_val <- gsub("[*]", "", key_pac3)
  pac3_key <- gsub("[^*]", "", key_pac3)
  res2$pac3_keys[res2$X6 %in% key_pac3] <- pac3_key
  res2$X6[res2$X6 %in% key_pac3] <- pac3_val

  header_res2 <- openxlsx::read.xlsx(infile,2, rows = 1:3, colNames = F )
  header_res2 <- unlist(header_res2)
  header_res2 <- unname(header_res2[!is.na(header_res2)])
  header_res2[5] <-  paste0(header_res2[4], ",",header_res2[5])
  header_res2[6] <-  paste0(header_res2[4], ",",header_res2[6])
  header_res2[7] <-  paste0(header_res2[4], ",",header_res2[7])
  header_res2 <-  header_res2[-4]
  header_res2 <- gsub("[\n]","-", header_res2)
  names(res2)[1:8] <- header_res2
  names(res2) <- gsub("\\\r","",names(res2))
  res2 <- res2[,c("No.","Chemical Name","CASRN","PACs based on AEGLs, ERPGs, or TEELs,PAC-1",
                  "pac1_keys", "PACs based on AEGLs, ERPGs, or TEELs,PAC-2","pac2_keys",
                  "PACs based on AEGLs, ERPGs, or TEELs,PAC-3","pac3_keys",
                  "Source of PACs-PAC-1, PAC-2, PAC-3","Units")]

  res3 <- openxlsx::read.xlsx(infile,3, rows = 4:3149, colNames = F )
  header_res3 <- openxlsx::read.xlsx(infile,3, rows = 1:3, colNames = F )
  header_res3 <- unlist(header_res3)
  header_res3 <- unname(header_res3[!is.na(header_res3)])
  header_res3[5] <-  paste0(header_res3[4], ",",header_res3[5])
  header_res3[6] <-  paste0(header_res3[4], ",",header_res3[6])
  header_res3[7] <-  paste0(header_res3[4], ",",header_res3[7])
  header_res3 <-  header_res3[-4]
  names(res3) <- header_res3

  res4 <- openxlsx::read.xlsx(infile,4, rows = 4:3149, colNames = F )
  header_res4 <- openxlsx::read.xlsx(infile,4, rows = 1:3, colNames = F )
  header_res4 <- unlist(header_res4)
  header_res4 <- unname(header_res4[!is.na(header_res4)])
  res4_unit<- gsub(".*\\(|\\)$","", header_res4[4])
  res4[,7] <- rep(res4_unit, nrow(res4))
  header_res4[4] <-  gsub("\\s\\(.*\\)", "", header_res4[4])
  header_res4[5] <-  paste0(header_res4[4], ",",header_res4[5])
  header_res4[6] <-  paste0(header_res4[4], ",",header_res4[6])
  header_res4[7] <-  paste0(header_res4[4], ",",header_res4[7])
  header_res4 <-  header_res4[-4]
  header_res4 <- c(header_res4, "Units")
  names(res4) <- header_res4
  rm(res4_unit)

  res <- Reduce(function(x,y) merge(x,y, all = T), list(res2,res3,res4))
  res$`PACs based on AEGLs, ERPGs, or TEELs,PAC-1` <- as.numeric(res$`PACs based on AEGLs, ERPGs, or TEELs,PAC-1`)
  res$`PACs based on AEGLs, ERPGs, or TEELs,PAC-2` <- as.numeric(res$`PACs based on AEGLs, ERPGs, or TEELs,PAC-2`)
  res$`PACs based on AEGLs, ERPGs, or TEELs,PAC-3` <- as.numeric(res$`PACs based on AEGLs, ERPGs, or TEELs,PAC-3`)

  res$`PACs based on AEGLs, ERPGs, or TEELs,PAC-1` <- signif(res$`PACs based on AEGLs, ERPGs, or TEELs,PAC-1`, digits = 3)
  res$`PACs based on AEGLs, ERPGs, or TEELs,PAC-2` <- signif(res$`PACs based on AEGLs, ERPGs, or TEELs,PAC-2`, digits = 3)
  res$`PACs based on AEGLs, ERPGs, or TEELs,PAC-3`<- signif(res$`PACs based on AEGLs, ERPGs, or TEELs,PAC-3`, digits = 3)

  new_res <- res %>% distinct(`Chemical Name`,CASRN, `PACs based on AEGLs, ERPGs, or TEELs,PAC-1`,
                              `PACs based on AEGLs, ERPGs, or TEELs,PAC-2`,`PACs based on AEGLs, ERPGs, or TEELs,PAC-3`,Units,
                              `Source of PACs-PAC-1, PAC-2, PAC-3`, .keep_all = T)

  no_source_res <- new_res[is.na(new_res$`Source of PACs-PAC-1, PAC-2, PAC-3`),]

  source_res <- new_res[!is.na(new_res$`Source of PACs-PAC-1, PAC-2, PAC-3`),]

  source_res1 <- source_res[,c(2:7)]
  no_source_res1 <- no_source_res[,c(2:7)]
  uniq_vals <- semi_join(source_res1,no_source_res1, by = c("Chemical Name", "CASRN", "PACs based on AEGLs, ERPGs, or TEELs,PAC-1",
                                                            "PACs based on AEGLs, ERPGs, or TEELs,PAC-2", "PACs based on AEGLs, ERPGs, or TEELs,PAC-3",
                                                            "Units"))
  new_no_source_res <- anti_join(no_source_res, uniq_vals, by = c("Chemical Name","CASRN", "PACs based on AEGLs, ERPGs, or TEELs,PAC-1",
                                                                  "PACs based on AEGLs, ERPGs, or TEELs,PAC-2","PACs based on AEGLs, ERPGs, or TEELs,PAC-3","Units"))
  new_res2 <- merge(new_no_source_res, source_res, all = T )

  new_res1 <- merge(res1, new_res2, by = c('Chemical Name','CASRN'))
  names(new_res1) <- c("name","casrn","rownames_res1", "new_molecular_weight_MW","original_molecular_weight_MW","Units_of_Original_Limits",
                       "ppm_to_mg/m3","Molecular_Formula_MF","State_at_25_degree_C","MP_or_FP_in_degree_C",
                       "BP_in_degree_C_at_760_mm_Hg_unless_indicated","Vapor_Pressure_mm_Hg", "Vapor_Pressure-T_in_degree_C",
                       "SG_at_25_degree_C_unless_indicated","LEL_ppm","PAC-TEEL_Originally_Derived",
                       "PAC-TEEL_Last_Reviewed", "PAC-TEEL_Last_Revised","rownames_new_res2",
                       "PAC_1","PAC_2","PAC_3", "toxval_units", "pac1_keys","pac2_keys",
                       "pac3_keys","Source_of_PACs-PAC-1_PAC-2_PAC-3")
  new_res1 <- new_res1[,c(-3,-19)]
  new_res1["doe_id"] <- c(1:dim(new_res1)[1])
  new_res1 <- new_res1[c("doe_id",names(new_res1[1:18]), names(new_res1[22]),names(new_res1[19]),names(new_res1[23]),
                         names(new_res1[20]),names(new_res1[24]), names(new_res1[21]), names(new_res1[25]))]

  names(new_res1) = c("doe_id","name",
                      "casrn","mw",
                      "original_mw","original_units",
                      "ppm_to_mg/m3","mol_formula",
                      "state_at_25C","mp",
                      "bp","vp",
                      "vp_temp","sg",
                      "lel_ppm","pac_teel_original",
                      "pac_teel_last_reviewed","pac_teel_last_revised",
                      "pac_1","pac1_keys",
                      "pac_2","pac2_keys",
                      "pac_3","pac3_keys",
                      "toxval_units","pac_source")
  mask = vector(length=nrow(new_res1),mode="integer")
  mask[] = 1
  for(i in 1:nrow(new_res1)) if(contains(new_res1[i,"casrn"],"z-")) mask[i] = 0
  new_res1 = new_res1[mask==1,]
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="DOE Protective Action Criteria",table="source_doe_pac",res=new_res1,F,T,T)
}
