#--------------------------------------------------------------------------------------
#' Load opp Source into dev_toxval_source_v2.
#' @param db The version of toxval into which the source is loaded.
#' @param infile The input file ./opp/opp_files/OPP RfD.xlsx
#--------------------------------------------------------------------------------------
import_opp_source <- function(db,
                              infile="../opp/opp_files/OPP RfD.xlsx",
                              chem.check.halt=T) {
  printCurrentFunction(db)

  #####################################################################
  cat("Build original_opp_table \n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile ,1,colNames = T)
  res_header <- names(res)
  print(names(res))
  #####################################################################
  cat("Do the chemical checking\n")
  #####################################################################
  source = "EPA OPP"
  res = as.data.frame(res)
  res$clowder_id = "-"
  res = fix.non_ascii.v2(res,source)
  res = source_chemical.process(db,res,source,chem.check.halt,casrn.col="casrn",name.col="name",verbose=F)
  #####################################################################
  cat("Build the hash key and load the data \n")
  #####################################################################
  res = subset(res,select=-c(chemical_index))
  toxval_source.hash.and.load(db,source,"original_opp_table",F,F,res)
  browser()
  return(1)
  runInsertTable(res,"original_opp_table",db,do.halt=T,verbose=F)

  #####################################################################
  cat("Split carcinogenic_HHBP_ppb range values to higher and lower values \n")
  #####################################################################

  res1 <- res
  # names replace period with underscore
  names(res1) <- gsub("\\.",'\\_', names(res1))
  #print(names(res1))
  hhbp_ppb_idx <- which(!is.na(res1$carcinogenic_HHBP_ppb))
  hhbp_ppb_val <- res1$carcinogenic_HHBP_ppb[1:nrow(res1) %in% hhbp_ppb_idx]
  lower_carcinogenic_HHBP_ppb_val <- gsub("-[^-]+$", "", hhbp_ppb_val)
  res1$lower_carcinogenic_HHBP_ppb[1:nrow(res1) %in% hhbp_ppb_idx] <- lower_carcinogenic_HHBP_ppb_val
  higher_carcinogenic_HHBP_ppb_val <- gsub(".*\\-", "", hhbp_ppb_val)
  res1$higher_carcinogenic_HHBP_ppb[1:nrow(res1) %in% hhbp_ppb_idx] <- higher_carcinogenic_HHBP_ppb_val
  #print(unique(res1$higher_carcinogenic_HHBP_ppb))
  res1$lower_carcinogenic_HHBP_ppb <-  as.numeric(res1$lower_carcinogenic_HHBP_ppb)
  res1$higher_carcinogenic_HHBP_ppb <- as.numeric(res1$higher_carcinogenic_HHBP_ppb)
  #print(names(res1))
  # rename carcinogenic_HHBP_ppb
  colnames(res1)[10] <- "original_carcinogenic_HHBP_ppb"
  nums <- unlist(lapply(res1, is.numeric))
  type_names <- names(res1[,nums])
  #print(type_names)
  toxval_type_val <- gsub("_[^_]+$", "", type_names)
  #print(toxval_type_val)
  study_duration_class <-  gsub("_RfD|_HHBP", "", toxval_type_val)
  study_duration_class <-replace(study_duration_class, study_duration_class=="cancer_slope_factor", "")
  toxval_type <- gsub("^\\w+_","" ,toxval_type_val)
  toxval_type <- replace(toxval_type, toxval_type=="factor", "cancer_slope_factor")
  toxval_units <- gsub("^[^_]+_[^_]+_|^[^_]+_[^_]+_[^_]+_", "", type_names)
  # res1["opp_id"] <- c(1:dim(res1)[1])
  # res1 <- res1[c("opp_id",names(res1[-13]))]
  #runInsertTable(res1,"whole_opp_table",db,do.halt=T,verbose=F)

  #####################################################################
  cat("Build opp_chemical_information table from res1\n")
  #####################################################################

  chemical_information <- res1[,c("casrn","name")]
  chemical_information <- unique(chemical_information[,1:2])
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  chemical_information <- chemical_information[c('chemical_id','name','casrn')]

  runInsertTable(chemical_information,"opp_chemical_information",db,do.halt=T,verbose=F)


  #####################################################################
  cat("Build new_opp_table from res1\n")
  #####################################################################
  t1 <- res1[,c(1,2,3,5,8)]
  colnames(t1)[3] <- c("toxval_numeric")
  t1["toxval_type"] <- c(rep(toxval_type[1], nrow(t1)))
  t1["toxval_units"] <- c(rep(toxval_units[1], nrow(t1)))
  t1["study_duration_class"] <- c(rep(study_duration_class[1], nrow(t1)))

  t2 <- res1[,c(1,2,4,5,8)]
  colnames(t2)[3] <- c("toxval_numeric")
  t2["toxval_type"] <- c(rep(toxval_type[2], nrow(t2)))
  t2["toxval_units"] <- c(rep(toxval_units[2], nrow(t2)))
  t2["study_duration_class"] <- c(rep(study_duration_class[2], nrow(t2)))

  t3 <- res1[,c(1,2,6,5,8)]
  colnames(t3)[3] <- c("toxval_numeric")
  t3["toxval_type"] <- c(rep(toxval_type[3], nrow(t3)))
  t3["toxval_units"] <- c(rep(toxval_units[3], nrow(t3)))
  t3["study_duration_class"] <- c(rep(study_duration_class[3], nrow(t3)))

  t4 <- res1[,c(1,2,7,5,8)]
  colnames(t4)[3] <- c("toxval_numeric")
  t4["toxval_type"] <- c(rep(toxval_type[4], nrow(t4)))
  t4["toxval_units"] <- c(rep(toxval_units[4], nrow(t4)))
  t4["study_duration_class"] <- c(rep(study_duration_class[4], nrow(t4)))

  t5 <- res1[,c(1,2,9,5,8)]
  colnames(t5)[3] <- c("toxval_numeric")
  t5["toxval_type"] <- c(rep(toxval_type[5], nrow(t5)))
  t5["toxval_units"] <- c(rep(toxval_units[5], nrow(t5)))
  t5["study_duration_class"] <- c(rep(study_duration_class[5], nrow(t5)))

  t6 <- res1[,c(1,2,11,5,8)]
  colnames(t6)[3] <- c("toxval_numeric")
  t6["toxval_type"] <- c(rep(toxval_type[6], nrow(t6)))
  t6["toxval_units"] <- c(rep(toxval_units[6], nrow(t6)))
  t6["study_duration_class"] <- c(rep(study_duration_class[6], nrow(t6)))

  t7 <- res1[,c(1,2,12,5,8)]
  colnames(t7)[3] <- c("toxval_numeric")
  t7["toxval_type"] <- c(rep(toxval_type[7], nrow(t7)))
  t7["toxval_units"] <- c(rep(toxval_units[7], nrow(t7)))
  t7["study_duration_class"] <- c(rep(study_duration_class[7], nrow(t7)))


  opp_types <- rbind(t1,t2,t3,t4,t5,t6,t7)
  opp_types <- subset(opp_types,opp_types[,"toxval_numeric"]!="")
  opp_types$toxval_numeric <- as.numeric(opp_types$toxval_numeric)

  print(unique(opp_types$study_duration_class))

  opp_types$population[opp_types$toxval_type=="HHBP" & opp_types$study_duration_class=="acute"]<- opp_types$acute_HHBP_sensitive_lifestage[opp_types$toxval_type=="HHBP" & opp_types$study_duration_class=="acute"]
  opp_types$population[opp_types$toxval_type=="HHBP" & opp_types$study_duration_class=="chronic"]<- opp_types$chronic_HHBP_sensitive_lifestage[opp_types$toxval_type=="HHBP" & opp_types$study_duration_class=="chronic"]
  opp_types$species[opp_types$population != ""] <- "human"
  opp_types$toxval_numeric_qualifier <- "="
  opp_types$toxval_numeric_qualifier[opp_types$study_duration_class == "lower_carcinogenic"] <- ">="
  opp_types$toxval_numeric_qualifier[opp_types$study_duration_class == "higher_carcinogenic"] <- "<="
  opp_types$study_duration_class[opp_types$study_duration_class == "lower_carcinogenic"] <- ""
  opp_types$study_duration_class[opp_types$study_duration_class == "higher_carcinogenic"] <- ""
  opp_types$phenotype[opp_types$toxval_type == "HHBP" & opp_types$toxval_numeric_qualifier == ">="] <- "cancer"
  opp_types$phenotype[opp_types$toxval_type == "HHBP" & opp_types$toxval_numeric_qualifier == "<="] <- "cancer"
  opp_types$phenotype[opp_types$toxval_type == "cancer_slope_factor"] <- "cancer"

  #print(str(opp_types))

  opp_types <- opp_types[,c(-4,-5)]
  opp_types["opp_id"] <- c(1:dim(opp_types)[1])
  opp_types <- opp_types[c("opp_id",names(opp_types[-11]))]

  runInsertTable(opp_types,"new_opp_table",db,do.halt=T,verbose=F)

}

