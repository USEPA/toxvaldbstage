library("openxlsx")
#--------------------------------------------------------------------------------------
#' Load doe_benchmarks Source into dev_toxval_source_v2. 
#' @param toxval.db The version of toxval into which the source is loaded.
#' @param infile The input file ./doe_benchmarks/doe_benchmarks_files/DOE_Wildlife_Benchmarks_1996.xlsx


#--------------------------------------------------------------------------------------

import_doe_benchmarks_source <- function(toxval.db,infile) {
  printCurrentFunction(toxval.db)
  
  #####################################################################
  cat("Build original_doe_benchmarks_table \n")
  #####################################################################
  res <- read.xlsx(infile ,1,colNames = T)
  
  runInsertTable(res,"original_doe_benchmarks_table",toxval.db,do.halt=T,verbose=F)
  
  #####################################################################
  cat("Build whole_doe_benchmarks_table and create dataframe res1, with updated datatypes \n")
  #####################################################################
  
  res1 <- res
  names(res1) <- gsub("\\.",'\\_', names(res1))
  names(res1) <- tolower(names(res1))
  types <- grep(".*\\(.*\\)", names(res1), value = T)
  toxval_units <- gsub(".*\\(|\\)", "", types)
  toxval_types <- gsub("_\\(.*\\)","", types)
  
  res1$`test_species_noael_(mg/kg/d)`[which(is.na(res1$`test_species_noael_(mg/kg/d)`))] <- ""
  res1$`test_species_noael_(mg/kg/d)` <- gsub("day-old white", "", res1$`test_species_noael_(mg/kg/d)`)
  res1$`test_species_noael_(mg/kg/d)`<- as.numeric(res1$`test_species_noael_(mg/kg/d)`)
  
  res1$`noael_piscivore_(mg/l)`[which(is.na(res1$`noael_piscivore_(mg/l)`))] <- ""
  res1$`noael_piscivore_(mg/l)` <- as.numeric(res1$`noael_piscivore_(mg/l)`)
  
  res1$`loael_piscivore_(mg/l)`[ which(is.na(res1$`loael_piscivore_(mg/l)`))] <- ""
  res1$`loael_piscivore_(mg/l)` <- as.numeric(res1$`loael_piscivore_(mg/l)`)
  
  res1["doe_benchmarks_id"] <- c(1:dim(res1)[1])
  res1 <- res1[c("doe_benchmarks_id",names(res1[-21]))]
  
  runInsertTable(res1,"whole_doe_benchmarks_table",toxval.db,do.halt=T,verbose=F)
  
  
  #####################################################################
  cat("Build doe_benchmarks_chemical_information table from res1\n")
  #####################################################################
  
  chemical_information <- res1[,c("name","casrn")]
  chemical_information <- unique(chemical_information[,1:2])
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  chemical_information <- chemical_information[c('chemical_id','name','casrn')]
  runInsertTable(chemical_information,"doe_benchmarks_chemical_information",toxval.db,do.halt=T,verbose=F)
  
  #####################################################################
  cat("Build new_doe_benchmarks_table from res1\n")
  #####################################################################
  t1 <- res1[,c(2,11,4,5,6,10,13,7,3)]
  colnames(t1)[2] <- c("toxval_numeric")
  colnames(t1)[5] <- c("source")
  colnames(t1)[4] <- c("source_url")
  t1["toxval_type"] <- c(rep(toxval_types[1], nrow(t1)))
  t1["toxval_units"] <- c(rep(toxval_units[1], nrow(t1)))
  t1 <- t1[c(names(t1[1:2]),names(t1[11]), names(t1[10]), names(t1[3:9]))]
  
  t2 <- res1[,c(2,12,4,5,6,10,13,7,3)]
  colnames(t2)[2] <- c("toxval_numeric")
  colnames(t2)[5] <- c("source")
  colnames(t2)[4] <- c("source_url")
  t2["toxval_type"] <- c(rep(toxval_types[2], nrow(t2)))
  t2["toxval_units"] <- c(rep(toxval_units[2], nrow(t2)))
  t2 <- t2[c(names(t2[1:2]),names(t2[11]), names(t2[10]), names(t2[3:9]))]
  
  t3 <- res1[,c(2,14,4,5,6,10,13,7,3)]
  colnames(t3)[2] <- c("toxval_numeric")
  colnames(t3)[5] <- c("source")
  colnames(t3)[4] <- c("source_url")
  t3["toxval_type"] <- c(rep(toxval_types[3], nrow(t3)))
  t3["toxval_units"] <- c(rep(toxval_units[3], nrow(t3)))
  t3 <- t3[c(names(t3[1:2]),names(t3[11]), names(t3[10]), names(t3[3:9]))]
  
  t4 <- res1[,c(2,15,4,5,6,10,13,7,3)]
  colnames(t4)[2] <- c("toxval_numeric")
  colnames(t4)[5] <- c("source")
  colnames(t4)[4] <- c("source_url")
  t4["toxval_type"] <- c(rep(toxval_types[4], nrow(t4)))
  t4["toxval_units"] <- c(rep(toxval_units[4], nrow(t4)))
  t4 <- t4[c(names(t4[1:2]),names(t4[11]), names(t4[10]), names(t4[3:9]))]
  
  t5 <- res1[,c(2,16,4,5,6,10,13,7,3)]
  colnames(t5)[2] <- c("toxval_numeric")
  colnames(t5)[5] <- c("source")
  colnames(t5)[4] <- c("source_url")
  t5["toxval_type"] <- c(rep(toxval_types[5], nrow(t5)))
  t5["toxval_units"] <- c(rep(toxval_units[5], nrow(t5)))
  t5 <- t5[c(names(t5[1:2]),names(t5[11]), names(t5[10]), names(t5[3:9]))]
  
  
  t6 <- res1[,c(2,17,4,5,6,10,13,7,3)]
  colnames(t6)[2] <- c("toxval_numeric")
  colnames(t6)[5] <- c("source")
  colnames(t6)[4] <- c("source_url")
  t6["toxval_type"] <- c(rep(toxval_types[6], nrow(t6)))
  t6["toxval_units"] <- c(rep(toxval_units[6], nrow(t6)))
  t6 <- t6[c(names(t6[1:2]),names(t6[11]), names(t6[10]), names(t6[3:9]))]
  
  
  t7 <- res1[,c(2,18,4,5,6,10,13,7,3)]
  colnames(t7)[2] <- c("toxval_numeric")
  colnames(t7)[5] <- c("source")
  colnames(t7)[4] <- c("source_url")
  t7["toxval_type"] <- c(rep(toxval_types[7], nrow(t7)))
  t7["toxval_units"] <- c(rep(toxval_units[7], nrow(t7)))
  t7 <- t7[c(names(t7[1:2]),names(t7[11]), names(t7[10]), names(t7[3:9]))]
  
  
  t8 <- res1[,c(2,19,4,5,6,10,13,7,3)]
  colnames(t8)[2] <- c("toxval_numeric")
  colnames(t8)[5] <- c("source")
  colnames(t8)[4] <- c("source_url")
  t8["toxval_type"] <- c(rep(toxval_types[8], nrow(t8)))
  t8["toxval_units"] <- c(rep(toxval_units[8], nrow(t8)))
  t8 <- t8[c(names(t8[1:2]),names(t8[11]), names(t8[10]), names(t8[3:9]))]
  
  
  t9 <- res1[,c(2,20,4,5,6,10,13,7,3)]
  colnames(t9)[2] <- c("toxval_numeric")
  colnames(t9)[5] <- c("source")
  colnames(t9)[4] <- c("source_url")
  t9["toxval_type"] <- c(rep(toxval_types[9], nrow(t9)))
  t9["toxval_units"] <- c(rep(toxval_units[9], nrow(t9)))
  t9 <- t9[c(names(t9[1:2]),names(t9[11]), names(t9[10]), names(t9[3:9]))]
  
  t10 <- res1[,c(2,21,4,5,6,10,13,7,3)]
  colnames(t10)[2] <- c("toxval_numeric")
  colnames(t10)[5] <- c("source")
  colnames(t10)[4] <- c("source_url")
  t10["toxval_type"] <- c(rep(toxval_types[10], nrow(t10)))
  t10["toxval_units"] <- c(rep(toxval_units[10], nrow(t10)))
  t10 <- t10[c(names(t10[1:2]),names(t10[11]), names(t10[10]), names(t10[3:9]))]
  
  doe_benchmarks_types <- rbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10)
  doe_benchmarks_types <- subset(doe_benchmarks_types,doe_benchmarks_types[,2]!="")
  doe_benchmarks_types["doe_benchmarks_id"] <- c(1:dim(doe_benchmarks_types)[1])
  doe_benchmarks_types <- doe_benchmarks_types[c("doe_benchmarks_id",names(doe_benchmarks_types[-12]))]
  doe_benchmarks_types$species[doe_benchmarks_types$toxval_type == 'test_species_noael'] <- doe_benchmarks_types$test_species[doe_benchmarks_types$toxval_type == 'test_species_noael']
  doe_benchmarks_types$species[doe_benchmarks_types$toxval_type == 'test_species_loael'] <- doe_benchmarks_types$test_species[doe_benchmarks_types$toxval_type == 'test_species_loael']
  doe_benchmarks_types$species[doe_benchmarks_types$toxval_type %in% unique(doe_benchmarks_types$toxval_type)[c(-1,-2)]] <- doe_benchmarks_types$endpoint_species[doe_benchmarks_types$toxval_type %in% unique(doe_benchmarks_types$toxval_type)[c(-1,-2)]]
  
  runInsertTable(doe_benchmarks_types,"new_doe_benchmarks_table",toxval.db,do.halt=T,verbose=F)
  
  
}
