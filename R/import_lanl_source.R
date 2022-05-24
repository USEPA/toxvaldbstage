library("openxlsx")
#--------------------------------------------------------------------------------------
#' Load lanl Source into dev_toxval_source_v2. 
#' @param toxval.db The version of toxval into which the source is loaded.
#' @param infile The input file ./lanl/lanl_files/ESLs_R3.3.xlsx


#--------------------------------------------------------------------------------------

import_lanl_source <- function(toxval.db,infile) {
  printCurrentFunction(toxval.db)

  #####################################################################
  cat("Build original_lanl_table \n")
  #####################################################################
  res <- read.xlsx( infile ,1,colNames = T)

  # print(names(res1))
  

  runInsertTable(res,"original_lanl_table",toxval.db,do.halt=T,verbose=F)

  # #####################################################################
  # cat("Build whole_lanl_table and create dataframe res1 \n")
  # #####################################################################
  # 
  # res1 <-  res
  # names(res1) <- gsub("\\.", "\\_", names(res1)) 
  # res1["lanl_id"] <- c(1:length(res1[,1]))
  # res1 <- res1[c('lanl_id', names(res1[-12]))]
  # res1$species_type <- gsub(".*\\(|\\).*|.*\\-", "", res1$ESL_Receptor)
  # res1$species <-  gsub("\\(.*\\)|\\-.*", "", res1$ESL_Receptor)
  # res1$species_type <-  gsub("^\\s+|\\s+$","", res1$species_type)
  # sps_type_media <- which(res1$species_type %in% tolower(res1$ESL_Medium))
  # res1$species_type[c(sps_type_media)] <- ""
  # res1 <- res1[c(names(res1[1:7]), names(res1[14]),names(res1[13]), names(res1[8:12]))]
  # colnames(res1) <- c('lanl_id','chemical_category','chemical_group',
  #                     'name','casrn','media','original_species','species','species_type','No_Effect_ESL',
  #                     'Lowest_Effect_ESL', 'units', 'Minimum_ESL','ESL_id')
  # 
  # res1$units <-  gsub("µ", "u", res1$units)
  # runInsertTable(res1,"whole_lanl_table",toxval.db,do.halt=T,verbose=F)
  # 
  # 
  
  #####################################################################
  cat("Build new_lanl_table from res1\n")
  #####################################################################
  res1 <-  res
  names(res1) <- gsub("\\.", "\\_", names(res1))
  
  colnames(res1) <- c('chemical_category','chemical_group','name','casrn','media',
                      'species','No_Effect_ESL','Lowest_Effect_ESL', 'toxval_units', 'Minimum_ESL','source_source_id')
  res1$species_type <- res1$species
  res1$species_type <- gsub(".*\\(|\\).*|.*\\-", "", res1$species)
  res1$species <-  gsub("\\(.*\\)|\\-.*", "", res1$species)
  res1$species_type <-  gsub("^\\s+|\\s+$","", res1$species_type)
  sps_type_media <- which(res1$species_type %in% tolower(res1$media))
  res1$species_type[c(sps_type_media)] <- ""
  res1$toxval_units <-  gsub("µ", "u", res1$toxval_units)
  t1 <- res1[,c(3,4,5,6,7,9,11)]
  colnames(t1)[5] <- c("toxval_numeric")
  colnames(t1)[6] <- c("toxval_units")
  t1["toxval_type"] <- c(rep(names(res1[7]), nrow(t1)))

  t2 <- res1[,c(3,4,5,6,8,9,11)]
  colnames(t2)[5] <- c("toxval_numeric")
  colnames(t2)[6] <- c("toxval_units")
  t2["toxval_type"] <- c(rep(names(res1[8]), nrow(t2)))

  lanl_types <- rbind(t1,t2)
  lanl_types <- subset(lanl_types,lanl_types[,"toxval_numeric"]!="")
  lanl_types$toxval_numeric <- as.numeric(lanl_types$toxval_numeric)
  #print(str(lanl_types))
  lanl_types["lanl_id"] <- c(1:dim(lanl_types)[1])
  lanl_types <- lanl_types[c("lanl_id",names(lanl_types[-9]))]
  runInsertTable(lanl_types,"new_lanl_table",toxval.db,do.halt=T,verbose=F)
  
  #####################################################################
  cat("Build lanl_chemical_information table from res1\n")
  #####################################################################
  chemical_information <- res1[,c("name","casrn")]
  chemical_information <- unique(chemical_information[,1:2])
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  chemical_information <- chemical_information[c('chemical_id','name','casrn')]
  runInsertTable(chemical_information,"lanl_chemical_information",toxval.db,do.halt=T,verbose=F)
  
  
  
}

