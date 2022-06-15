library("openxlsx")
library("tidyr")
library("stringr")
library('dplyr')
#--------------------------------------------------------------------------------------
#' Load cosmos Source files into dev_toxval_source_v4.
#' @param db The version of toxval into which the source is loaded.
#' @param infile1 The input file ./cosmos/cosmos_files/COSMOS_DB_v1_export_2016_04_02_study_data.xlsx
#' @param infile2 The input file ./cosmos/cosmos_files/COSMOS_DB_v1_export_2016_04_02_cosmetics_inventory.xlsx

#--------------------------------------------------------------------------------------
import_cosmos_source <- function(db,
                                 infile1="../cosmos/cosmos_files/COSMOS_DB_v1_export_2016_04_02_study_data.xlsx",
                                 infile2="../cosmos/cosmos_files/COSMOS_DB_v1_export_2016_04_02_cosmetics_inventory.xlsx",
                                 indir="../cosmos/cosmos_files/",
                                 chem.check.halt=F) {
  printCurrentFunction(db)

  #####################################################################
  cat("Build cosmos source tables \n")
  #####################################################################

  # data = list()
  # for(i in 1:13) {
  #   temp = openxlsx::read.xlsx(infile1,sheet=i)
  #   browser()
  # }
  cosmos_files <- openxlsx::loadWorkbook(infile1)
  sheetNames <- sheets(cosmos_files)
  for(i in 1:length(sheetNames)) {
    assign(sheetNames[i], openxlsx::readWorkbook(cosmos_files,sheet = i))
  }

  res <- Filter(function(x) is(x, "data.frame"), mget(ls()))
  names(res) <- tolower(names(res))
  names(res) <- paste0("cosmos_", names(res))
  names(res) <- gsub("\\s+","\\_", names(res))
  names(res) <- gsub("_-_","_",names(res))
  res <- lapply(res, function(x) setNames(x, gsub("\\.+","\\_", names(x))))
  res <- lapply(res, function(x) setNames(x, gsub("\\#","NO", names(x))))
  res <- lapply(res, function(x) setNames(x, gsub("\\-","\\_", names(x))))
  res <- lapply(res, function(x) setNames(x, gsub("%","PER", names(x))))
  res <- lapply(res, function(x) setNames(x, gsub("\\(|\\)","", names(x))))

  ### create id's for each dataframe
  res[[1]]["clinical_chemistry_id"] <- c(1:length(res[[1]][,1]))
  res[[1]] <- res[[1]][c("clinical_chemistry_id",names(res[[1]][-25]))]

  res[[2]]["hematology_id"] <- c(1:length(res[[2]][,1]))
  res[[2]] <- res[[2]][c("hematology_id",names(res[[2]][-25]))]

  res[[3]]["neuro_fob_id"] <- c(1:length(res[[3]][,1]))
  res[[3]] <- res[[3]][c("neuro_fob_id",names(res[[3]][-25]))]

  res[[4]]["organ_weight_id"] <- c(1:length(res[[4]][,1]))
  res[[4]] <- res[[4]][c("organ_weight_id",names(res[[4]][-25]))]

  res[[5]]["pathology_macro_id"] <- c(1:length(res[[5]][,1]))
  res[[5]] <- res[[5]][c("pathology_macro_id",names(res[[5]][-25]))]

  res[[6]]["pathology_micro_id"] <- c(1:length(res[[6]][,1]))
  res[[6]] <- res[[6]][c("pathology_micro_id",names(res[[6]][-25]))]

  res[[7]]["repro_dev_adults_id"] <- c(1:length(res[[7]][,1]))
  res[[7]] <- res[[7]][c("repro_dev_adults_id",names(res[[7]][-26]))]

  res[[8]]["repro_dev_offspring_id"] <- c(1:length(res[[8]][,1]))
  res[[8]] <- res[[8]][c("repro_dev_offspring_id",names(res[[8]][-25]))]

  res[[9]]["repro_offspring_id"] <- c(1:length(res[[9]][,1]))
  res[[9]] <- res[[9]][c("repro_offspring_id",names(res[[9]][-25]))]

  res[[11]]["systemic_id"] <- c(1:length(res[[11]][,1]))
  res[[11]] <- res[[11]][c("systemic_id",names(res[[11]][-25]))]

  res[[12]]["tissue_chemistry_id"] <- c(1:length(res[[12]][,1]))
  res[[12]] <- res[[12]][c("tissue_chemistry_id",names(res[[12]][-25]))]

  res[[13]]["urinalysis_id"] <- c(1:length(res[[13]][,1]))
  res[[13]] <- res[[13]][c("urinalysis_id",names(res[[13]][-25]))]

  names(res[[7]])[25] <- "COMMENTS1"
  names(res[[7]])[26] <- "COMMENTS2"

  new_res <- res[[10]]
  res <- res[-10]

  res <- lapply(res, function(x) {colnames(x) <- tolower(colnames(x));x})
  res <- lapply(res, function(x) setNames(x, gsub("test_substance_name", "name", names(x))))
  res <- lapply(res, function(x) setNames(x, gsub("registry_number", "casrn", names(x))))
  res <- lapply(res, function(x) setNames(x, gsub("year_report/citation", "year", names(x))))

  table_names <- tolower(c("cosmos_clinical_chemistry","cosmos_hematology",
                           "cosmos_neuro_fob","cosmos_organ_weight","cosmos_pathology_macro",
                           "cosmos_pathology_micro","cosmos_repro_dev_adults","cosmos_repro_dev_offspring","cosmos_repro_offspring",
                           "cosmos_systemic", "cosmos_tissue_chemistry","cosmos_urinalysis"))

  # stop = FALSE
  # for( i in 1:length(res)){
  #   for (j in 1:length(table_names)){
  #
  #     runInsertTable(res[[i]],table_names[j],db,do.halt=T,verbose=F)
  #     i <- i+1
  #     if (i == length(res)+1){
  #       stop = TRUE
  #       break
  #     }
  #   }
  #   if (stop){break}
  # }

  #####################################################################
  cat("Build cosmos_study_information table \n")
  #####################################################################
  names(new_res) <- tolower(names(new_res))

  #####################################################################
  cat("Do the chemical checking\n")
  #####################################################################
  source = "COSMOS"
  res = as.data.frame(new_res)
  res$clowder_id = "-"
  res = fix.non_ascii.v2(res,source)
  res = source_chemical.process(db,res,source,chem.check.halt,casrn.col="registry_number",name.col="test_substance_name")
  #####################################################################
  cat("Build the hash key and load the data \n")
  #####################################################################
  res = subset(res,select=-c(chemical_index))
  toxval_source.hash.and.load(db,source,"original_cosmos",F,F,res)
  browser()
  return(1)

  ### seperate study information, route of exposure into exposure method and exposure route
  new_res$exposure_route <- gsub("\\-.*","", new_res$route_of_exposure)
  exp_method <- grep("-", new_res$route_of_exposure, value = T)
  new_exp_method <- gsub(".*\\-","", exp_method)
  new_res$exposure_method <- "-"
  new_res$exposure_method[new_res$route_of_exposure %in% exp_method] <- new_exp_method

  ### duplicated study no in study information table converted to 6618001
  which(new_res$study_no == 6618)
  new_res$study_no[which(duplicated(new_res$study_no))] <- as.integer(paste(new_res$study_no[which(duplicated(new_res$study_no))], "001", sep = ""))
  which(new_res$study_no == 6618)

  ### seperate duration to study_duration_value and study_duartion_units
  new_res[is.na(new_res)] <- "-"
  new_res$duration <- gsub("Gestation day","GD",new_res$duration, ignore.case = TRUE)
  new_res$duration <- gsub(" NA","-1 -",new_res$duration, ignore.case = TRUE)
  new_res <- separate(new_res,duration,c("study_duration_value", "study_duration_units")," ")
  new_res[grep("[0-9]-",new_res$study_duration_value),10] <- as.numeric(sapply(strsplit(new_res[grep("[0-9]-",new_res$study_duration_value),10],"-"), '[[',2)) - as.numeric(sapply(strsplit(new_res[grep("[0-9]-",new_res$study_duration_value),10],"-"), '[[',1))
  new_res$study_duration_value <- as.numeric(new_res$study_duration_value)

  ### create toxval_numeric, toxval_type and toxval_units from study_results
  new_res$study_results <- gsub("=", ": ",new_res$study_results)
  new_res$study_results <- gsub("L;", "L:",new_res$study_results)
  new_res <-mutate(new_res, study_results = strsplit(study_results,";")) %>% unnest(study_results)
  new_res <- new_res[!is.na(new_res$study_results),]
  new_res <- new_res[new_res$study_results != "",]
  new_res <- new_res[grep("established",new_res$study_results, ignore.case = TRUE, invert = TRUE),]
  new_res <- new_res[grep("CFSAN",new_res$study_results, ignore.case = TRUE, invert = TRUE),]
  new_res$toxval_type <- str_trim(substr(new_res$study_results,1,regexpr(":",new_res$study_results)-1))
  new_res$toxval_numeric <- str_trim(substr(new_res$study_results,regexpr(":",new_res$study_results)+1,regexpr("[0-9] ",new_res$study_results)))
  new_res$toxval_numeric <- as.numeric(new_res$toxval_numeric)
  new_res$study_results <- gsub("m2","m-squared",new_res$study_results)
  new_res$toxval_units <- str_trim(gsub(".*[0-9]","",new_res$study_results))
  new_res[new_res$toxval_units=="", which(colnames(new_res)=="toxval_type")] <- "-"
  new_res[new_res$toxval_units=="", which(colnames(new_res)=="toxval_units")] <- "-"
  new_res <- unique(new_res)

  ### fix name
  new_res[grep("Preferred",new_res$test_substance_name, invert = TRUE),"test_substance_name"] = paste0(grep("Preferred",new_res$test_substance_name, invert = TRUE, value = TRUE)," (Preferred Term)")
  new_res$test_substance_name = unlist(sapply(strsplit(new_res$test_substance_name, "; "), grep, pattern="Preferred", value = TRUE))
  new_res$test_substance_name = gsub(" (Preferred Term)","",new_res$test_substance_name, fixed = TRUE)
  new_res$test_substance_name = gsub(" (INCI)","",new_res$test_substance_name, fixed = TRUE)

  ### fix the column names of study information
  names(new_res) <- tolower(names(new_res))
  names(new_res) <- gsub("test_substance_name", "name", names(new_res))
  names(new_res) <- gsub("registry_number", "casrn", names(new_res))
  names(new_res) <- gsub("year_report/citation", "year", names(new_res))
  new_res$year <-  gsub("-","", new_res$year)
  new_res$year <-  as.integer(new_res$year)

  new_res["study_information_id"] <- c(1:nrow(new_res))

  new_res <- new_res[c("study_information_id",names(new_res[-31]))]

  ### direct processing of new_res within runInsertTable was hanging the whole run process for the script

  file <- "./cosmos/cosmos_files/cosmos_study_info.xlsx"
  write.xlsx(new_res,file)
  new_res2 <- read.xlsx("./cosmos/cosmos_files/cosmos_study_info.xlsx",1)
  runInsertTable(new_res2,"cosmos_study_information",db,do.halt=T,verbose=F)

  #####################################################################
  cat("Build cosmos_chemical_information table \n")
  #####################################################################
  chemical_information <- new_res2[,c("name","casrn")]
  chemical_information <- unique(chemical_information[,1:2])
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  colnames(chemical_information) <- c('name','casrn','chemical_id')
  chemical_information <- chemical_information[c('chemical_id','name','casrn')]
  runInsertTable(chemical_information,"cosmos_chemical_information",db,do.halt=T,verbose=F)


  #####################################################################
  cat("Build cosmos_cosmetics_inventory table\n")
  #####################################################################

  res2 <- read.xlsx(infile2,1)
  names(res2) <- gsub("\\.+","\\_", names(res2))
  names(res2) <- gsub("\\-","\\_", names(res2))
  res2["cosmetics_inventory_id"] <- c(1:length(res2[,1]))
  res2 <- res2[c("cosmetics_inventory_id",names(res2[-8]))]
  names(res2) <-  tolower(names(res2))
  names(res2) <-  gsub("registry_number", "casrn", names(res2))
  runInsertTable(res2,"cosmos_cosmetics_inventory",db,do.halt=T,verbose=F)

}

