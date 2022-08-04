#--------------------------------------------------------------------------------------
#' Load cosmos Source files into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile1 The input file ./cosmos/cosmos_files/COSMOS_DB_v1_export_2016_04_02_study_data.xlsx
#' @param infile2 The input file ./cosmos/cosmos_files/COSMOS_DB_v1_export_2016_04_02_cosmetics_inventory.xlsx
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#--------------------------------------------------------------------------------------
import_cosmos_source <- function(db,
                                 infile1="COSMOS_DB_v1_export_2016_04_02_study_data.xlsx",
                                 infile2="COSMOS_DB_v1_export_2016_04_02_cosmetics_inventory.xlsx",
                                 chem.check.halt=F) {
  printCurrentFunction(db)
  infile1 = paste0(toxval.config()$datapath,"cosmos/cosmos_files/",infile1)
  infile2 = paste0(toxval.config()$datapath,"cosmos/cosmos_files/",infile2)
  indir = paste0(toxval.config()$datapath,"cosmos/cosmos_files/")
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
  #####################################################################
  cat("Build cosmos_study_information table \n")
  #####################################################################
  names(new_res) <- tolower(names(new_res))
  names(new_res) = c("study_no","study_id","cms_id","name","casrn" ,
                     "per_purity","per_active","test_substance_comments","study_type","duration",
                     "route_of_exposure","species","strain","dose_or_conc_levels","dose_comments" ,
                     "study_design_comments","study_results","study_result_comments","data_quality","document_source",
                     "document_number","study_title","study_reference","year")
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="COSMOS",table="source_cosmos",res=new_res,F,T,T)
}

