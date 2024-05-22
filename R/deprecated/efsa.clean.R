#--------------------------------------------------------------------------------------
#' prepare EFSA_combined.xlsx using dictionaries from efsa.dict.prep.R to be
#' loaded into dev_toxval_source_v4.
#' @param dir The directory where the input data sits, ../efsa2/efsa2_files/

#--------------------------------------------------------------------------------------

efsa.clean <- function(dir,verbose = F){
  printCurrentFunction()
  file <- paste0(dir,"merge2/EFSA_combined.xlsx")
  MAT2 <- read.xlsx(file)

  mat <- MAT2

  #-------------------------------------------------------------------------------------------------------
  cat("deal with study_type\n")
  mat$study_type_original <- mat$study_type
  file <- paste0(dir,"merge2/efsa2_study_type_dict.xlsx")
  dict <- read.xlsx(file)

  x <- unique(mat$study_type)
  x <- x[!is.element(x,dict[,1])]
  cat("   missing values in dictionary:",length(x),"\n")
  if(length(x)>0) browser()

  for(i in 1:nrow(dict)) {
    valold <- dict[i,1]
    valnew <- dict[i,2]
    mat[is.element(mat$study_type,valold),"study_type"] <- valnew
  }

  #--------------------------------------------------------------------------------------------------------
  cat("deal with study_duration_value and study_duration_units\n")
  mat$study_duration_value_original <- mat$study_duration_value
  mat$study_duration_units_original <- mat$study_duration_units
  file <- paste0(dir,"merge2/efsa2_study_duration_dict.xlsx")

  dict <- read.xlsx(file)

  x <- unique(mat$study_duration_value)
  x <- x[!is.element(x,dict[,1])]
  cat("   missing values in dictionary:",length(x),"\n")
  if(length(x)>0) browser()

  for(i in 1:nrow(dict)) {
    valold <- dict[i,1]
    valnew <- dict[i,2]
    mat[is.element(mat$study_duration_value,valold),"study_duration_value"] <- valnew
  }

  x <- unique(mat$study_duration_units)
  x <- x[!is.element(x,dict[,3])]
  cat("   missing values in dictionary:",length(x),"\n")
  if(length(x)>0) browser()

  for(i in 1:nrow(dict)) {
    valold <- dict[i,3]
    valnew <- dict[i,4]
    mat[is.element(mat$study_duration_units,valold),"study_duration_units"] <- valnew
  }

  #--------------------------------------------------------------------------------------------------------
  cat("deal with species and strain \n")
  mat$species <- mat$species_original
  mat$strain <- mat$species_original
  file <- paste0(dir,"merge2/efsa2_species_dict.xlsx")

  dict <- read.xlsx(file)

  x <- unique(mat$species)
  x <- x[!is.element(x,dict[,1])]
  cat("   missing values in dictionary:",length(x),"\n")
  if(length(x)>0) browser()

  for(i in 1:nrow(dict)) {
    valold <- dict[i,1]
    valnew <- dict[i,2]
    mat[is.element(mat$species,valold),"species"] <- valnew
  }

  x <- unique(mat$strain)
  x <- x[!is.element(x,dict[,1])]
  cat("   missing values in dictionary:",length(x),"\n")
  if(length(x)>0) browser()

  for(i in 1:nrow(dict)) {
    valold <- dict[i,1]
    valnew <- dict[i,3]
    mat[is.element(mat$strain,valold),"strain"] <- valnew
  }

  #--------------------------------------------------------------------------------------------------------
  cat("deal with exposure route and exposure method \n")
  mat$exposure_route_original <- mat$exposure_route
  mat$exposure_method <- mat$exposure_route
  file <- paste0(dir,"merge2/efsa2_exposure_dict.xlsx")

  dict <- read.xlsx(file)

  x <- unique(mat$exposure_route)
  x <- x[!is.element(x,dict[,1])]
  cat("   missing values in dictionary:",length(x),"\n")
  if(length(x)>0) browser()

  for(i in 1:nrow(dict)) {
    valold <- dict[i,1]
    valnew <- dict[i,2]
    mat[is.element(mat$exposure_route,valold),"exposure_route"] <- valnew
  }

  x <- unique(mat$exposure_method)
  x <- x[!is.element(x,dict[,1])]
  cat("   missing values in dictionary:",length(x),"\n")
  if(length(x)>0) browser()

  for(i in 1:nrow(dict)) {
    valold <- dict[i,1]
    valnew <- dict[i,3]
    mat[is.element(mat$exposure_method,valold),"exposure_method"] <- valnew
  }
  #-------------------------------------------------------------------------------------------------------
  cat("deal with sex\n")
  mat$sex_original <- mat$sex
  file <- paste0(dir,"merge2/efsa2_sps_sex_dict.xlsx")

  dict <- read.xlsx(file)

  x <- unique(mat$sex)
  x <- x[!is.element(x,dict[,1])]
  cat("   missing values in dictionary:",length(x),"\n")
  if(length(x)>0) browser()

  for(i in 1:nrow(dict)) {
    valold <- dict[i,1]
    valnew <- dict[i,2]
    mat[is.element(mat$sex,valold),"sex"] <- valnew
  }

  #-------------------------------------------------------------------------------------------------------
  cat("deal with toxval_numeric\n")
  mat$toxval_numeric_original <- mat$toxval_numeric
  file <- paste0(dir,"merge2/efsa2_tox_num_dict.xlsx")

  dict <- read.xlsx(file)

  x <- unique(mat$toxval_numeric)
  x <- x[!is.element(x,dict[,1])]
  cat("   missing values in dictionary:",length(x),"\n")
  #if(length(x)>0) browser()

  for(i in 1:nrow(dict)) {
    valold <- dict[i,1]
    valnew <- dict[i,2]
    mat[is.element(mat$toxval_numeric,valold),"toxval_numeric"] <- valnew
  }
  mat[which(is.na(mat$toxval_numeric)), "toxval_numeric"] <- "-1"
  mat$toxval_numeric <- as.numeric(mat$toxval_numeric)
  #--------------------------------------------------------------------------------------------------------
  cat("deal with toxval_units\n")
  mat$toxval_units_original <- mat$toxval_units
  file <- paste0(dir,"merge2/efsa2_tox_units_dict.xlsx")

  dict <- read.xlsx(file)
  x <- unique(mat$toxval_units)
  x <- x[!is.element(x,dict[,1])]
  cat("   missing values in dictionary:",length(x),"\n")
  if(length(x)>0) browser()

  for(i in 1:nrow(dict)) {
    valold <- dict[i,1]
    valnew <- dict[i,2]
    row_name <- dict[i,3]
    mat[row_name,][is.element(mat[row_name,"toxval_units"],valold),"toxval_units"] <- valnew
  }

  #--------------------------------------------------------------------------------------------------------
  cat("deal with toxval_numeric_qualifier\n")
  mat$toxval_numeric_qualifier_original <- mat$toxval_numeric_qualifier
  file <- paste0(dir,"merge2/efsa2_tox_num_qual_dict.xlsx")

  dict <- read.xlsx(file)
  x <- unique(mat$toxval_numeric_qualifier)
  x <- x[!is.element(x,dict[,1])]
  cat("   missing values in dictionary:",length(x),"\n")
  #if(length(x)>0) browser()

  for(i in 1:nrow(dict)) {
    valold <- dict[i,1]
    valnew <- dict[i,2]
    row_name <- dict[i,3]
    mat[row_name,][is.element(mat[row_name,"toxval_numeric_qualifier"],valold),"toxval_numeric_qualifier"] <- valnew
  }

  names.list <- c("casrn","name","study_type","study_duration_value","study_duration_units","species","strain","sex","exposure_route",
                  "exposure_method","toxval_type","toxval_numeric_qualifier","toxval_numeric","toxval_units","long_ref","url","aid",
                  "document_name")

  file <- paste0(dir,"merge2/EFSA_combined_new.xlsx")

  write.xlsx(mat[,names.list],file)
}
