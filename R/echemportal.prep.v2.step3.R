#--------------------------------------------------------------------------------------
#' Process the ECHA eChemPortal data from 2020 - step 3
#'
#' @param do.load If TRUE, laod all of the in vivo data
#' @export
#--------------------------------------------------------------------------------------
echemportal.prep.v2.step3 <- function(do.load=F) {
  printCurrentFunction()
  if(do.load) {
    file <- "../echa/echa_files/eChemPortal mammalian data 2020 step 2.xlsx"
    MAT2 <<- read.xlsx(file)
  }
  mat <- MAT2
  generation.list <- unique(mat$generation)
  generation.list <- generation.list[!is.element(generation.list,"")]
  for(generation in generation.list) {
    generation0 <- substr(generation,2,nchar(generation))
    temp <- str_split(generation0,"\\|")[[1]][1]
    mat[is.element(mat$generation,generation),"generation"] <- temp
  }

  #-------------------------------------------------------------------------------
  cat("deal with toxval_type\n")
  mat$toxval_type_original <- mat$toxval_type
  file <- "../echa/echa_files/dict_toxval_type.xlsx"
  dict <- read.xlsx(file)

  x <- unique(mat$toxval_type)
  x <- x[!is.element(x,dict[,"toxval_type"])]
  cat("   missing values in dictionary:",length(x),"\n")
  if(length(x)>0) browser()

  for(i in 1:nrow(dict)) {
    valold <- dict[i,"toxval_type"]
    valnew <- dict[i,"toxval_type_new"]
    mat[is.element(mat$toxval_type,valold),"toxval_type"] <- valnew
  }

  mat$critical_effect <- "-"

  for(i in 1:nrow(mat)) {
    st0 <- mat[i,"toxval_type_original"]
    ce <- dict[is.element(dict[,"toxval_type"],st0),"critical_effect"]
    if(!is.na(ce)) mat[i,"critical_effect"] <- ce
    if(i%%10000==0) cat("finished:",i,"\n")
  }

  #-------------------------------------------------------------------------------
  cat("deal with exposure_route\n")
  mat$exposure_route_original <- mat$exposure_route
  file <- "../echa/echa_files/dict_exposure_route.xlsx"
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

  for(i in 1:nrow(mat)) {
    st0 <- mat[i,"exposure_route_original"]
    em <- dict[is.element(dict[,1],st0),"exposure_method"]
    if(!is.na(em)) {
      #cat(st0,em,"\n")
      em0 <- mat[i,"exposure_method"]
      if(em0=="")  mat[i,"exposure_method"] <- em
      else mat[i,"exposure_method"] <- paste(em0,",",em)
    }
    if(i%%10000==0) cat("finished:",i,"\n")
  }

  #-------------------------------------------------------------------------------
  cat("deal with study_type\n")
  mat$study_type_original <- mat$study_type
  file <- "../echa/echa_files/dict_study_type.xlsx"
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

    mat$risk_assessment_class <- mat$study_type

  #-------------------------------------------------------------------------------
  cat("deal with toxval_units\n")
  mat$toxval_units_original <- mat$toxval_units
  file <- "../echa/echa_files/dict_toxval_units.xlsx"
  dict <- read.xlsx(file)

  x <- unique(mat$toxval_units)
  x <- x[!is.element(x,dict[,1])]
  cat("   missing values in dictionary:",length(x),"\n")
  if(length(x)>0) browser()

  for(i in 1:nrow(dict)) {
    valold <- dict[i,1]
    valnew <- dict[i,2]
    mat[is.element(mat$toxval_units,valold),"toxval_units"] <- valnew
  }
  #-------------------------------------------------------------------------------
  cat("deal with study_duration_value and study_duration_units\n")
  mat$new_guideline <- gsub("^(,[^,]*,)(.*)", "\\2", mat$guideline)
  mat$new_guideline <- gsub("^\\s+","", mat$new_guideline)
  file <- "../echa/echa_files/ECHA goidline map.xlsx"
  dict <- read.xlsx(file)
  dict <- dict[dict[,"study_duration_value"]>0,]
  for(i in 1:nrow(dict)) {
    guideline <- dict[i,"guideline"]
    study_duration_units <- dict[i,"study_duration_units"]
    study_duration_value <- dict[i,"study_duration_value"]
    mat[is.element(mat[,"new_guideline"],guideline),"study_duration_value"] <- study_duration_value
    mat[is.element(mat[,"new_guideline"],guideline),"study_duration_units"] <- study_duration_units
    
  }
  
  file <- "../echa/echa_files/eChemPortal mammalian data 2020 step 3.xlsx"
  mat <- mat[,!(names(mat) == "new_guideline")]
  write.xlsx(mat,file)
}
