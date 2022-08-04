library(stringr)
#-------------------------------------------------------------------------------------
#' standardize critical_effect in toxval table based on icf dictionary
#' Fix occurances of multiple critical_effect in the toxval_critical_effect table based on values from critical_effect dictionary file
#'
#'
#' @param toxval.db The version of toxvaldb to use.
#' @export
#-------------------------------------------------------------------------------------
fix.critical_effect.icf <- function(toxval.db) {
  printCurrentFunction(toxval.db)
  
  #####################################################################
  cat("extract dictionary info \n")
  #####################################################################
  
  file <- paste0(toxval.config()$datapath,"dictionary/icf_critical_effect.xlsx")
  dict <- read.xlsx(file)
  print(dim(dict))
  print(names(dict))
  #####################################################################
  cat("extract critical effect info from toxval \n")
  #####################################################################
  
  query <- "select critical_effect_original,critical_effect,source from toxval"
  res <- runQuery(query,toxval.db)
  
  #####################################################################
  cat("find critical effect values from toxval that are missing from icf dict \n")
  #####################################################################
  
  x <- unique(res$critical_effect_original)
  x <- x[!is.element(x,dict[,2])]
  cat("   missing values in dictionary:",length(x),"\n")
  # file <- paste0(toxval.config()$datapath,"dictionary/critical_effect_icf_missing_",Sys.Date(),".xlsx")
  # write.xlsx(x, file)
  # 
  #####################################################################
  cat("find critical effect values from toxval that are in icf dict \n")
  #####################################################################
  y <- unique(res$critical_effect_original)
  y <- y[is.element(y,dict[,2])]
  cat("   values in dictionary:",length(y),"\n")
  
  print(dim(dict))
  dict_new1 <- dict[dict$critical_effect_original %in% y,]
  
  # file <- paste0(toxval.config()$datapath,"dictionary/critical_effect_icf_in_toxval_",Sys.Date(),".xlsx")
  # write.xlsx(y, file)
  # 
  #####################################################################
  cat("find missing icf dict values in existing critical effect dictionary \n")
  #####################################################################
  file <- paste0(toxval.config()$datapath,"dictionary/critical_effect_stage2_set_1234b2021-04-14.xlsx")
  dict_current <- read.xlsx(file)
  print(dim(dict_current))
  
  z <- unique(dict_current$critical_effect_original_0)
  z <- z[is.element(z,x)]
  cat("   missing icf values found in dictionary:",length(z),"\n")
  # file <- paste0(toxval.config()$datapath,"dictionary/critical_effect_icf_missing_values_in_dict_",Sys.Date(),".xlsx")
  # write.xlsx(z, file)
  
  dict_new2 <- dict_current[dict_current$critical_effect_original_0 %in% z,]
  # subset only the new value column and the original value column
  dict_new2 <- dict_new2[,c(3,1)]
  names(dict_new2) <- c("critical_effect","critical_effect_original")
  
  # combine both dictionary(icf and current dictionary) values found in toxval to create new dictionary
  
  dict_new <- rbind(dict_new1, dict_new2)
  print(dim(dict_new))
  dict_new <- unique(dict_new)
  # file <- paste0(toxval.config()$datapath,"dictionary/critical_effect_dict6_",Sys.Date(),".xlsx")
  # write.xlsx(dict_new, file)
  
  # find missing toxval critical effect values from the new dictionary
  query <- "select critical_effect_original,critical_effect,source from toxval"
  res <- runQuery(query,toxval.db)
  #print(View(res))
  x <- unique(res$critical_effect_original)
  x <- x[!is.element(x,dict_new[,2])]
  cat("   missing values in dictionary:",length(x),"\n")
  file <- paste0(toxval.config()$datapath,"dictionary/critical_effect_missing6_",Sys.Date(),".xlsx")
  write.xlsx(x, file)

  for(i in 1:nrow(dict_new)) {
    original <- dict_new[i,2]
    final <- dict_new[i,1]
    cat(original,":",final,"\n"); flush.console()
    query <- paste0("update toxval set critical_effect =\"",final,"\" where critical_effect_original=\"",original,"\" and source not like 'ECOTOX' and source not like 'ToxRefDB' and source not like 'HAWC' and source not like 'HAWC PFAS 150' and source not like 'PFAS Summary PODs' and source not like 'PFAS 430'")
    runInsert(query,toxval.db,T,F,T)
  }
  query <- paste0("update toxval set critical_effect ='-' where critical_effect_original is NULL")
  runInsert(query,toxval.db,T,F,T)
  
}
  