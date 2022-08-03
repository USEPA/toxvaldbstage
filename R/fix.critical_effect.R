library(stringr)
#-------------------------------------------------------------------------------------
#' Fix critical_effect in toxval table
#' Fix occurances of multiple critical_effect in the toxval_critical_effect table based on values from critical_effect dictionary file
#'
#'
#' @param toxval.db The version of toxvaldb to use.
#' @export
#-------------------------------------------------------------------------------------
fix.critical_effect <- function(toxval.db) {
  printCurrentFunction(toxval.db)
  
  
  #######runQuery("update toxval set critical_effect=critical_effect_original",toxval.db)
  
  
  #####################################################################
  cat("create critical_effect_original \n")
  #####################################################################
  #######start:commented out on 29th july 21###################
  # file <- paste0(toxval.config()$datapath,"dictionary/critical_effect_stage2_set_1234a.xlsx")
  # dict <- read.xlsx(file)
  # # make sure step 70 , quote change is applied to dictionary
  # dict <- unique(dict)
  # dict_flag_change <- grep('\\[\\.\\.\\.\\]',dict[,1])
  # print(length(dict_flag_change))
  # dict[dict_flag_change,1] <- str_replace_all(dict[dict_flag_change,1],'\\[\\.\\.\\.\\]','XXX')
  # print(grep("[\r\n]",dict[,1],value = T))
  # dict_carriage_returns <- grep("[\r\n]",dict[,1])
  # dict_carriage_returns_new <- grep("[\r\n]",dict[,3])
  # dict_carriage_returns_stage1 <- grep("[\r\n]",dict[,2])
  # print(length(dict_carriage_returns))
  # print(length(dict_carriage_returns_new))
  # print(length(dict_carriage_returns_stage1))
  # dict[dict_carriage_returns,1] <- sapply(dict[dict_carriage_returns,1], function(x) { gsub("[\r\n]", " ", x) })
  # dict[dict_carriage_returns,1] <- sapply(dict[dict_carriage_returns,1], function(x) { gsub("\\s+", " ", x) })
  # dict[dict_carriage_returns_new,3] <- sapply(dict[dict_carriage_returns_new,3], function(x) { gsub("[\r\n]", " ", x) })
  # dict[dict_carriage_returns_new,3] <- sapply(dict[dict_carriage_returns_new,3], function(x) { gsub("\\s+", " ", x) })
  # dict[dict_carriage_returns_stage1,2] <- sapply(dict[dict_carriage_returns_stage1,2], function(x) { gsub("[\r\n]", " ", x) })
  # dict[dict_carriage_returns_stage1,2] <- sapply(dict[dict_carriage_returns_stage1,2], function(x) { gsub("\\s+", " ", x) })
  # print(dim(dict))
  # dict <- unique(dict)
  # print(dim(dict))
  # file <- paste0(toxval.config()$datapath,"dictionary/critical_effect_stage2_set_1234b",Sys.Date(),".xlsx")
  # write.xlsx(dict, file)
  #######stop:commented out on 29th july 21###################
  
  file <- paste0(toxval.config()$datapath,"dictionary/critical_effect_stage2_set_1234b2021-04-14.xlsx")
  dict <- read.xlsx(file)
  
  query <- "select critical_effect_original,critical_effect,source from toxval"
  res <- runQuery(query,toxval.db)
  # print(View(res))
  # res$updated_critical_effect_original <- res$critical_effect_original
  
  res_carriage_returns <- grep("[\r\n]",res[,2])
  print(length(res_carriage_returns))
  res_dict <- data.frame(res[res_carriage_returns,2], res[res_carriage_returns,2], stringsAsFactors = F)
  names(res_dict) <- c("critical_effect","updated_critical_effect")
  res_dict[,2] <- sapply(res_dict[,2], function(x) { gsub("[\r\n]", " ", x) })
  res_dict[,2] <- sapply(res_dict[,2], function(x) { gsub("\\s+", " ", x) })
  file <- paste0(toxval.config()$datapath,"dictionary/critical_effect_updates",Sys.Date(),".xlsx")
  write.xlsx(res_dict, file)
  
  for(i in 1:nrow(res_dict)) {
    original <- res_dict[i,1]
    updated <- res_dict[i,2]
    query <- paste0("update toxval set critical_effect ='",updated,"'  where critical_effect='",original,"'")
    runInsert(query,toxval.db,T,F,T)
  }
  #print(View(res))

  # update all double quotes to single quotes in original and also in original dictionary, to rectify issue due to the presence of multiple quotes within data
  runQuery(paste0("update toxval SET critical_effect"," = ", "REPLACE", "( critical_effect",  ",\'\"\',", " \"'\" ) WHERE critical_effect"," LIKE \'%\"%\'" ),toxval.db)

  
  #######start:commented out on 29th july 21###################
  # res_carriage_returns <- grep("[\r\n]",res[,1])
  # print(length(res_carriage_returns))
  # res_dict <- data.frame(res[res_carriage_returns,1], res[res_carriage_returns,1], stringsAsFactors = F)
  # names(res_dict) <- c("critical_effect_original","updated_critical_effect_original")
  # res_dict[,2] <- sapply(res_dict[,2], function(x) { gsub("[\r\n]", " ", x) })
  # res_dict[,2] <- sapply(res_dict[,2], function(x) { gsub("\\s+", " ", x) })
  # file <- paste0(toxval.config()$datapath,"dictionary/critical_effect_original_updates",Sys.Date(),".xlsx")
  # write.xlsx(res_dict, file)
  # for(i in 1:nrow(res_dict)) {
  #   original <- res_dict[i,1]
  #   updated <- res_dict[i,2]
  #   query <- paste0("update toxval set critical_effect_original ='",updated,"'  where critical_effect_original='",original,"'")
  #   runInsert(query,toxval.db,T,F,T)
  # }
  # #print(View(res))
  # 
  # # update all double quotes to single quotes in original and also in original dictionary, to rectify issue due to the presence of multiple quotes within data
  # runQuery(paste0("update toxval SET critical_effect_original"," = ", "REPLACE", "( critical_effect_original",  ",\'\"\',", " \"'\" ) WHERE critical_effect_original"," LIKE \'%\"%\'" ),toxval.db)

  #######stop:commented out on 29th july 21###################
  
  
  # server connection lost to mysql in middle of run , ran the remaining dict values
  # file <- paste0(toxval.config()$datapath,"dictionary/critical_effect_stage2_set_1234b2021-04-14.xlsx")
  # dict <- read.xlsx(file)
  # dict <- dict[26856:36418,]
  # print(View(dict))

  query <- "select critical_effect_original,critical_effect,source from toxval"
  res <- runQuery(query,toxval.db)
  #print(View(res))
  x <- unique(res$critical_effect)
  x <- x[!is.element(x,dict[,1])]
  cat("   missing values in dictionary:",length(x),"\n")
  file <- paste0(toxval.config()$datapath,"dictionary/critical_effect_missing_",Sys.Date(),".xlsx")
  write.xlsx(x, file)

  # for(i in 1:nrow(dict)) {
  #   original <- dict[i,1]
  #   final <- dict[i,3]
  #   cat(original,":",final,"\n"); flush.console()
  #   query <- paste0("update toxval set critical_effect =\"",final,"\" where critical_effect=\"",original,"\" and source not like 'ECOTOX' and source not like 'ToxRefDB' and source not like 'HAWC' and source not like 'HAWC PFAS' and source not like 'PFAS Summary PODs' and source not like 'PFAS 430'")
  #   runInsert(query,toxval.db,T,F,T)
  # }
  # query <- paste0("update toxval set critical_effect ='-' where critical_effect is NULL")
  # runInsert(query,toxval.db,T,F,T)
  # 
  
  #######start:commented out on 29th july 21###################  
  # query <- "select critical_effect_original,critical_effect,source from toxval"
  # res <- runQuery(query,toxval.db)
  # #print(View(res))
  # x <- unique(res$critical_effect_original)
  # x <- x[!is.element(x,dict[,1])]
  # cat("   missing values in dictionary:",length(x),"\n")
  # #print(x)
  # a <- data.frame(x, stringsAsFactors = F)
  # a[,2] <- ""
  # names(a) <- c("critical_effect_original_0", "critical_effect")
  # #dict_new <- rbind(dict, a)
  # file <- paste0(toxval.config()$datapath,"dictionary/critical_effect_missing_",Sys.Date(),".xlsx")
  # write.xlsx(a, file)
  # 
  # for(i in 1:nrow(dict)) {
  #   original <- dict[i,1]
  #   final <- dict[i,3]
  #   cat(original,":",final,"\n"); flush.console()
  #   query <- paste0("update toxval set critical_effect =\"",final,"\" where critical_effect_original=\"",original,"\" and source not like 'ECOTOX' and source not like 'ToxRefDB' and source not like 'HAWC' and source not like 'HAWC PFAS' and source not like 'PFAS Summary PODs' and source not like 'PFAS 430'")
  #   runInsert(query,toxval.db,T,F,T)
  # }
  # query <- paste0("update toxval set critical_effect ='-' where critical_effect_original is NULL")
  # runInsert(query,toxval.db,T,F,T)
  #######stop:commented out on 29th july 21###################
}
  
  
  



