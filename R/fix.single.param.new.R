#-------------------------------------------------------------------------------------
#' Alter the contents of toxval according to an excel dictionary
#' @param toxval.db The version of toxval in which the data is altered.
#' @param param THe parameter value to be fixed
#' @param ignore If TRUE allow missing values to be ignored
#' @return The database will be altered
#' @export
#-------------------------------------------------------------------------------------
fix.single.param.new <- function(toxval.db,param, ignore = FALSE) {
  printCurrentFunction(paste(toxval.db,":",param))
  
  
  file <- paste0(toxval.config()$datapath,"dictionary/2021_dictionaries/",param,"_5.xlsx")
  mat <- read.xlsx(file, na.strings = "NOTHING")
  print(View(mat))
  mat_flag_change <- grep('\\[\\.\\.\\.\\]',mat[,2])
  mat[mat_flag_change,2] <- str_replace_all(mat[mat_flag_change,2],'\\[\\.\\.\\.\\]','XXX')
  #print(View(mat))
  
  
  db.values <- runQuery(paste("select distinct (",param,") from toxval",sep=""),toxval.db)[,1]
  
  missing <- db.values[!is.element(db.values,mat[,2])]
  missing = missing[!is.na(missing)]
  
  if(length(missing)>0 & !ignore) {
    cat("values missing from the dictionary\n")
    for(i in 1:length(missing)) cat(missing[i],"\n",sep="")
    #browser()
  }
  missing_vals <- ""
  for(i in 1:length(missing)){
    missing_vals[i] <- data.frame(missing[i], stringsAsFactors = F)
  }
  
  missing_vals <- data.frame(unlist(missing_vals), stringsAsFactors = F)
  names(missing_vals) <- "Missing value"
  file <- paste0("./dictionary/missing_values_",param,"_",Sys.Date(),".xlsx")
  write.xlsx(missing_vals,file)
  
  
  
  # cat("  original list: ",nrow(mat),"\n")
  # query <- paste0("select distinct (",param,") from toxval")
  # param_values <- runQuery(query,toxval.db)[,1]
  # #print(View(param_values))
  # mat <- mat[is.element(mat[,2],param_values),]
  # #print(View(mat))
  # cat("  final list: ",nrow(mat),"\n")
  # 
  # for(i in 1:dim(mat)[1]) {
  #   v0 <- mat[i,2]
  #   v1 <- mat[i,1]
  #   cat(v0,":",v1,"\n"); flush.console()
  #   query <- paste0("update toxval set ",param,"='",v1,"' where ",param,"='",v0,"'")
  #   runInsert(query,toxval.db,T,F,T)
  # }
  # query <- paste0("update toxval set ",param,"='-' where ",param,"_original is NULL")
  # runInsert(query,toxval.db,T,F,T)
}
