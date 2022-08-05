library('openxlsx')
library('dplyr')
#-------------------------------------------------------------------------------------
#' Add clowder_id's to source db tables
#' @param toxval.db The version of toxval in which the data is altered.
#' @param infile The input file ./clowder_id_mapping/source_doc_with_clowder_id.xlsx, new_qa_set_with_ClowderID_12-22.csv,clowderid_record_source_hash_hawc_pfas_150_430_with_source_hash.xlsx,
#' atsdr_toxval_record_source_w_document-Aswani.csv,efsa2_toxval_record_source_w_document-Aswani.csv, original_oppt_table_w_document-Aswani.csv,
#' subset_v8_chiu_record_source_hash-Aswani.csv,WHOIPCS_v8_v9_should_be_same_record_source_hash-Aswani.csv,missing_toxval_source_clowder_corrected.xlsx, toxval_new_qa_set_ClowderID_02-02-ASWANI.csv,
#' del_docs_matched_20220323.xlsx, toxval_qa_set_2_ECHA-IUCLID_ClowderID_03-23_Aswani.csv
#' @return The database will be altered
#' @export
#--------------------------------------------------------------------------------------
fix.clowder_id.by.toxval <- function(toxval.db,source.db, source_table, source, infile){
  
  printCurrentFunction(paste(toxval.db,":",source.db,":", source_table, ":",source))
  
  #####################################################################
  cat("extract dictionary info \n")
  #####################################################################

  clow1 <- read.csv(infile,stringsAsFactors = F)
  #print(View(clow1))
  


  clow1 <- clow1[,names(clow1)[grep("qa\\_",names(clow1), invert = T)]]
  #####################################################################
  cat("extract source info \n")
  #####################################################################

  mat = runQuery(paste0("select * from toxval where source like \"",source,"\" "), toxval.db)

  mat$clowder_id <- "-"
  
  #print(names(mat))

  ####################################################################
  cat("extract clowder file values for particular source \n")
  #####################################################################
  clow2 <- clow1[,names(clow1)[names(clow1) %in% names(mat)]]
  #print(View(clow2))
  clow3 <- clow2[which(clow2$source == paste0("",source,"")),]
  #print(View(clow3))
  
  clow3 <- unique(clow3)
  
  #####################################################################
  cat("find clowder_ids for each source hash \n")
  #####################################################################
  
  mat <- mat %>% left_join(clow3)
  #print(names(mat))
  #print(unique(mat$clowder_id))
  mat <- mat[which(!is.na(mat$clowder_id)),]
  #print(View(mat))
  
  #####################################################################
  cat("map the clowder ids to source tables using the source hash key \n")
  #####################################################################

  for(i in 1:nrow(mat)) {
    original <- mat[i,"source_hash"]
    final <- mat[i,"clowder_id"]
    cat(original,":",final,"\n"); flush.console()
    query <- paste0("update ",source_table," set clowder_id =\"",final,"\" where source_hash=\"",original,"\" ")
    runInsert(query,source.db,T,F,T)

  }

  #######################################################################
  #######################################################################
  # ############################################################################
  # cat("map the new clowder ids replacing the old from graces's corrected clowder_id file\n")
  # ############################################################################
  # 
  # clow1 <- read_xlsx(infile)
  
  # names(clow1)[names(clow1) == "clowder_id"] <- "old_clowder_id"
  # names(clow1)[names(clow1) == "corrected_id"] <- "clowder_id"
  
  # 
  # #clow1 <- clow1[which(clow1$source == "echa_echemportal_api_original"),]
  # clow1 <- clow1[which(clow1$source == "new_doe_table"),]
  # print(View(clow1))
  # 
  # for(i in 1:nrow(clow1)) {
  #   original <- clow1[i,"source_hash"]
  #   final <- clow1[i,"clowder_id"]
  #   cat(original,":",final,"\n"); flush.console()
  #   query <- paste0("update ",source_table," set clowder_id =\"",final,"\" where source_hash=\"",original,"\" ")
  #   runInsert(query,source.db,T,F,T)
  # 
  # }
  # ##############################################################################
  # ##############################################################################
  # # for cases where source_hash provided in mapping file
  # ##############################################################################
  # cat("Assign clowder_id where source_hash is provided \n")
  # ##############################################################################
  # 
  # clow1 <- read.csv(infile,stringsAsFactors = F)
  # #print(View(clow1))
  # #print(names(clow1))
  # 
  # clow1 <- clow1[which(clow1$source == paste0("",source,"")),]
  # 
  # 
  # for(i in 1:nrow(clow1)) {
  #   original <- clow1[i,"source_hash"]
  #   final <- clow1[i,"clowder_id"]
  #   cat(original,":",final,"\n"); flush.console()
  #   query <- paste0("update ",source_table," set clowder_id =\"",final,"\" where source_hash=\"",original,"\" ")
  #   runInsert(query,source.db,T,F,T)
  # 
  # }
  ##############################################################################
  # ############################################################################
  # cat("map the new clowder ids replacing the old from Taylor's(Jonathan Wall) corrected clowder_id file\n")
  # ############################################################################
  # 
  # clow1 <- readxl::read_xlsx(infile)
  # 
  # names(clow1)[names(clow1) == "del_file_id"] <- "old_clowder_id"
  # names(clow1)[names(clow1) == "new_file_id"] <- "clowder_id"
  # 
  # clow1 <- clow1[which(clow1$toxval_tbl == "original_hawc_pfas_150"),]
  # print(View(clow1))
  # for(i in 1:nrow(clow1)) {
  #   original <- clow1[i,"old_clowder_id"]
  #   final <- clow1[i,"clowder_id"]
  #   query <- paste0("update ",source_table," set clowder_id =\"",final,"\" where clowder_id=\"",original,"\" ")
  #   runInsert(query,source.db,T,F,T)
  #   
  # }
  
  
}  
