#--------------------------------------------------------------------------------------
#' Load TEST Source data into toxval_source
#' @param db The version of toxval_source into which the source info is loaded.
#' @param infile1 The input file ./test/test_files/TEST data.xlsx
#' @param infile2 The input file ./test/test_files/test_chemicals_invitrodb.csv to map casrn to names from prod_internal_invitrodb_v3_2.chemical
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#--------------------------------------------------------------------------------------
import_test_source <- function(db,
                               infile1="TEST data.xlsx",
                               infile2="test_chemicals_invitrodb.csv",
                               chem.check.halt=T) {
  printCurrentFunction(db)

  infile1 = paste0(toxval.config()$datapath,"test/test_files/",infile1)
  infile2 = paste0(toxval.config()$datapath,"test/test_files/",infile2)

  #####################################################################
  cat("Build original_test_table and new_test_table \n")
  #####################################################################
  res1 <- openxlsx::read.xlsx(infile1, 1, startRow = 1)
  #runInsertTable(res1,"original_test_table",db,do.halt=T,verbose=F)
  res1["test_id"] <- c(1:length(res1[,1]))
  res1 <- res1[c("test_id",names(res1[-7]))]
  res1["toxval_type"] <- c("LD50")
  res1["name"] <- "-"

  chem_name <- read.csv(infile2, header = T, sep = ',', stringsAsFactors = F)
  #runInsertTable(chem_name,"test_map_invitrodb_chemicals",db,do.halt=T,verbose=F)

  map_casrn <- which(res1$CAS %in% chem_name$casn)
  name_val <- as.character(chem_name$chnm[res1$CAS %in% chem_name$casn])
  res1$name[1:nrow(res1) %in% map_casrn] <- name_val

  res1$toxval_numeric <- gsub("[^0-9.]","", res1$LD50)
  res1$toxval_numeric_qualifier <- gsub("[a-zA-Z ]|\\d([\\.]\\d)?","", res1$LD50)

  res1 <- res1[-9916,]
  res1$toxval_numeric <- as.numeric(res1$toxval_numeric)
   #print(str(res1))

  res1 <- res1[c(names(res1[1:2]),"name","toxval_type","toxval_numeric","toxval_numeric_qualifier",names(res1[3:7]))]
  colnames(res1) <- c("test_id", "casrn","name","toxval_type","toxval_numeric","toxval_numeric_qualifier",
                      "original_toxval_numeric", "toxval_units", "critical_effect", "reference", "reference_url")

  open_paranthesis_effect <- which(str_count(res1$critical_effect,"\\(") == 0 & str_count(res1$critical_effect,"\\)") == 1)
  open_paranthesis_effect2 <- grep("[^\\)]$",res1[which(str_count(res1$critical_effect,"\\(") == 0 & str_count(res1$critical_effect,"\\)") == 1),"critical_effect"])
  critical_effect_to_clean <- open_paranthesis_effect[open_paranthesis_effect2]
  res1[critical_effect_to_clean,"critical_effect"] <- gsub("\\)","",res1[critical_effect_to_clean,"critical_effect"])
  res1[is.na(res1$name),"name"] = "noname"
  res1[res1$name=="-","name"] = "noname"

  res = subset(res1,select=-c(test_id))
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="TEST",table="source_test",res=res,F,T,T)
}
