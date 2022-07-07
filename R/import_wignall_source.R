#--------------------------------------------------------------------------------------
#' Load wignall Source data into dev_toxval_source_v2.
#' @param db The version of toxval into which the source info is loaded.
#' @param infile The input file ./wignall/wignall_files/BMD_Results_2014-06-17_reviewed Mar 2018.xlsx
#--------------------------------------------------------------------------------------
import_wignall_source <- function(db,
                                  infile="../wignall/wignall_files/BMD_Results_2014-06-17_reviewed Mar 2018 parsed.xlsx",
                                  chem.check.halt=T) {
  printCurrentFunction(db)

  #####################################################################
  cat("Build original_wignall_table and new_wignall_table \n")
  #####################################################################
  res1 <- openxlsx::read.xlsx(infile,1,startRow = 1)
  #runInsertTable(res1,"original_wignall_table",db,do.halt=T,verbose=F)
  res1["new_toxval_units"] <- gsub(".*\\(|\\)", "",res1$Toxicity.value.type )
  res1["new_toxval_type"] <- gsub("\\(.*|\\)", "",res1$Toxicity.value.type )
  res1["UF"] <- res1$POD / res1$Toxicity.Value

  colnames(res1) <- c("source_id", "casrn","name","original_toxval_type","original_toxval_units", "subsource",
                      "toxval_numeric", "POD_numeric", "POD_units","POD_type","organ","critical_effect",
                      "effect_description","dose_number","dose_values","dose_units","dose_converted",
                      "DR_type","mean_response","response_units","SD_of_response",
                      "total_number_of_animals","incidence_in_number_of_animals","BMR",
                      "BMD","BMDL","BMD/L_WIZARD_notes","action_taken","BMD'","BMDL'","BMD/L'_WIZARD_notes",
                      "comments","hyperlink","reference","toxval_units","toxval_type","UF")

  res1 <- res1[,c(1,2,3,7,35,36,4,5,6,8:10,37,11:34)]
  res1$critical_effect <- paste(res1$effect, res1$effect_description, sep = ";")
  res1$toxval_type <- gsub("\\s+$","", res1$toxval_type)

  nlist = c("source_id","casrn","name","toxval_numeric",
            "toxval_units","toxval_type","original_toxval_type","original_toxval_units",
            "subsource","pod_numeric","pod_units","pod_type",
            "uf","organ","critical_effect","effect_description",
            "dose_number","dose_values","dose_units","dose_converted",
            "dr_type","mean_response","response_units","sd_of_response",
            "total_number_of_animals","incidence_in_number_of_animals","bmr","bmd",
            "bmdl","bmdl_wizard_notes","action_taken","bmd_prime",
            "bmdl_prime","bmdl_prime_wizard_notes","comments","hyperlink",
            "reference")
  names(res1) = nlist

  browser()
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="Wignall",table="source_wignall",res=res1,F,T,T)
}
