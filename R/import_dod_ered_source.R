#--------------------------------------------------------------------------------------
#' Load dod Source into dev_toxval_source_v2.
#' @param db The version of toxval into which the source is loaded.
#' @param infile The input file ./dod/dod_files/USACE_ERDC_ERED_database_12_07_2018.xlsx

#--------------------------------------------------------------------------------------
import_dod_ered_source <- function(db,
                                   infile="../dod/dod_files/USACE_ERDC_ERED_database_12_07_2018.xlsx",
                                   chem.check.halt=F) {
  printCurrentFunction(db)

  #####################################################################
  cat("Build original_dod_ered_table and create dataframe res1 \n")
  #####################################################################

  dod_table <- openxlsx::read.xlsx(infile,1)
  dod_table$Date.Modified <- excel_numeric_to_date(as.numeric(as.character(dod_table$Date.Modified)), date_system = "modern")
  dod_table$Date.Modified <- format(dod_table$Date.Modified, format = "%d-%b-%y")
  dod_table["dod_id"] <- c(1:length(dod_table[,1]))
  dod_table <- dod_table[c("dod_id",names(dod_table[-43]))]

  colnames(dod_table) <- c("dod_id","ered_id","ref_id","study_type","species",
                           "common_name","name_categories","life_stage","animal_source",
                           "name","casrn","chemical_group","mixed_chemical","spiked_chemical",
                           "media","exposure_route","exposure_conc","exposure_units","dose",
                           "study_duration","tissue_residue_conc","tissue_residue_conc_units",
                           "test_tissue_type","critical_effect","effect_trend","percentage_effect",
                           "risk","effect_significance","p_value","control_result","comments",
                           "data_source","data_year","phylum","phylum_desc","class","class_desc",
                           "order","order_desc","habitat_desc","environment","ered_date_modified",
                           "moisture_percentage")
  dod_table = dod_table[dod_table$casrn!="N/A",]
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="DOD ERED",table="source_dod_ered",res=dod_table,F,T,T)
}
