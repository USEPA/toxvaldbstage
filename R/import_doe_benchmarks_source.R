library("openxlsx")
#--------------------------------------------------------------------------------------
#' Load doe_benchmarks Source into dev_toxval_source_v2.
#' @param db The version of toxval into which the source is loaded.
#' @param infile The input file ./doe_benchmarks/doe_benchmarks_files/DOE_Wildlife_Benchmarks_1996.xlsx
#--------------------------------------------------------------------------------------
import_doe_benchmarks_source <- function(db,
                                         infile="../doe_benchmarks/doe_benchmarks_files/DOE_Wildlife_Benchmarks_1996.xlsx",
                                         chem.check.halt=F) {
  printCurrentFunction(db)

  #####################################################################
  cat("Build original_doe_benchmarks_table \n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile ,1,colNames = T)
  names(res) = c("source_name_sid","casrn","name","url",
                 "data_collection","source_name_cid","analyte","form",
                 "test_species","test_species_noael","test_species_loael",
                 "endpoint_species",
                 "wildlife_noael","noael_food","noael_water","noael_piscivore",
                 "wildlife_loael","loael_food","loael_water","loael_piscivore" )

  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="DOE Wildlife Benchmarks",table="source_doe_benchmarks",res=res,F,T,T)
}
