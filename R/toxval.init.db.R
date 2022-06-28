#-------------------------------------------------------------------------------------
#' Initialize the database
#' @param toxval.db The version of toxval into which the tables are loaded.
#' @export
#--------------------------------------------------------------------------------------
toxval.init.db <- function(toxval.db) {
  printCurrentFunction(toxval.db)

  #################################################################
  cat("load the info table\n")
  #################################################################
  query = paste0("insert into info (name,description,date_created) values ('res_toxvald_v92','ToxVal DB v9.2','",Sys.Date(),"')")
  runQuery(query,toxval.db)

  #################################################################
  cat("load the dictionaries\n")
  #################################################################
  import.dictionary(toxval.db)

  #################################################################
  cat("load the species_ecotox table\n")
  #################################################################
  date_string="2022-05-25"
  file =paste0(toxval.config()$datapath,"species/ecotox_species_dictionary_",date_string,".xlsx")
  dict = read.xlsx(file)
  file = paste0(toxval.config()$datapath,"species/ecotox_species_synonyms_",date_string,".xlsx")
  synonyms = read.xlsx(file)
  file = paste0(toxval.config()$datapath,"species/toxvaldb_extra_species_",date_string,".xlsx")
  extra = read.xlsx(file)

  dict2 = extra[,c("species_id","common_name","latin_name","ecotox_group")]
  dict2 = dict2[!is.element(dict2$species_id,dict$species_id),]
  dict2$kingdom = NA
  dict2$phylum_division = NA
  dict2$subphylum_div = NA
  dict2$superclass = NA
  dict2$class = NA
  dict2$tax_order = NA
  dict2$family = NA
  dict2$genus = NA
  dict2$species = NA
  dict2$subspecies = NA
  dict2$variety = NA
  dict2 = dict2[,names(dict)]
  dict = rbind(dict,dict2)
  count = runQuery("select count(*) from species_ecotox where species_id=-1",toxval.db)[1,1]
  if(count==0) {
    runInsert("insert into species_ecotox (species_id,common_name,latin_name) values (-1,'Not Specified','Not Specified')",toxval.db,do.halt=T)
  }
  runQuery("delete from species_ecotox where species_id>=0",toxval.db)
  runInsertTable(dict,"species_ecotox",toxval.db)
}
