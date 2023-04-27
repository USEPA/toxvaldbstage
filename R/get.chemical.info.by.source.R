#-------------------------------------------------------------------------------------
#' @description get chemical info from source db tables
#' @param source.db The version of toxval source to use.
#' @param source The name of toxval source to use.
#' @param source_table The name of toxval source table to use.
#' @param file_id The suffixed 5 digit identifiers specified in the file names in the folder ./chemical_mapping/source_chemical_files
#' @return database info collected
#' @export
#' @title FUNCTION_TITLE
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#' @rdname get.chemical.info.by.source
#--------------------------------------------------------------------------------------
get.chemical.info.by.source <- function(source.db,source_table,source, file_id){
  printCurrentFunction(paste(source.db,":", source_table,":",source,":", file_id))


  #####################################################################
  cat("extract source info \n")
  #####################################################################

  mat = runQuery(paste0("select * from ",source_table," "), source.db)

  name_synonyms <- c("name","NAME","Analyte.Name","Chemical.Name")
  names(mat)[names(mat) %in% name_synonyms] <- "name"

  casrn_synonyms <- c("casrn","CASRN","Analyte.Code","CAS.Number")
  names(mat)[names(mat) %in% casrn_synonyms] <- "casrn"

  #####################################################################
  cat("Build chemical_info file from mat\n")
  #####################################################################
  chemical_information <- mat[,c("casrn","name")]
  chemical_information <- unique(chemical_information)
  chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
  chemical_information <- chemical_information[,c('chemical_id','name','casrn')]
  names(chemical_information) <- c("chemical_id","raw_name","raw_casrn")
  id_prefix <- paste("ToxVal",file_id, sep = "")
  chemical_information$chemical_id <- paste(id_prefix, chemical_information$chemical_id, sep = "_")
  file <- paste0(toxval.config()$datapath,"source_chemical_info_files/chemical_information_for_",source,"_",Sys.Date(),".xlsx")
  #write.xlsx(chemical_information,file)
  print(chemical_information)

  chem_table_name <- paste0(source,"_2022_04_19_",file_id)
  chem_table_name <- gsub("\\s+","_",chem_table_name)
  chem_table_name <- gsub("\\.","",chem_table_name)
  chem_table_name <- gsub("\\(","",chem_table_name)
  chem_table_name <- gsub("\\)","",chem_table_name)
  chem_table_name <- tolower(chem_table_name)
  print(chem_table_name)


  runInsertTable(chemical_information,chem_table_name,source.db,do.halt=T,verbose=F)


}

