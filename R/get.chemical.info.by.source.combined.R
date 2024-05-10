# Script to create chemical table for toxval_source across all source tables
# By: Jonathan Taylor Wall
# Created: 2022-05-27
# R version 4.1.0 (2021-05-18)
# dplyr_1.0.8; RMySQL_0.10.23; DBI_1.1.2; readxl_1.3.1

#-------------------------------------------------------------------------------------
#' @title get.chemical.info.by.source.combined
#' @description get chemical info from source db tables for curation, create
#' chemical table to map curated chemicals to.
#' @param source.db The version of toxval source to use.
#' @param source The name of toxval source to use.
#' @param source_table The name of toxval source table to use.
#' @return database info collected
#' @export
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{bind}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{context}}
#' @rdname get.chemical.info.by.source.combined
#' @importFrom dplyr bind_rows mutate n
#--------------------------------------------------------------------------------------
get.chemical.info.by.source.combined <- function(source.db,source_table,source){
  message("Function still in draft phase - waiting for more guidance")
  return()
  printCurrentFunction(paste(source.db,":", source_table,":",source))
  # Get unique list of source tables to pull chemicals from (ignore source chemical tables)
  tbl_list = runQuery(query=paste0("SELECT TABLE_NAME FROM information_schema.tables WHERE TABLE_TYPE = 'base table' AND TABLE_SCHEMA='",source.db,"'"),
                      db=source.db) %>%
    unlist() %>% unname()
  # Filter out source chemical tables
  tbl_filter = sapply(tbl_list, function(x){
    tmp = strsplit(x, split="_")[[1]]
    if(length(tmp) > 4){
      return(paste(tmp[1:(length(tmp)-4)], collapse = "_"))
    }
    return(paste(tmp, collapse = "_"))
  }) %>% unique()

  tbl_list = tbl_list[tbl_list %in% tbl_filter]
  #####################################################################
  cat("extract source info \n")
  #####################################################################
  chems = lapply(tbl_list, function(src){
    #####################################################################
    cat("Build chemical_info file from mat: ",src,"\n")
    #####################################################################
    mat = runQuery(paste0("select * from ",src), source.db)

    # Rename fields containing chemical name information to "name"
    name_synonyms <- c("name","NAME","Analyte.Name","Chemical.Name")
    if(!any(name_synonyms %in% names(mat))){
      cat(paste0(src, " missing chemical name column...\n"))
      return(NULL)
    }
    names(mat)[names(mat) %in% name_synonyms] <- "name"

    # Rename fields containing chemical CASRN information to "casrn"
    casrn_synonyms <- c("casrn","CASRN","Analyte.Code","CAS.Number")
    if(!any(casrn_synonyms %in% names(mat))){
      cat(paste0(src, " missing casrn column...\n"))
      return(NULL)
    }
    names(mat)[names(mat) %in% casrn_synonyms] <- "casrn"

    # Export source chemical information
    chemical_information <- mat[,c("casrn","name")]
    chemical_information <- unique(chemical_information)
    chemical_information["chemical_id"] <- c(1:length(chemical_information[,1]))
    chemical_information <- chemical_information[,c('chemical_id','name','casrn')]
    names(chemical_information) <- c("chemical_id","raw_name","raw_casrn")
    chemical_information$raw_casrn = as.character(chemical_information$raw_casrn)
    #id_prefix <- paste("ToxVal",file_id, sep = "")
    id_prefix <- src
    chemical_information$chemical_id <- paste(id_prefix, chemical_information$chemical_id, sep = "_")
    file <- paste0(toxval.config()$datapath,"source_chemical_info_files/chemical_information_for_",src,"_",Sys.Date(),".xlsx")
    #write.xlsx(chemical_information,file)

    # chem_table_name <- paste0(source,"_2022_04_19_",file_id)
    # chem_table_name <- gsub("\\s+","_",chem_table_name)
    # chem_table_name <- gsub("\\.","",chem_table_name)
    # chem_table_name <- gsub("\\(","",chem_table_name)
    # chem_table_name <- gsub("\\)","",chem_table_name)
    # chem_table_name <- tolower(chem_table_name)
    # print(chem_table_name)
    #print(View(chemical_information))
    return(chemical_information)
  }) %>% dplyr::bind_rows()

  # Add ID column
  chems = chems %>%
    dplyr::mutate(id = 1:dplyr::n())
  # Insert Chemical Table for all source tables
  #runInsertTable(chemical_information,"toxval_source_chemicals",source.db,do.halt=T,verbose=F)

}


