#-------------------------------------------------------------------------------------
#' Load the FLEX data (old ACToR data) from files to toxval source. This will load all
#' Excel file in the folder ACToR replacements/
#' @param db The version of toxval_source into which the tables are loaded.
#' @param filepath The path for all the input xlsx files ./ACToR replacements
#' @param verbose Whether the loaded rows should be printed to the console.
#' @param chem.check.halt If TRUE and there are problems with chemicals CASRN checks, halt the program
#' @param do.clean If true, remove data for these sources before reloading
#' @export
#--------------------------------------------------------------------------------------
import_flex_source <- function(db,
                               filepath="ACToR replacements",
                               verbose=F,
                               chem.check.halt=F,
                               do.clean = F) {
  printCurrentFunction(db)
  filepath = paste0(toxval.config()$datapath,filepath)
  #####################################################################
  cat("Build all old ACToR tables \n")
  #####################################################################
  files.list <- list.files(path = filepath, pattern = "*.xlsx")
  any_temp_files <- grep("^\\~\\$.*", files.list, value = T)
  files.list <- files.list[! files.list %in% any_temp_files]
  files.list <- paste0( filepath, '/',files.list)
  print(files.list)
  browser()
  res <- lapply(files.list,openxlsx::read.xlsx)

  names.list <- gsub("(.*)(\\/)(.*)(for.*)","\\3",files.list)
  names.list <- gsub("(.*)(\\_$|\\s+$)","\\1",names.list)
  names.list <- tolower(names.list)
  names.list <- gsub("\\s+","_",names.list)
  names.list[names.list == "penndep"] <- "penn_dep"
  names(res) <- names.list

  for(i in 1:length(res)) {
    nres = names(res)[i]
    res0 = res[[i]]
    res0 = res0[!is.element(res0$casrn,"NOCAS"),]
    source = res0[1,"source"]
    table = paste0("source_",nres)
    if(do.clean) {
      query = paste0("delete from ",table)
      cat(query,"\n")
      runQuery(query,db)
    }
    cat("-------------------------------\n",nres,":",source,":",table,"\n-------------------------------\n")
    #####################################################################
    cat("Prep and load the data\n")
    #####################################################################
    source_prep_and_load(db,source=source,table=table,res=res0,F,T,T)
  }
}
