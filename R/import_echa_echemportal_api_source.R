#--------------------------------------------------------------------------------------
#' Load ECHA echemportal api Source into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param filepath The path for all the input xlsx files ./echa_echemportal_api/echa_echemportal_api_files

#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#--------------------------------------------------------------------------------------
import_echa_echemportal_api_source <- function(db,
                                               filepath="echa_echemportal_api/echa_echemportal_api_files",
                                               chem.check.halt=T) {
  printCurrentFunction(db)

  filepath = paste0(toxval.config()$datapath,"",filepath)
  #####################################################################
  cat("Build source_echa_echemportal_api table\n")
  #####################################################################
  files.list <- list.files(path = filepath, pattern = "*.xlsx")
  any_temp_files <- grep("^\\~\\$.*", files.list, value = T)
  files.list <- files.list[! files.list %in% any_temp_files]

  files.list <- paste0(filepath, "/", files.list)
  res <- lapply(files.list,openxlsx::read.xlsx)
  names(res) <- gsub("(.*\\/eChemPortalAPI_)(.*)(_.*)", "\\2", files.list)
  for (i in 1:length(res)) {
    res[[i]] <- lapply(res[[i]], function(x) type.convert(as.character(x), as.is = T))
    res[[i]] <- data.frame(res[[i]], stringsAsFactors = F)
  }
  # res[[16]] and res[[18]] has no data in excel files in latest data retrieval on 9-14-21, cause identified as echa portal changes

  for (i in c(1:15,17,19:26)) {
    res[[i]][sapply(res[[i]], function(x) all(is.na(x) == T))] <- ""
  }

  res1 <- rbindlist(res, fill = TRUE, idcol = 'source_table')
  res1 <- data.frame(res1, stringsAsFactors = F)
  res1$Name <- enc2utf8(res1$Name)
  names(res1) <- tolower(names(res1))
  res1$year <- res1$years
  res1 <- res1[ , !(names(res1) %in% c("years"))]
  res1$experimental.value <- enc2utf8(res1$experimental.value)
  names(res1) <- gsub("\\.+","_", names(res1))
  res1 = res1[!is.na(res1$number_type),]
  res1 = res1[res1$number_type=="CAS Number",]

  #####################################################################
  cat("Do the chemical checking\n")
  #####################################################################
  source = "ECHA eChemPortal"
  res = as.data.frame(res1)
  res = res[!is.element(res$number,c("134895-42-8","61-76-3")),]
  names(res)[is.element(names(res),"number")] = "casrn"
  print(dim(res))

  source_prep_and_load(db,source=source,table="source_echa_echemportal_api",res=res,F,T,T)
}
