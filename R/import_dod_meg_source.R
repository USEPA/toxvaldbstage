#--------------------------------------------------------------------------------------
#' @description Load DOD MEG to toxval_source. The file to be loaded are in ./dod_meg/dod_meg_files
#'
#' @param db The version of toxval_source into which the tables are loaded.
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#' @export 
#' @title FUNCTION_TITLE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[openxlsx]{read.xlsx}}
#'  \code{\link[stats]{reshape}}
#'  \code{\link[stringr]{word}}
#'  \code{\link[tidyr]{reexports}}
#' @rdname import_dod_meg_source
#' @importFrom openxlsx read.xlsx
#' @importFrom stats reshape
#' @importFrom stringr word
#' @importFrom tidyr contains
#-------------------------------------------------------------------------------------
import_dod_meg_source = function(db,
                             chem.check.halt=F) {
  printCurrentFunction(db)

  #Air-long-term
  openxlsx::read.xlsx(paste0(toxval.config()$datapath,"dod_meg/dod_meg_files/DOD_Air-MEGs_Long-Term_2013.xlsx")) -> mat
  mat = mat[3:nrow(mat),c(2:3,7:10)]
  names(mat) = c("casrn", "name","numeric_Negligible","subsource_Negligible", "numeric_Marginal","subsource_Marginal")
  mat$id = 1:nrow(mat)
  mat = stats::reshape(mat, varying = 3:6, idvar = "id", direction = "long", sep = "_")
  names(mat)[4] = "MEG-type"
  mat = mat[!is.na(mat$numeric),]
  mat$subtype = "Long-term"
  mat$route = "Inhalation"
  mat$method = "Air"
  mat$duration = "1 year"
  mat$units = "mg/m3"
  mat$units[mat$name=="Asbestos"] = "fibers/cm3"
  total = mat

  #Air-short-term
  openxlsx::read.xlsx(paste0(toxval.config()$datapath,"dod_meg/dod_meg_files/DOD_Air-MEGS_Short-Term_2013.xlsx")) -> mat
  mat = mat[4:nrow(mat),c(2:3,7:15)]
  names(mat) = c("casrn", "name","numeric_Negligible","subsource_Negligible", "numeric_Marginal","subsource_Marginal", "numeric_Critical","subsource_Critical", "numeric_Catastrophic","subsource_Catastrophic", "MEG-info")
  mat$id = 1:nrow(mat)
  mat = stats::reshape(mat, varying = 3:10, idvar = "id", direction = "long", sep = "_")
  names(mat)[5] = "MEG-type"
  mat = mat[!is.na(mat$numeric),]
  mat$subtype = "Short-term"
  mat$route = "Inhalation"
  mat$method = "Air"
  mat$duration = stringr::word(mat$`MEG-info`,1,2,sep = "-|\\s")
  mat$units = "mg/m3"
  mat = mat[,c(1:2,4:12)]
  total = rbind(total,mat)

  #Soil
  openxlsx::read.xlsx(paste0(toxval.config()$datapath,"dod_meg/dod_meg_files/DOD_Soil-MEGs_2013.xlsx")) -> mat
  mat = mat[3:nrow(mat),c(2:3,7:8)]
  names(mat) = c("casrn", "name","numeric","subsource")
  mat$id = 1:nrow(mat)
  mat = mat[!is.na(mat$numeric),]
  mat$subtype = "Soil"
  mat$route = "Soil"
  mat$method = "Soil"
  mat$duration = "1 year"
  mat$units = "mg/kg"
  mat$'MEG-type' = "Negligible"
  total = rbind(total,mat)

  #Water Long-term
  openxlsx::read.xlsx(paste0(toxval.config()$datapath,"dod_meg/dod_meg_files/DOD_Water-MEGS_Long-Term_2013.xlsx")) -> mat
  mat = mat[3:nrow(mat),c(2:3,7:8)]
  names(mat) = c("casrn", "name","numeric","subsource")
  mat$id = 1:nrow(mat)
  mat = mat[!is.na(mat$numeric),]
  mat$subtype = "Long-Term, 5L/d"
  mat$route = "Oral"
  mat$method = "Water"
  mat$duration = "1 year"
  mat$units = "mg/L"
  mat$units[mat$name=="Asbestos"] = "fibers/L"
  mat$'MEG-type' = "Negligible"
  total = rbind(total,mat)

  #Water Short-term
  openxlsx::read.xlsx(paste0(toxval.config()$datapath,"dod_meg/dod_meg_files/DOD_Water-MEGs_Short-Term_2013.xlsx")) -> mat
  mat = mat[4:nrow(mat),c(2:3,7:11)]
  names(mat) = c("casrn", "name","numeric_5","subsource_5","numeric_15","subsource_15", "duration")
  mat$id = 1:nrow(mat)
  mat = stats::reshape(mat, varying = 3:6, idvar = "id", direction = "long", sep = "_")
  mat = mat[!is.na(mat$numeric),]
  mat$subtype = paste0("Short-Term, ",mat$time,"L/d")
  mat$route = "Oral"
  mat$method = "Water"
  mat$duration = stringr::word(mat$duration,1,2)
  mat$units = "mg/L"
  mat$'MEG-type' = "Negligible"
  mat = mat[,c(1:4,6:12)]
  total = rbind(total,mat)

  mat = total
  mat[,5]=as.numeric(mat[,5])
  mat$subsource = iconv(mat$subsource, "UTF-8", "ASCII", sub = "")
  names(mat)[4:5] = c("meg_type", "toxval_numeric")
  mask = vector(length=nrow(mat),mode="integer")
  mask[] = 1
  for(i in 1:nrow(mat)) if(tidyr::contains(mat[i,"casrn"],"ACToR")) mask[i] = 0
  mat = mat[mask==1,]

  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="DOD",table="source_dod_meg",res=mat,F,T,T)
}
