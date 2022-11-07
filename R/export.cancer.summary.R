#-----------------------------------------------------------------------------------
#
#' Build a data frame of the Cancer calls and exports as xlsx
#'
#' @param toxval.db Database version
#' @param file.name If not NULL, read afles of casrn from the chemical folder and only export
#' those chemicals
#' @return writes and Excel file with the name
#'  ../qc_export/toxval_pod_summary_min_quality_id_[human_eco]_[min_quality_id]_[Sys.Date].xlsx
#'
#-----------------------------------------------------------------------------------
export.cancer.summary <- function(toxval.db, file.name=NA) {
  printCurrentFunction(toxval.db)

  query <- paste0("SELECT
                  a.casrn,a.name,a.dtxsid,b.source,b.cancer_call,b.exposure_route,b.url
                  FROM
                  chemical a
                  INNER JOIN cancer_summary b on a.dtxsid=b.dtxsid")

  mat <- runQuery(query,toxval.db,T,F)
  mat <- unique(mat)
  print(dim(mat))
  print(dim(mat))
  nrow <- dim(mat)[1]
  if(!is.na(file.name)) {
    file <- paste0(toxval.config()$datapath,"chemicals/for_load/",file.name,".xlsx")
    chems <- read.xlsx(file)
    casrn.list <- chems[,"casrn"]
    mat <- mat[is.element(mat[,"casrn"],casrn.list),]
    print(dim(mat))
  }
  file = paste0(toxval.config()$datapath,"export/toxval_cancer_summary_",Sys.Date(),".xlsx")
  if(!is.na(file.name)) file = paste0(toxval.config()$datapath,"export/toxval_cancer_summary_",file.name,"_",Sys.Date(),".xlsx")
  sty <- createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  write.xlsx(mat,file,firstRow=T,headerStyle=sty)
}
