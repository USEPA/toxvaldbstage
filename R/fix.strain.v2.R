#--------------------------------------------------------------------------------------
#'
#' Set the strain information in toxval
#'
#' @param toxval.db The version of the database to use
#' @export
#--------------------------------------------------------------------------------------
fix.strain.v2 <- function(toxval.db,source,date_string="2022-05-25") {
  printCurrentFunction()
  file = paste0(toxval.config()$datapath,"species/strain_dictionary_",date_string,".xlsx")
  dict = read.xlsx(file)

  so = runQuery(paste0("select distinct strain_original from toxval where source='",source,"'"),toxval.db)[,1]
  count.good = 0
  cat("Start setting strain\n")
  if(length(so)>0) {
    for(i in 1:length(so)) {
      tag = so[i]
      if(is.element(tag,dict$strain_original)) {
        strain = dict[is.element(dict$strain_original,tag),"strain"]
        sg = dict[is.element(dict$strain_original,tag),"strain_group"]
        query = paste0("update toxval set strain='",strain,"', strain_group='",sg,"' where source='",source,"' and strain_original='",tag,"'")
        runQuery(query,toxval.db)
      }
      if(i%%100==0) cat("finished",i,"out of",length(so),":",count.good,"\n")
    }
  }
}
