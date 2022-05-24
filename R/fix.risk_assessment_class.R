#-------------------------------------------------------------------------------------
#' Set the risk assessment class of toxval according to an excel dictionary.
#' Values may beset multiple times, so the excel sheet should be ordered so that
#' the last ones to be set are last
#'
#' @param toxval.db The version of toxval in which the data is altered.
#' @param restart If TRUE, delete all values and start from scratch
#' @export
#--------------------------------------------------------------------------------------
fix.risk_assessment_class <- function(toxval.db,restart=T) {
  printCurrentFunction(toxval.db)

  if(restart) {
    query <- "update toxval set risk_assessment_class = '-'"
    runInsert(query,toxval.db,T,F,T)
  }
  cat("deal with ecotox\n")
  st.list = runQuery("select distinct study_type from toxval where source='ECOTOX'",toxval.db)[,1]
  sdc.list = runQuery("select distinct study_duration_class from toxval where source='ECOTOX'",toxval.db)[,1]
  for(st in st.list) {
    for(sdc in sdc.list) {
      rac = paste(sdc,tolower(st))
      cat("ecotox ",st,sdc,rac,"\n")
      query = paste0("update toxval set risk_assessment_class = '",rac,"' where source = 'ECOTOX' and
                      study_type = '",st,"' and study_duration_class = '",sdc,"' and risk_assessment_class='-'")
      runInsert(query,toxval.db,T,F,T)
      n0 = runQuery("select count(*) from toxval",toxval.db)[1,1]
      n1 = runQuery("select count(*) from toxval where risk_assessment_class='-'",toxval.db)[1,1]
      cat("RAC still missing: ",n1," out of ",n0," : ",rac,"\n")
    }
  }

  n1.0 = 0
  conv = read.xlsx(paste0(toxval.config()$datapath,"dictionary/RAC_rules.xlsx"))
  conv = conv[conv$order>0,]
  conv = conv[order(conv$term),]
  conv = conv[order(conv$risk_assessment_class),]
  conv = conv[order(conv$order),]
  for(i in 1:nrow(conv)){
    term <- conv[i,"term"]
    rac <- conv[i,"risk_assessment_class"]
    field <- conv[i,"field"]
    source <- conv[i,"source"]
    order = conv[i,"order"]
    #browser()
    if(source!='x') {
      if(field=="guideline") {
      #  sid <- runQuery(paste0("select study_id from study_details where guideline='",term,"'"),toxval.db)[1,1]
      #  query <- paste0("update toxval set risk_assessment_class = '",rac,"' where study_id= ",sid," and risk_assessment_class='-'")
      #  runInsert(query,toxval.db,T,F,T)
      }
      else {
        if(source=="*")  query <- paste0("update toxval set risk_assessment_class = '",rac,"' where ",field," = '",term,"' and risk_assessment_class='-'")
        else query <- paste0("update toxval set risk_assessment_class = '",rac,"' where ",field," = '",term,"' and source = '",source,"' and risk_assessment_class='-'")
        runInsert(query,toxval.db,T,F,T)
      }
      if(field!="guideline") {
        n0 <- runQuery("select count(*) from toxval",toxval.db)[1,1]
        n1 <- runQuery("select count(*) from toxval where risk_assessment_class='-'",toxval.db)[1,1]
        cat("RAC still missing: ",order," : ",n1," out of ",n0," from original:",field,":",term," to rac:",rac,"\n")
        if(n1==0) return()
        if(n1.0==n1 && restart==T) browser()
        n1.0 = n1
      }
    }
  }

  file = "../export/toxval_missing_rac.xlsx"
  mat = runQuery("select * from toxval where risk_assessment_class='-'",toxval.db)
  write.xlsx(mat,file)
}
