#-------------------------------------------------------------------------------------
#' Set the risk assessment class of toxval according to an excel dictionary.
#' Values may beset multiple times, so the excel sheet should be ordered so that
#' the last ones to be set are last
#'
#' @param toxval.db The version of toxval in which the data is altered.
#' @param restart If TRUE, delete all values and start from scratch
#' @export
#--------------------------------------------------------------------------------------
fix.risk_assessment_class.by.source <- function(toxval.db,source, restart=F) {
  printCurrentFunction(paste(toxval.db,":", source))
  conv = read.xlsx(paste0(toxval.config()$datapath,"dictionary/RAC_rules.xlsx"))
  print(dim(conv))
  conv = conv[conv$order>0,]
  conv = conv[order(conv$term),]
  conv = conv[order(conv$risk_assessment_class),]
  conv = conv[order(conv$order),]

  if(restart) {
    query <- paste0("update toxval set risk_assessment_class = '-'  where source like '",source,"'")
    runInsert(query,toxval.db,T,F,T)
  }

  if(source == "DOD"){
    cat("deal with DOD\n")
    dict = conv[conv$source=="DOD",]
    for(i in 1:nrow(dict)) {
      term <- dict[i,"term"]
      rac <- dict[i,"risk_assessment_class"]
      field <- dict[i,"field"]
      #Source <- dict[i,"source"]
      order = dict[i,"order"]
      query <- paste0("update toxval set risk_assessment_class = '",rac,"' where ",field," = '",term,"' and source = '",source,"' and risk_assessment_class='-'")
      runInsert(query,toxval.db,T,F,T)
    }
  }
  else if(source == "ECOTOX"){
    cat("deal with ECOTOX\n")
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
  }
  else {
    n1.0 = 0
    for(i in 1:nrow(conv)){
      term <- conv[i,"term"]
      rac <- conv[i,"risk_assessment_class"]
      field <- conv[i,"field"]
      source_c <- conv[i,"source"]
      order = conv[i,"order"]
      #browser()
      if(source_c!='x') {
        if(field=="guideline") {
          #  sid <- runQuery(paste0("select study_id from study_details where guideline='",term,"'"),toxval.db)[1,1]
          #  query <- paste0("update toxval set risk_assessment_class = '",rac,"' where study_id= ",sid," and risk_assessment_class='-'")
          #  runInsert(query,toxval.db,T,F,T)
        }
        else {
          if(source_c=="*")  query <- paste0("update toxval set risk_assessment_class = '",rac,"' where ",field," = '",term,"' and source = '",source,"' and risk_assessment_class='-'")
          else if(source_c==source) {
            query <- paste0("update toxval set risk_assessment_class = '",rac,"' where ",field," = '",term,"' and source = '",source,"' and risk_assessment_class='-'")
          }
          else query <- paste0("update toxval set risk_assessment_class = '",rac,"' where ",field," = '",term,"' and source = '",source,"' and risk_assessment_class='-'")
          runInsert(query,toxval.db,T,F,T)
        }
        if(field!="guideline") {
          n0 <- runQuery(paste0("select count(*) from toxval where source = '",source,"'"),toxval.db )[1,1]
          n1 <- runQuery(paste0("select count(*) from toxval where risk_assessment_class='-' and source = '",source,"'") ,toxval.db)[1,1]
          cat("RAC still missing: ",order," : ",n1," out of ",n0," from original:",field,":",term," to rac:",rac,"\n")
          if(n1==0) return()
          if(n1.0==n1 && restart==T)    n1.0 = n1
        }
      }
    }
  }
  # file = "./export/toxval_missing_rac.xlsx"
  # mat = runQuery(paste0("select * from toxval where risk_assessment_class='-' and source like '",source,"'") ,toxval.db)
  # write.xlsx(mat,file)
}
