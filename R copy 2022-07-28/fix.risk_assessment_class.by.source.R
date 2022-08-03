#-------------------------------------------------------------------------------------
#' Set the risk assessment class of toxval according to an excel dictionary.
#' Values may beset multiple times, so the excel sheet should be ordered so that
#' the last ones to be set are last
#'
#' @param toxval.db The version of toxval in which the data is altered.
#' @param source The source to be updated
#' @param restart If TRUE, delete all values and start from scratch
#' @export
#--------------------------------------------------------------------------------------
fix.risk_assessment_class.by.source <- function(toxval.db,source, restart=T) {
  printCurrentFunction(paste(toxval.db,":", source))
  conv = read.xlsx(paste0(toxval.config()$datapath,"dictionary/RAC_rules_by_source v92.xlsx"))
  print(dim(conv))
  conv = conv[conv$order>0,]
  conv = conv[order(conv$term),]
  conv = conv[order(conv$risk_assessment_class),]
  conv = conv[order(conv$order),]
  conv = conv[conv$useme==1,]
  conv = conv[!is.na(conv$source),]

  if(restart) {
    query = paste0("update toxval set risk_assessment_class = '-'  where source like '",source,"'")
    runInsert(query,toxval.db,T,F,T)
  }
  dict = conv[conv$source==source,]
  n1.0 = runQuery(paste0("select count(*) from toxval where risk_assessment_class='-' and source = '",source,"'") ,toxval.db)[1,1]
  for(i in 1:nrow(dict)){
    term = dict[i,"term"]
    rac = dict[i,"risk_assessment_class"]
    field = dict[i,"field"]
    source_c = dict[i,"source"]
    order = dict[i,"order"]
    query = paste0("update toxval set risk_assessment_class = '",rac,"' where ",field," = '",term,"' and source = '",source,"' and risk_assessment_class='-'")
    runInsert(query,toxval.db,T,F,T)
    n0 = runQuery(paste0("select count(*) from toxval where source = '",source,"'"),toxval.db )[1,1]
    n1 = runQuery(paste0("select count(*) from toxval where risk_assessment_class='-' and source = '",source,"'") ,toxval.db)[1,1]
    cat("RAC still missing: ",order," : ",n1," out of ",n0," from original:",field,":",term," to rac:",rac,"\n")
    if(n1==0) break()
  }
  if(n1>0) {
    query = paste0("SELECT
                    b.toxval_id,b.source_hash,b.source_table,
                    a.dtxsid,a.casrn,a.name,b.chemical_id,
                    b.source,b.subsource,
                    b.source_url,b.subsource_url,
                    b.qc_status,
                    b.details_text,
                    b.priority_id,
                    b.risk_assessment_class,
                    b.human_eco,
                    b.toxval_type,b.toxval_type_original,
                    b.toxval_subtype,
                    e.toxval_type_supercategory,
                    e.toxval_type_category,
                    b.toxval_numeric,b.toxval_units,
                    b.toxval_numeric_original,b.toxval_units_original,
                    b.toxval_numeric_standard,b.toxval_units_standard,
                    b.toxval_numeric_human,b.toxval_units_human,
                    b.study_type,b.study_type_original,
                    b.study_duration_class,b.study_duration_class_original,
                    b.study_duration_value,b.study_duration_value_original,
                    b.study_duration_units,b.study_duration_units_original,
                    b.species_id,b.species_original,d.common_name,d.latin_name,d.ecotox_group,d.habitat,
                    b.strain,b.strain_group,b.strain_original,
                    b.sex,b.sex_original,
                    b.generation,b.lifestage,
                    b.exposure_route,b.exposure_route_original,
                    b.exposure_method,b.exposure_method_original,
                    b.exposure_form,b.exposure_form_original,
                    b.media,b.media_original,
                    b.critical_effect,
                    b.critical_effect_original,
                    b.year,
                    b.datestamp,
                    f.long_ref,
                    f.title,
                    f.author,
                    f.journal,
                    f.volume,
                    f.year,
                    f.issue,
                    f.url,
                    f.document_name
                    FROM
                    toxval b
                    INNER JOIN chemical a on a.dtxsid=b.dtxsid
                    LEFT JOIN species d on b.species_id=d.species_id
                    INNER JOIN toxval_type_dictionary e on b.toxval_type=e.toxval_type
                    INNER JOIN record_source f on b.toxval_id=f.toxval_id
                    WHERE
                    b.source='",source,"'
                   and risk_assessment_class='-'")

    query = paste0("SELECT
                    b.toxval_id,b.source_hash,b.source_table,
                    a.dtxsid,a.casrn,a.name,b.chemical_id,
                    b.source,b.subsource,
                    b.source_url,b.subsource_url,
                    b.qc_status,
                    b.details_text,
                    b.priority_id,
                    b.risk_assessment_class,
                    b.human_eco,
                    b.toxval_type,b.toxval_type_original,
                    b.toxval_subtype,
                    b.toxval_numeric,b.toxval_units,
                    b.toxval_numeric_original,b.toxval_units_original,
                    b.toxval_numeric_standard,b.toxval_units_standard,
                    b.toxval_numeric_human,b.toxval_units_human,
                    b.study_type,b.study_type_original,
                    b.study_duration_class,b.study_duration_class_original,
                    b.study_duration_value,b.study_duration_value_original,
                    b.study_duration_units,b.study_duration_units_original,
                    b.species_id,b.species_original,
                    b.strain,b.strain_group,b.strain_original,
                    b.sex,b.sex_original,
                    b.generation,b.lifestage,
                    b.exposure_route,b.exposure_route_original,
                    b.exposure_method,b.exposure_method_original,
                    b.exposure_form,b.exposure_form_original,
                    b.media,b.media_original,
                    b.critical_effect,
                    b.critical_effect_original,
                    b.year,
                    b.datestamp
                    FROM
                    toxval b
                    INNER JOIN chemical a on a.dtxsid=b.dtxsid
                    WHERE
                    b.source='",source,"'
                   and risk_assessment_class='-'")
    temp = runQuery(query,toxval.db)
    if(!is.element(source,c("DOD ERED","EFSA","HAWC","HPVIS","IRIS"))) browser()
    file = paste0(toxval.config()$datapath,"missing_rac/missing_",source,".xlsx")
    write.xlsx(temp,file)
  }
}
