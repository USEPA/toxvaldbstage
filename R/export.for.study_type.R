#-----------------------------------------------------------------------------------
#
#' Export data required for setting the study type
#'
#' @param toxval.db Database version
#' @return writes an Excel file with the name
#'  ../export/toxval_pod_summary_[human_eco]_Sys.Date().xlsx
#'
#-----------------------------------------------------------------------------------
export.for.study_type <- function(toxval.db="res_toxval_v93") {
  printCurrentFunction(toxval.db)
  dir = paste0(toxval.config()$datapath,"dictionary/")
  slist = runQuery("select distinct source from toxval",toxval.db)[,1]
  res = NULL
  for(src in slist) {
    query = paste0("SELECT
                    a.dtxsid,a.casrn,a.name,
                    b.source,
                    b.risk_assessment_class,
                    b.toxval_type,
                    b.toxval_units,
                    b.study_type,
                    b.study_type as study_type_corrected,
                    b.study_duration_value,
                    b.study_duration_units,
                    d.common_name,
                    b.generation,
                    b.exposure_route,
                    b.exposure_method,
                    b.critical_effect,
                    f.long_ref,
                    f.title,
                    b.source_hash,
                    a.cleaned_casrn,a.cleaned_name
                    FROM
                    toxval b
                    INNER JOIN source_chemical a on a.chemical_id=b.chemical_id
                    LEFT JOIN species d on b.species_id=d.species_id
                    INNER JOIN record_source f on b.toxval_id=f.toxval_id
                    INNER JOIN toxval_type_dictionary e on b.toxval_type=e.toxval_type
                    WHERE
                    b.source='",src,"'
                    and b.human_eco='human health'
                    and e.toxval_type_supercategory in ('Point of Departure','Lethality Effect Level','Toxicity Value')")

    mat = runQuery(query,toxval.db,T,F)
    mat = unique(mat)
    mat[mat$casrn=='-',"casrn"] = mat[mat$casrn=='-',"cleaned_casrn"]
    mat[mat$name=='-',"name"] = mat[mat$name=='-',"cleaned_name"]

    cremove = c("cleaned_name","cleaned_casrn","casrn")
    mat = mat[ , !(names(mat) %in% cremove)]
    cat(src,nrow(mat),"\n")
    res = rbind(res,mat)
  }
  file <- paste0(dir,"/toxval_new_study_type ",toxval.db," ",Sys.Date(),".xlsx")
  sty <- createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  write.xlsx(res,file,firstRow=T,headerStyle=sty)
}
