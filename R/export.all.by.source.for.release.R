#-----------------------------------------------------------------------------------
#' Build a data frame of the data from toxval and export by source as a
#' series of xlsx files. This only has the columns tht someone outside might use
#'
#' @param toxval.db Database version
#' @param source The source to be updated
#' #' @return for each source writes an Excel file with the name
#'  ../export/export_by_source_{data}/toxval_all_{toxval.db}_{source}.xlsx
#'
#-----------------------------------------------------------------------------------
export.all.by.source.for.release <- function(toxval.db, source=NULL) {
  printCurrentFunction(toxval.db)

  dir = paste0(toxval.config()$datapath,"export/release_export_by_source_",toxval.db,"_",Sys.Date())
  if(!dir.exists(dir)) dir.create(dir)

  slist = sort(runQuery("select distinct source from toxval",toxval.db)[,1])
  if(!is.null(source)) slist=source

  for(src in slist) {
    query = paste0("SELECT
                    a.dtxsid,a.casrn,a.name,
                    b.source,b.subsource,
                    b.qc_status,
                    b.risk_assessment_class,
                    b.human_eco,
                    b.toxval_type,
                    b.toxval_subtype,
                    e.toxval_type_supercategory,
                    b.toxval_numeric,b.toxval_units,
                    b.study_type,
                    b.study_duration_class,
                    b.study_duration_value,
                    b.study_duration_units,
                    d.common_name,d.latin_name,d.ecotox_group,d.habitat,
                    b.strain,b.strain_group,
                    b.sex,
                    b.generation,
                    b.lifestage,
                    b.exposure_route,
                    b.exposure_method,
                    b.exposure_form,
                    b.media,
                    b.critical_effect,
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
                    a.cleaned_casrn,a.cleaned_name
                   FROM
                    toxval b
                    INNER JOIN source_chemical a on a.chemical_id=b.chemical_id
                    LEFT JOIN species d on b.species_id=d.species_id
                    INNER JOIN toxval_type_dictionary e on b.toxval_type=e.toxval_type
                    INNER JOIN record_source f on b.toxval_id=f.toxval_id
                    WHERE
                    qc_status='pass'
                    AND b.source='",src,"'")
    mat = runQuery(query,toxval.db,T,F)
    mat[mat$casrn=='-',"casrn"] = mat[mat$casrn=='-',"cleaned_casrn"]
    mat[mat$name=='-',"name"] = mat[mat$name=='-',"cleaned_name"]
    cremove = c("cleaned_name","cleaned_casrn")
    mat = mat[ , !(names(mat) %in% cremove)]
    mat = unique(mat)
    cat(src,nrow(mat),"\n")
    file = paste0(dir,"/toxval_all_",toxval.db,"_",src,"_for_release.xlsx")
    sty = createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
    openxlsx::write.xlsx(mat,file,firstRow=T,headerStyle=sty)
  }
}
