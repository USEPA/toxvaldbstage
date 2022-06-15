#-----------------------------------------------------------------------------------
#
#' Build a data frame of the data from toxval and export by source as a
#' series of xlsx files
#'
#' @param toxval.db Database version
#' @return for each source writes an Excel file with the name
#'  ../export/export_by_source_{data}/toxval_all_{toxval.db}_{source}.xlsx
#'
#-----------------------------------------------------------------------------------
export.all.by.source <- function(toxval.db) {
  printCurrentFunction(toxval.db)

  dir = paste0(toxval.config()$datapath,"export/export_by_source_",Sys.Date())
  if(!dir.exists(dir)) dir.create(dir)

  slist = sort(runQuery("select distinct source from toxval",toxval.db)[,1])
  nlist = c("source","rows","dtxsid","toxval_type","toxval_units","rac","study_type",
            "study_duration_class","study_duration_units","species","strain","sex",
            "human_eco","exposure_route","exposure_method","exposure_form","media",
            "critical_effect","long_ref","","")
  res = as.data.frame(matrix(nrow=length(slist),ncol=length(nlist)))
  names(res) = nlist
  res$source = slist
  rownames(res) = res$source
   file = paste0(dir,"/source_counts ",Sys.Date(),".xlsx")
   openxlsx::write.xlsx(res,file)
   browser()
  for(src in slist) {
    query = paste0("SELECT
                    a.dtxsid,a.casrn,a.name,b.toxval_id,
                    b.source,b.subsource,
                    b.toxval_type,b.toxval_type_original,
                    b.toxval_subtype,
                    e.toxval_type_supercategory,
                    e.toxval_type_category,
                    b.toxval_numeric,b.toxval_numeric_original,
                    b.toxval_units,b.toxval_units_original,
                    b.toxval_numeric_standard,b.toxval_numeric_standard_units,
                    b.toxval_numeric_human,b.toxval_numeric_human_units,
                    b.risk_assessment_class,
                    b.study_type,b.study_type_original,
                    b.study_duration_class,b.study_duration_class_original,
                    b.study_duration_value,b.study_duration_value_original,
                    b.study_duration_units,b.study_duration_units_original,
                    b.species_id,b.species_original,d.common_name,d.latin_name,d.ecotox_group,d.habitat,
                    b.human_eco,
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
                    b.qa_status,
                    b.quality_id,b.priority_id,
                    b.source_source_id,
                    b.details_text,
                    b.toxval_uuid,
                    b.toxval_hash,
                    b.datestamp,
                    f.long_ref,
                    f.title,
                    f.author,
                    f.journal,
                    f.volume,
                    f.year,
                    f.issue,
                    f.url,
                    f.document_name,
                    f.record_source_type,
                    f.record_source_hash
                    FROM
                    toxval b
                    INNER JOIN chemical a on a.dtxsid=b.dtxsid
                    LEFT JOIN species_ecotox d on b.species_id=d.species_id
                    INNER JOIN toxval_type_dictionary e on b.toxval_type=e.toxval_type
                    INNER JOIN record_source f on b.toxval_id=f.toxval_id
                    WHERE
                    b.toxval_numeric>0
                    and b.source='",src,"'")
    mat = runQuery(query,toxval.db,T,F)
    mat = unique(mat)
    cat(src,nrow(mat),"\n")

    cat("missing data ...\n")
    n1 = runQuery("select count(*) from toxval where toxval_id not in (select toxval_id from record_source)",toxval.db)[,1]
    cat("missing from record_source:",n1,"\n")
    n2 = runQuery("select count(*) from toxval where toxval_type not in (select toxval_type from toxval_dictionary)",toxval.db)[1,]
    cat("missing from toxval_type_dictionary:",n2,"\n")
    n3 = runQuery("select count(*) from toxval where dtxsid not in (select dtxsid from chemical)",toxval.db)[1,]
    cat("missing from chemical:",n2,"\n")

    col.list = names(mat)
    exclude.list = c("toxval_id","toxval_numeric","toxval_numeric_original","toxval_numeric_converted",
                      "study_duration_value","species_id","quality_id","priority_id","study_id",
                     "source_source_id","chemical_id","qa_status","toxval_numeric_standard","toxval_numeric_human")
    col.list = col.list[!is.element(col.list,exclude.list)]

    col.list = col.list[is.element(col.list,names(mat))]
    # for(col in col.list) {
    #   cat(col,"\n")
    #   x = mat[,col]
    #   y = enc2utf8(x)
    #   z = 1-(x==y)
    #   z[is.na(z)] = 0
    #   mask = x
    #   mask[] = 0
    #   cat("column:",col,":",sum(z),"\n")
    #   if(sum(z)>0) {
    #     xx = x[z==1]
    #     yy = y[z==1]
    #   }
    #   y[z==1] = "BAD STRING"
    #   mat[,col] = y
    # }
    cat(src,nrow(mat),"\n")
    res[src,"rows"] = nrow(mat)
    file = paste0(dir,"/toxval_all_",toxval.db,"_",src,".xlsx")
    sty = createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
    openxlsx::write.xlsx(mat,file,firstRow=T,headerStyle=sty)
  }
  file = paste0(dir,"/source_counts ",Sys.Date(),".xlsx")
  openxlsx::write.xlsx(res,file)
}
