#-----------------------------------------------------------------------------------
#
#' Build a data frame of the PODs and exports as xlsx
#'
#' @param toxval.db Database version
#' @param human_eco Either 'human health' or 'eco'
#' @param file.name If not NA, this is a file containing chemicals, and only those chemicals will be exported
#'
#'
#' @return writes an Excel file with the name
#'  ../export/toxval_pod_summary_[human_eco]_Sys.Date().xlsx
#'
#-----------------------------------------------------------------------------------
export.all.with.references <- function(toxval.db,dir="../export/",file.name=NA) {
  printCurrentFunction(toxval.db)

  query <- paste0("SELECT
                  a.dtxsid,a.casrn,a.name,b.toxval_id,
                  b.source,b.subsource,
                  b.toxval_type,b.toxval_type_original,
                  b.toxval_subtype,
                  e.toxval_type_supercategory,
                  e.toxval_type_category,
                  b.toxval_numeric,b.toxval_numeric_original,
                  b.toxval_units,b.toxval_units_original,
                  b.risk_assessment_class,
                  b.study_type,b.study_type_original,
                  b.study_duration_class,b.study_duration_class_original,
                  b.study_duration_value,b.study_duration_value_original,
                  b.study_duration_units,b.study_duration_units_original,
                  b.species_id,b.species_original,d.species_common,d.species_scientific,d.habitat,
                  b.human_eco,
                  b.strain,b.strain_original,
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
                  LEFT JOIN species d on b.species_id=d.species_id
                  INNER JOIN toxval_type_dictionary e on b.toxval_type=e.toxval_type
                  INNER JOIN record_source f on b.toxval_id=f.toxval_id
                  WHERE
                  b.toxval_numeric>0
                  and b.qa_status=1")

  #and b.source!='ECOTOX'
  #and b.source not like 'Alaska%'
  #and b.source not like 'ATSDR%'
  #and b.source not like 'Cal O%'
  #and b.source not like 'Califor%'
  #and b.source not like 'Chiu%'
  #and b.source not like 'Copper%'
  #and b.source not like 'COSMOS%'
  #and b.source not like 'DOD%'
  #and b.source not like 'DOE%'
  #and b.source not like 'EFSA%'
  #and b.source not like 'Enviro%'
  #and b.source not like 'EPA AE%'
  #and b.source not like 'EPA OPP'
  #and b.source not like 'EPA OPPT'
  #and b.source not like 'FDA%'
  #and b.source not like 'HAWC%'
  #and b.source not like 'Health%'
  #and b.source not like 'HEAST%'
  #and b.source not like 'HESS%'
  #and b.source not like 'HPVIS%'
  #and b.source not like 'IRIS%'
  #and b.source not like 'Mass%'
  #and b.source not like 'NIOSH%'
  #and b.source not like 'OSHA%'
  #and b.source not like 'OW %'
  #and b.source not like 'Penn%'
  #and b.source not like 'PFAS%'
  #and b.source not like 'PPRTV%'
  #and b.source not like 'RSL%'
  #and b.source not like 'TEST%'
  #and b.source not like 'ToxRefDB%'
  #and b.source not like 'USGS%'
  #and b.source not like 'WHO%'
  #and b.source not like 'Wig%'
  #and b.source not like 'ECHA%'
  #and e.toxval_type_supercategory in ('Toxicity Value')
  #and species_original != '-'
  #limit 1000
  #and a.dtxsid='DTXSID1026241'
  mat <- runQuery(query,toxval.db,T,F)
  mat <- unique(mat)
  print(dim(mat))

  cat("missing data ...\n")
  n1 = runQuery("select count(*) from toxval where toxval_id not in (select toxval_id from record_source)",toxval.db)[,1]
  cat("missing from record_source:",n1,"\n")
  n2 = runQuery("select count(*) from toxval where toxval_type not in (select toxval_type from toxval_dictionary)",toxval.db)[1,]
  cat("missing from toxval_type_dictionary:",n2,"\n")
  n3 = runQuery("select count(*) from toxval where dtxsid not in (select dtxsid from chemical)",toxval.db)[1,]
  cat("missing from chemical:",n2,"\n")

  col.list <- names(mat)
  exclude.list <- c("toxval_id","toxval_numeric","toxval_numeric_original","toxval_numeric_converted",
                    "study_duration_value","species_id","quality_id","priority_id","study_id","source_source_id","chemical_id","qa_status")
  col.list <- col.list[!is.element(col.list,exclude.list)]

  col.list <- col.list[is.element(col.list,names(mat))]
  for(col in col.list) {
    cat(col,"\n")
    if(col=="qa_status") browser()
    x <- mat[,col]
    y <- enc2utf8(x)
    z <- 1-(x==y)
    z[is.na(z)] <- 0
    mask <- x
    mask[] <- 0
    cat("column:",col,":",sum(z),"\n")
    if(sum(z)>0) {
      xx <- x[z==1]
      yy <- y[z==1]
    }
    y[z==1] <- "BAD STRING"
    mat[,col] <- y
  }
  print(dim(mat))
  nrow <- dim(mat)[1]
  if(!is.na(file.name)) {
    file <- paste0("../chemicals/for_load/",file.name,".xlsx")
    print(file)
    chems <- read.xlsx(file)
    casrn.list <- unique(chems[,"casrn"])
    mat <- mat[is.element(mat[,"casrn"],casrn.list),]
    print(dim(mat))
  }
  file <- paste0(dir,"/toxval_all_with_references_",toxval.db,"_",Sys.Date(),".xlsx")
  if(!is.na(file.name))
    file <- paste0(dir,"/toxval_all_with_references_",file.name,"_",toxval.db,"_",Sys.Date(),".xlsx")
  sty <- createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  write.xlsx(mat,file,firstRow=T,headerStyle=sty)

  #file <- paste0(dir,"/toxval_all_with_references_",toxval.db,"_",Sys.Date(),".csv")
  #if(!is.na(file.name))
  #  file <- paste0(dir,"/toxval_all_with_references_",file.name,"_",toxval.db,"_",Sys.Date(),".csv")
  #write.csv(mat,file,row.names=F)
}
