#-----------------------------------------------------------------------------------
#' Build a data frame of the data from toxval and export by source as a
#' series of xlsx files. THis is a version for PFAS specific use
#'
#' @param toxval.db Database version
#' @param source The source to be updated
#' @return Write a file with the results
#'
#-----------------------------------------------------------------------------------
export.all.by.source.pfas <- function(toxval.db) {
  printCurrentFunction(toxval.db)


  dir = paste0(toxval.config()$datapath,"pfas")
  file = paste0(dir,"/PFAS Universe fixed columns.xlsx")
  chems = read.xlsx(file)
  dlist = unique(chems$dtxsid)
  dlist = unique(dlist)
  cat("n(dtxsid)[1]:",length(dlist),"\n")

  file = paste0(dir,"/Big Consolidated PfAS Priority List_8_1_22.xlsx")
  priority = read.xlsx(file)
  dlist = c(dlist,priority$DTXSID)
  dlist = unique(dlist)
  cat("n(dtxsid)[2]:",length(dlist),"\n")

  file = paste0(dir,"/PFAS Struct4.xlsx")
  temp = read.xlsx(file)
  dlist = c(dlist,temp$DTXSID)
  dlist = unique(dlist)
  cat("n(dtxsid)[3]:",length(dlist),"\n")

  file = paste0(dir,"/PFAS chemicals.xlsx")
  temp = read.xlsx(file)
  dlist = c(dlist,temp$dtxsid)
  dlist = unique(dlist)
  cat("n(dtxsid)[3]:",length(dlist),"\n")

  dlist2 = c(dlist,runQuery("select dtxsid from source_chemical where source in ('HAWC PFAS 150','HAWC PFAS 430','PFAS 150 SEM v2','ATSDR PFAS','ATSDR PFAS 2021')",toxval.db)[,1])
  dlist2 = unique(dlist2)
  dlist = c(dlist,dlist2)
  dlist = unique(dlist)
  cat("n(dtxsid)[4]:",length(dlist),"\n")

  dlistt = runQuery("select distinct dtxsid from toxval",toxval.db)[,1]
  cat("n(dtxsid)[toxval]:",length(dlistt),"\n")

  dlist = dlist[is.element(dlist,dlistt)]
  cat("n(dtxsid)[5]:",length(dlist),"\n")

  dall = runQuery("select distinct source,dtxsid from toxval",toxval.db)
  slist = sort(unique(dall[is.element(dall$dtxsid,dlist),"source"]))

  res = NULL
  for(src in slist) {
    query = paste0("SELECT
                    a.dtxsid,a.casrn,a.name,
                    b.source,b.subsource,
                    b.qc_status,
                    b.risk_assessment_class,
                    b.toxval_type,
                    b.toxval_subtype,
                    e.toxval_type_supercategory,
                    b.toxval_numeric,b.toxval_units,
                    b.study_type,
                    b.study_duration_class,
                    b.study_duration_value,
                    b.study_duration_units,
                    d.common_name,
                    b.strain,b.strain_group,
                    b.sex,
                    b.generation,
                    b.exposure_route,
                    b.exposure_method,
                    b.critical_effect,
                    b.year,
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
                    INNER JOIN source_chemical a on a.chemical_id=b.chemical_id
                    LEFT JOIN species d on b.species_id=d.species_id
                    INNER JOIN toxval_type_dictionary e on b.toxval_type=e.toxval_type
                    INNER JOIN record_source f on b.toxval_id=f.toxval_id
                    WHERE
                    b.source='",src,"'
                    and human_eco='human health'
                    and toxval_type_supercategory in ('Point of Departure','Lethality Effect Level')
                    and qc_status='pass'")
    mat = runQuery(query,toxval.db,T,F)
    mat = unique(mat)
    mat[mat$casrn=='-',"casrn"] = mat[mat$casrn=='-',"cleaned_casrn"]
    mat[mat$name=='-',"name"] = mat[mat$name=='-',"cleaned_name"]

    cremove = c("cleaned_name","cleaned_casrn")
    mat = mat[ , !(names(mat) %in% cremove)]
    mat = mat[is.element(mat$dtxsid,dlist),]
    if(nrow(mat)>0) {
      nlist = c("dtxsid","source","subsource","risk_assessment_class","toxval_units","study_type","study_duration_class",
                "study_duration_value","study_duration_units","common_name","strain","sex","exposure_route",
                "exposure_method","year","long_ref","year.1","document_name","title","author","journal","volume","issue")
      temp = mat[,nlist]
      mat$hashkey = NA
      mat$study_group = NA
      for(i in 1:nrow(mat)) mat[i,"hashkey"] = digest(paste0(temp[i,],collapse=""), serialize = FALSE)
      hlist = unique(mat$hashkey)
      for(i in 1:length(hlist)) {
        sg = paste0(src,"_",i)
        mat[mat$hashkey==hlist[i],"study_group"] = sg
      }
      cat(src,nrow(mat),length(hlist),"\n")
      res = rbind(res,mat)
    }
  }
  res = subset(res,select= -c(hashkey,toxval_type_supercategory,qc_status))
  file = paste0(dir,"/toxval_all_for_pfas_",toxval.db," ",Sys.Date(),".xlsx")
  sty = createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  openxlsx::write.xlsx(res,file,firstRow=T,headerStyle=sty)


  priority = priority[,1:2]
  rlist = sort(unique(res$risk_assessment_class))
  nlist = c("dtxsid","name",rlist)
  mat = as.data.frame(matrix(nrow=nrow(priority),ncol=length(nlist)))
  names(mat) = nlist
  mat[,1:2] = priority[,1:2]
  for(i in 1:nrow(mat)) {
    dtxsid = mat[i,"dtxsid"]
    temp = res[res$dtxsid==dtxsid,]
    for(rac in rlist) {
      temp2 = temp[temp$risk_assessment_class==rac,]
      mat[i,rac] = nrow(temp2)
    }
  }
  file = paste0(dir,"/big consolidated pfas priority list toxvaldb counts ",Sys.Date(),".xlsx")
  write.xlsx(mat,file)
}
