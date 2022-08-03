#--------------------------------------------------------------------------------------
#' Load HEAST Source into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile The input file ./heast/heast_files/EPA_HEAST_Table1_ORNL for loading.xlsx
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#--------------------------------------------------------------------------------------
import_heast_source <- function(db,
                                infile="EPA_HEAST_Table1_ORNL for loading.xlsx",
                                chem.check.halt=T) {
  printCurrentFunction(db)
  infile = paste0(toxval.config()$datapath,"heast/heast_files/",infile)
  #####################################################################
  cat("Build original_heast_table \n")
  #####################################################################
  res0 <- openxlsx::read.xlsx(infile,1,colNames = T)
  res0 = res0[order(res0$row_id),]
  res0$keep = 0
  start = 0
  pointer = -1
  for(i in 1:nrow(res0)) {
    if(!is.na(res0[i,"name"])) {
      if(i>1) res0[pointer,"critical_effect"] = ce
      pointer = i
      start = 1
      ce = res0[pointer,"critical_effect"]
      res0[i,"keep"] = 1
    }
    else {
      ce = paste0(ce,"|",res0[i,"critical_effect"])
    }
  }
  #browser()
  res0 = res0[res0$keep==1,]
  cremove = c("keep")
  res0 = res0[ , !(names(res0) %in% cremove)]

  # for(i in 1:nrow(res0)) {
  #   if(is.na(res0[i,"name"])) res0[i,"name"] = res0[(i-1),"name"]
  # }
  # clist = unique(res0$name)
  # res = NULL
  # for(chem in clist) {
  #   temp = res0[is.element(res0$name,chem),]
  #   temp1 = temp[1,]
  #   ce = NULL
  #   for(i in 1:nrow(temp)) ce = c(ce,paste0(temp[i,"target"],":",temp[i,"critical_effect"]))
  #   temp1[1,"critical_effect"] = paste(ce,collapse="|")
  #   res = rbind(res,temp1)
  # }
  res = res0
  nlist1 = c(
  "row_id","name","casrn",
  "species","exposure_route","exposure_method","study_duration_value","study_duration_units","study_duration_class",
  "critical_effect","comment","ornl_table",
  "toxval_type","toxval_numeric","toxval_units"
  )
  res1 = res[,nlist1]
  res1$uf = NA
  nlist = names(res1)

  nlist2 = c(
    "row_id","name","casrn",
    "species","exposure_route","exposure_method","study_duration_value","study_duration_units","study_duration_class",
    "critical_effect","comment","ornl_table",
    "toxval_type","toxval_numeric","toxval_units",
    "rfc_subchronic","rfc_subchronic_units","rfc_subchronic_uf"
  )

  res2 = res[,nlist2]
  res2 = res2[!is.na(res2$rfc_subchronic),]
  res2$study_duration_value = NA
  res2$study_duration_units = NA
  res2$study_duration_class = "subchronic"
  res2$toxval_type = "RfC"
  res2$toxval_numeric = res2$rfc_subchronic
  res2$toxval_units = res2$rfc_subchronic_units
  res2$uf = res2$rfc_subchronic_uf
  res2 = res2[,nlist]

  nlist3 = c(
    "row_id","name","casrn",
    "species","exposure_route","exposure_method","study_duration_value","study_duration_units","study_duration_class",
    "critical_effect","comment","ornl_table",
    "toxval_type","toxval_numeric","toxval_units",
    "rfd_subchronic","rfd_subchronic_units","rfd_subchronic_uf"
  )
  res3 = res[,nlist3]
  res3 = res3[!is.na(res3$rfd_subchronic),]
  res3$study_duration_value = NA
  res3$study_duration_units = NA
  res3$study_duration_class = "subchronic"
  res3$toxval_type = "RfD"
  res3$toxval_numeric = res3$rfd_subchronic
  res3$toxval_units = res3$rfd_subchronic_units
  res3$uf = res3$rfd_subchronic_uf
  res3 = res3[,nlist]

  nlist4 = c(
    "row_id","name","casrn",
    "species","exposure_route","exposure_method","study_duration_value","study_duration_units","study_duration_class",
    "critical_effect","comment","ornl_table",
    "toxval_type","toxval_numeric","toxval_units",
    "rfc_chronic","rfc_chronic_units","rfc_chronic_uf"
  )
  res4 = res[,nlist4]
  res4 = res4[!is.na(res4$rfc_chronic),]
  res4$study_duration_value = NA
  res4$study_duration_units = NA
  res4$study_duration_class = "chronic"
  res4$toxval_type = "RfC"
  res4$toxval_numeric = res4$rfc_chronic
  res4$toxval_units = res4$rfc_chronic_units
  res4$uf = res4$rfc_chronic_uf
  res4 = res4[,nlist]

  nlist5 = c(
    "row_id","name","casrn",
    "species","exposure_route","exposure_method","study_duration_value","study_duration_units","study_duration_class",
    "critical_effect","comment","ornl_table",
    "toxval_type","toxval_numeric","toxval_units",
    "rfd_chronic","rfd_chronic_units","rfd_chronic_uf"
  )
  res5 = res[,nlist5]
  res5 = res5[!is.na(res5$rfd_chronic),]
  res5$study_duration_value = NA
  res5$study_duration_units = NA
  res5$study_duration_class = "chronic"
  res5$toxval_type = "RfD"
  res5$toxval_numeric = res5$rfd_chronic
  res5$toxval_units = res5$rfd_chronic_units
  res5$uf = res5$rfd_chronic_uf
  res5 = res5[,nlist]

  #browser()
  res = rbind(res1,res2,res3,res4,res5)
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="HEAST",table="source_heast",res=res,F,T,T)
}
