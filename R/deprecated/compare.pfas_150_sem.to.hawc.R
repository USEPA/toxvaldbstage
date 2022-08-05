#--------------------------------------------------------------------------------------
#' Compare teh PFAS 150 SEM and hte corresponding HAWC data
#' @param db The version of toxval_source into which the source info is loaded.
#--------------------------------------------------------------------------------------
compare.pfas_150_sem.to.hawc <- function(db) {
  printCurrentFunction(db)

  nlist.final=c("dtxsid","casrn","name",
                "toxval_type","toxval_subtype","toxval_numeric","toxval_units",
                "species","strain","sex","generation",
                "exposure_route","exposure_method",
                "study_type","study_duration_value","study_duration_units",
                "critical_effect")

  #####################################################################
  cat("Get HAWC \n")
  #####################################################################
  query = "select b.dtxsid,b.casrn,b.name,
            a.toxval_type,a.toxval_numeric,a.toxval_units,
            a.species,a.strain,a.sex,a.generation,
            a.exposure_route,a.exposure_method,
            a.study_type,a.study_duration_value,a.study_duration_units,
            a.critical_effect
            from source_hawc_pfas_150 a,source_chemical b
            where a.chemical_id=b.chemical_id"

  reshawc = runQuery(query,db)
  reshawc$toxval_subtype = NA
  reshawc = reshawc[,nlist.final]

  #####################################################################
  cat("Get PFAS 150 SEM \n")
  #####################################################################
  indir = paste0(toxval.config()$datapath,"PFAS 150 SEM v2/PFAS 150 SEM v2_files/")

  file = paste0(indir,"PFAS 150 SEM chemicals.xlsx")
  chems = read.xlsx(file)
  file = paste0(indir,"PFAS 150 SEM results.xlsx")
  res = read.xlsx(file)
  names(res)[is.element(names(res),"chemical_name")] = "name"
  res$casrn = NA
  res$dtxsid = NA
  nlist = unique(res$name)
  for(name in nlist) {
    dtxsid = chems[is.element(chems$name,name),"dtxsid"]
    casrn = chems[is.element(chems$name,name),"casrn"]
    res[res$name==name,"dtxsid"] = dtxsid
    res[res$name==name,"casrn"] = casrn
  }

  cenames2 = c("endpoint_system","endpoint_organ","endpoint","system_descriptor")
  res$critical_effect = NA
  res.ce = res[,cenames2]
  x = as.data.frame(do.call(paste,c(res.ce,sep=":")))
  res$critical_effect = x[,1]

  nlist1=c("dtxsid","casrn","name",
           "species","strain","sex","generation",
           "exposure_route","exposure_method",
           "study_type","study_duration_value","study_duration_units",
           "toxval_type_1_system","toxval_numeric_1_system","toxval_units_1_system","critical_effect")

  nlist2=c("dtxsid","casrn","name",
           "species","strain","sex","generation",
           "exposure_route","exposure_method",
           "study_type","study_duration_value","study_duration_units",
           "toxval_type_2_system","toxval_numeric_2_system","toxval_units_2_system","critical_effect")

  nlist3=c("dtxsid","casrn","name",
           "species","strain","sex","generation",
           "exposure_route","exposure_method",
           "study_type","study_duration_value","study_duration_units",
           "toxval_type_1_study","toxval_numeric_1_study","toxval_units_1_study","critical_effect")

  nlist4=c("dtxsid","casrn","name",
           "species","strain","sex","generation",
           "exposure_route","exposure_method",
           "study_type","study_duration_value","study_duration_units",
           "toxval_type_2_study","toxval_numeric_2_study","toxval_units_2_study","critical_effect")

  nlist = names(res)


  res1 = res[,nlist1]
  res2 = res[,nlist2]
  res3 = res[,nlist3]
  res4 = res[,nlist4]
  res1$toxval_subtype = "System-level NOAEL"
  res2$toxval_subtype = "System-level LOAEL"
  res3$toxval_subtype = "Study-level NOAEL"
  res4$toxval_subtype = "Study-level LOAEL"

  nlist=c("dtxsid","casrn","name",
          "species","strain","sex","generation",
          "exposure_route","exposure_method",
          "study_type","study_duration_value","study_duration_units",
          "toxval_type","toxval_numeric","toxval_units","critical_effect","toxval_subtype")
  names(res1) = nlist
  names(res2) = nlist
  names(res3) = nlist
  names(res4) = nlist
  ressem = rbind(res1,res2,res3,res4)

  ressem = ressem[!is.na(ressem$toxval_numeric),]

  #####################################################################
  cat("Collapse duplicated that just differ by critical effect \n")
  #####################################################################
  res = ressem
  res2 = ressem[,!names(ressem)%in%c("critical_effect")]

  res2$hashkey = NA
  for(i in 1:nrow(res2)) {
    hashkey = digest(paste0(res2[i,],collapse=""), serialize = FALSE)
    res2[i,"hashkey"] = hashkey
    res[i,"hashkey"] = hashkey
  }
  res2 = unique(res2)
  res2$critical_effect = NA
  for(i in 1:nrow(res2)) {
    hashkey = res2[i,"hashkey"]
    res3 = res[res$hashkey==hashkey,]
    y = unique(sort(res3$critical_effect))
    ce = paste0(y,collapse="|")
    res2[i,"critical_effect"] = ce
  }
  res2 = res2[,!names(res2)%in%c("hashkey")]
  ressem.unique = res2
  ressem = ressem.unique[,nlist.final]

  dlist = unique(ressem$dtxsid)
  for(dtxsid in dlist) {
    if(is.element(dtxsid,reshawc$dtxsid)) {
      name = unique(reshawc[reshawc$dtxsid==dtxsid,"name"])
      ressem[ressem$dtxsid==dtxsid,"name"] = name
    }
  }

  reshawc$source = "HAWC PFAS 150"
  ressem$source = "PFAS 150 SEM"
  res = rbind(ressem,reshawc)
  res[is.element(res$sex,"Female"),"sex"] = "F"
  res[is.element(res$sex,"Male"),"sex"] = "M"
  res[is.element(res$sex,"Combined"),"sex"] = "M/F"

  res[is.element(res$study_type,"Chronic (>90 days)"),"study_type"] = "chronic"
  res[is.element(res$study_type,"Developmental"),"study_type"] = "developmental"
  res[is.element(res$study_type,"Short-term (1-30 days)"),"study_type"] = "short-term"
  res[is.element(res$study_type,"Subchronic (30-90 days)"),"study_type"] = "subchronic"
  res[is.element(res$study_type,"Reproductive"),"study_type"] = "reproductive"

  res[is.element(res$exposure_route,"Oral gavage"),"exposure_route"] = "oral"
  res[is.element(res$exposure_method,"Oral gavage"),"exposure_method"] = "gavage"
  res[is.element(res$exposure_route,"Oral diet"),"exposure_route"] = "oral"
  res[is.element(res$exposure_method,"Oral diet"),"exposure_method"] = "diet"

  res[is.element(res$exposure_route,"Inhalation - vapor"),"exposure_route"] = "inhalation"
  res[is.element(res$exposure_method,"Inhalation - vapor"),"exposure_method"] = "vapor"

  res[is.element(res,"unknown")] = "-"
  file = "../pfas/PFAS_HAWC_SEM_compare.xlsx"
  res$species = tolower(res$species)
  write.xlsx(res,file)


}

