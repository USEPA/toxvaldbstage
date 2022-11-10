#--------------------------------------------------------------------------------------
#' Load PFAS 150 SEM V2 Source data into toxval_source
#' @param db The version of toxval_source into which the source info is loaded.
#' @param chem.check.halt If TRUE and there are problems with chemicals CASRN checks, halt the program
#--------------------------------------------------------------------------------------
import_pfas_150_sem_v2_source <- function(db,
                                          chem.check.halt=F) {
  printCurrentFunction(db)
  indir = paste0(toxval.config()$datapath,"pfas_150_sem_v2/PFAS 150 SEM v2_files/")

  file = paste0(indir,"PFAS 150 SEM chemicals.xlsx")
  chems = read.xlsx(file)
  file = paste0(indir,"PFAS 150 SEM results.xlsx")
  res = read.xlsx(file)
  file = paste0(indir,"PFAS 150 SEM HERO ID vs citation.xlsx")
  hero = read.xlsx(file)

  res[is.na(res$hero_id),"hero_id"] = -1
  res$long_ref = res$citation
  res$casrn = NA
  res$dtxsid = NA
  hlist = unique(res$hero_id)
  hlist = hlist[!is.na(hlist)]
  hlist = hlist[hlist>0]
  for(hero_id in hlist) {
    if(is.element(hero_id,hero$HERO.ID)) cit = hero[hero$HERO.ID==hero_id,"Citation"]
    else cit = unique(res[is.element(res$hero_id,hero_id),"citation"])[1]
    res[is.element(res$hero_id,hero_id),"long_ref"] = cit
  }
  clist = unique(res$chemical_name)
  for(name in clist) {
    if(is.element(name,chems$name)) {
      dtxsid = chems[is.element(chems$name,name),"dtxsid"]
      casrn = chems[is.element(chems$name,name),"casrn"]
      res[is.element(res$chemical_name,name),"dtxsid"] = dtxsid
      res[is.element(res$chemical_name,name),"casrn"] = casrn
      #cat(casrn,dtxsid,name,"\n")
    }
    else {
      cat("missing name:[",name,"]\n")
      browser()
    }
  }

  #####################################################################
  cat("pull the different PODs apart \n")
  #####################################################################
  names(res)[is.element(names(res),"chemical_name")] = "name"
  cenames2 = c("endpoint_system","endpoint_organ","endpoint","system_descriptor")
  res$critical_effect = NA
  res.ce = res[,cenames2]
  x = as.data.frame(do.call(paste,c(res.ce,sep=":")))
  res$critical_effect = x[,1]

  nlist1=c("dtxsid","casrn","name",
           "species","strain","sex","generation",
           "exposure_route","exposure_method",
           "study_type","study_duration_value","study_duration_units",
           "toxval_type_1_system","toxval_numeric_1_system","toxval_units_1_system","critical_effect","hero_id","citation")

  nlist2=c("dtxsid","casrn","name",
           "species","strain","sex","generation",
           "exposure_route","exposure_method",
           "study_type","study_duration_value","study_duration_units",
           "toxval_type_2_system","toxval_numeric_2_system","toxval_units_2_system","critical_effect","hero_id","citation")

  nlist3=c("dtxsid","casrn","name",
           "species","strain","sex","generation",
           "exposure_route","exposure_method",
           "study_type","study_duration_value","study_duration_units",
           "toxval_type_1_study","toxval_numeric_1_study","toxval_units_1_study","critical_effect","hero_id","citation")

  nlist4=c("dtxsid","casrn","name",
           "species","strain","sex","generation",
           "exposure_route","exposure_method",
           "study_type","study_duration_value","study_duration_units",
           "toxval_type_2_study","toxval_numeric_2_study","toxval_units_2_study","critical_effect","hero_id","citation")

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
          "toxval_type","toxval_numeric","toxval_units","critical_effect","hero_id","citation","toxval_subtype")
  names(res1) = nlist
  names(res2) = nlist
  names(res3) = nlist
  names(res4) = nlist
  res = rbind(res1,res2,res3,res4)
  res = res[!is.na(res$toxval_numeric),]
  #####################################################################
  cat("Collapse duplicated that just differ by critical effect \n")
  #####################################################################
  res2 = res[,!names(res)%in%c("critical_effect")]

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

  nlist.final=c("dtxsid","casrn","name",
                "toxval_type","toxval_subtype","toxval_numeric","toxval_units",
                "species","strain","sex","generation",
                "exposure_route","exposure_method",
                "study_type","study_duration_value","study_duration_units",
                "hero_id","citation","critical_effect")
  res = res2[,nlist.final]

  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  #names(res)[names(res)=="chemical_name"] = "name"
  res$long_ref = res$citation
  chems = unique(res[,c("dtxsid","casrn","name")])
  chems$raw_casrn = chems$casrn
  chems$cleaned_casrn = chems$casrn
  chems$raw_name = chems$name
  chems$cleaned_name = chems$name
  chems$quality = "Pass from source"
  res = res[ , !(names(res) %in% c("dtxsid"))]
  source_prep_and_load(db,source="PFAS 150 SEM v2",table="source_pfas_150_sem_v2",res=res,F,T,T)
}

