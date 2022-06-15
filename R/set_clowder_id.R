#--------------------------------------------------------------------------------------
#' Set the clowder_id and document_name in res
#' @param res The input dataframe
#' @param source The data source name
#' @return Returns the input dataframe with defaults set
#--------------------------------------------------------------------------------------
set_clowder_id <- function(res,source) {
  printCurrentFunction(source)

  # Set the easy ones
  if(source=="Alaska DEC") {
    res$clowder_id = "610038e1e4b01a90a3f9ae63"
    res$document_name = "53dec438dd4a7efab7ca19ffd32e9e45-Alaska Department of Environmental Conservation-2008-Clean-up L.pdf"
    cat("clowder_id and document_name set for ",source,"\n")
    return(res)
  }
  if(source=="ATSDR MRLs 2020") {
    res$clowder_id = "610036c7e4b01a90a3f9879c"
    res$document_name = "a6a427952aa24d5d9e1d1a229109ba7e-Agency for Toxic Substances and Disease Registry-2020.pdf"
    cat("clowder_id and document_name set for ",source,"\n")
    return(res)
  }
  if(source=="ATSDR PFAS") {
    res$clowder_id = "6238e943e4b0b18cb57ced5a"
    res$document_name = "tp200-c2.pdf"
    cat("clowder_id and document_name set for ",source,"\n")
    return(res)
  }
  if(source=="ATSDR PFAS 2021") {
    res$clowder_id = "6238b97ae4b0b18cb57ce4f6"
    res$document_name = "tp200.pdf"
    cat("clowder_id and document_name set for ",source,"\n")
    return(res)
  }
  if(source=="Chiu") {
    res$clowder_id = "61003953e4b01a90a3f9b6d1"
    res$document_name = "08b44893c3ec2ed2c917bd2962aefca2-Chiu-2018-Beyond the.pdf"
    cat("clowder_id and document_name set for ",source,"\n")
    return(res)
  }
  if(is.element(source,c("DOD"))) {
    res$clowder_id = "61003ab1e4b01a90a3f9ce11"
    res$document_name = "3606a83ea4293730c355bceca0900d9c-Anonymous-2013-Technical .pdf"
    cat("clowder_id and document_name set for ",source,"\n")
    return(res)
  }
  if(is.element(source,c("DOE Wildlife Benchmarks"))) {
    res$clowder_id = "61003756e4b01a90a3f99471"
    res$document_name = "ee2b2fc62d5fa7a1e025dd6a119bdd16-Sample-1996-Toxicologi.pdf"
    cat("clowder_id and document_name set for ",source,"\n")
    return(res)
  }
  if(source=="EnviroTox_v2") {
    res$clowder_id = "61f14c70e4b0ebacf2ec476c"
    res$document_name = "Connors_2019a.pdf"
    cat("clowder_id and document_name set for ",source,"\n")
    return(res)
  }
  if(is.element(source,c("EPA AEGL"))) {
    res$clowder_id = "61003a84e4b01a90a3f9ca2f"
    res$document_name = "4627c891c8ea494fb8ea7846b220bd14-United States Environmental Protection Agency (USEPA)-2020-Acute Expo.pdf"
    cat("clowder_id and document_name set for ",source,"\n")
    return(res)
  }
  if(is.element(source,c("EPA OPP"))) {
    res$clowder_id = "610038d7e4b01a90a3f9ada1"
    res$document_name = "44b5bf69be26a74522e947eea7a44e4c-United States Environmental Protection Agency (USEPA)-2017-Huma.pdf"
    cat("clowder_id and document_name set for ",source,"\n")
    return(res)
  }
  if(is.element(source,c("Health Canada"))) {
    res$clowder_id = "61003a57e4b01a90a3f9c305"
    res$document_name = "60683b9de75ea6aced60e004a919370b-Health Canada-2010-Part II H.pdf"
    cat("clowder_id and document_name set for ",source,"\n")
    return(res)
  }
  if(is.element(source,c("NIOSH"))) {
    res$clowder_id="61fac10de4b04a563fdc9c88"
    res$document_name="ToxValQA33091630_NIOSH_2020_ImmediatelyDangerous-(IDLH)Values.pdf"
    return(res)
    cat("clowder_id and document_name set for ",source,"\n")
    return(res)
  }
  if(is.element(source,c("OW Drinking Water Standards"))) {
    res$clowder_id = "610036ede4b01a90a3f98ae0"
    res$document_name = "b5ffe2b7e16578b78213213141cfc3ad-United States Environmental Protection Agency (USEPA)-2018-2018 Drink.pdf"
    cat("clowder_id and document_name set for ",source,"\n")
    return(res)
  }
  if(source=="Pennsylvania DEP MCLs") {
    res$clowder_id = "61003849e4b01a90a3f9a409"
    res$document_name = "3d84a7d7e81b35b4e5ac9b820558be27-Pennsylvania Department of Environmental Protection (DEP)-2012.pdf "
    cat("clowder_id and document_name set for ",source,"\n")
    return(res)
  }
  if(source=="Pennsylvania DEP ToxValues") {
    res$clowder_id = "61003849e4b01a90a3f9a409"
    res$document_name = "3d84a7d7e81b35b4e5ac9b820558be27-Pennsylvania Department of Environmental Protection (DEP)-2012.pdf "
    cat("clowder_id and document_name set for ",source,"\n")
    return(res)
  }
  if(source=="RSL") {
    res[,"clowder_id"] = "61fac10ee4b04a563fdc9c92"
    res[,"document_name"] = "ToxValQA33099727_EPA_2021_RegionalScreeningLevels-(TR=1E-06,THQ=1.0).pdf"
    res[res$risk_assessment_class=="subchronic","clowder_id"] = "61fac10fe4b04a563fdc9c9c"
    res[res$risk_assessment_class=="subchronic","document_name"] = "ToxValQA33106078_EPA_2021_RegionalScreeningLevels-SubchronicToxicityValues.pdf"
    res[res$toxval_subtype=="Thq =  1","clowder_id"] = "61fac10ee4b04a563fdc9c92"
    res[res$toxval_subtype=="Thq =  1","document_name"] = "ToxValQA33099727_EPA_2021_RegionalScreeningLevels-(TR=1E-06,THQ=1.0).pdf"
    res[res$toxval_subtype=="Thq =  0.1","clowder_id"] = "61fac10ee4b04a563fdc9c97"
    res[res$toxval_subtype=="Thq =  0.1","document_name"] = "ToxValQA33100390_EPA_2021_RegionalScreeningLevels-(TR=1E-06,THQ=0.1).pdf"
    cat("clowder_id and document_name set for ",source,"\n")
    return(res)
  }
  if(source=="USGS HBSL") {
    res$clowder_id = "61fac110e4b04a563fdc9ca1"
    res$document_name = "ToxValQA29186291_USGS_2018_Health-BasedScreening-Water-QualityData.pdf"
    cat("clowder_id and document_name set for ",source,"\n")
    return(res)
  }
  if(source=="WHO IPCS") {
    res$clowder_id = "61bb6895e4b0d5d4d1e36956"
    res$document_name = "FAOWHO_2008_Pesticideresidues-Report2008.pdf"
    cat("clowder_id and document_name set for ",source,"\n")
    return(res)
  }
  if(is.element(source,c("OSHA Air Contaminants"))) {
    res$clowder_id="61fac10de4b04a563fdc9c8d"
    res$document_name="ToxValQA29180809_OSHA_TABLEZ-1LimitsforAirContaminants.pdf"
    cat("clowder_id and document_name set for ",source,"\n")
    return(res)
  }

  file = "../clowder_v3/toxval_document_map_icf.xlsx"
  map.icf = openxlsx::read.xlsx(file)

  file = "../clowder_v3/toxval_document_map_ccte.xlsx"
  map.ccte = openxlsx::read.xlsx(file)

  map.icf = fix.non_ascii.v2(map.icf,"map.icf")
  map.ccte = fix.non_ascii.v2(map.ccte,"map.ccte")
  nlist = names(res)
  if(!is.element("clowder_id",nlist)) res$clowder_id = "-"
  if(!is.element("document_name",nlist)) res$document_name = "-"

  # See if the source has records from the CCTE document mapping process
  ccte = F
  if(is.element(source,map.ccte$source)) ccte=T
  if(is.element(source,map.ccte$subsource)) ccte=T
  if(ccte) {
    map = map.ccte
    if(is.element(source,c("HAWC PFAS 150","HAWC PFAS 430"))) {
      title2 = res$title
      title2 = str_replace(title2,"Registration dossier: ","")
      title2 = gsub('\\.$','',title2)
      res$title2 = title2
      res$title2 = tolower(str_trim(title2))
      for(i in 1:nrow(map)) {
        title = str_trim(tolower(map[i,"study_name"]))
        cid = map[i,"clowder_id"]
        docname = map[i,"document_name"]
        res[is.element(res$title2,title),"clowder_id"] = cid
        res[is.element(res$title2,title),"document_name"] = docname
      }
      n1 = nrow(res)
      n2 = nrow(res[res$clowder_id!="-",])
      res2 = res[res$clowder_id=="-",]
      n3 = length(unique(res2$title))
      cat("matching for source",source,":",n2," out of ",n1," missing unique documents:",n3,"\n")
      browser()
      nlist = names(res)
      nlist = nlist[!is.element(nlist,"title2")]
      res = res[,nlist]
      return(res)
    }
    else if(is.element(source,c("Chiu"))) {
      lrtab = unique(runQuery("select long_ref, title from record_source where source='Chiu'","dev_toxval_v8"))
      lrtab$title = tolower(lrtab$title)
      for(i in 1:nrow(map)) {
        title = str_trim(tolower(map[i,"study_name"]))
        cid = map[i,"clowder_id"]
        docname = map[i,"document_name"]
        if(is.element(title,lrtab$title)) {
          longref = lrtab[is.element(lrtab$title,title),"long_ref"]
          res[is.element(res$long_ref,longref),"clowder_id"] = clowder_id
          res[is.element(res$long_ref,longref),"document_name"] = docname
        }
      }
      n1 = nrow(res)
      n2 = nrow(res[res$clowder_id!="-",])
      res2 = res[res$clowder_id=="-",]
      n3 = length(unique(res2$long_ref))
      cat("matching for source",source,":",n2," out of ",n1," missing unique documents:",n3,"\n")
      browser()
      nlist = names(res)
      nlist = nlist[!is.element(nlist,"title2")]
      res = res[,nlist]
      return(res)
    }
  }
  else {
    cat("try the v8 records\n")
    browser()
    return(res)
  }
}
