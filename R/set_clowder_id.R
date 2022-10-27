#--------------------------------------------------------------------------------------
#' Set the clowder_id and document_name in res
#' @param res The input dataframe
#' @param source The data source name
#' @param map_file A dataframe of Clowder document mapping info \code{see update.source.clowder.id.R}
#' @return Returns the input dataframe with defaults set
#--------------------------------------------------------------------------------------
set_clowder_id <- function(res,source, map_file=NULL) {
  printCurrentFunction(source)
  file = paste0(toxval.config()$datapath,"clowder_v3/toxval_document_map_icf.xlsx")
  map.icf = openxlsx::read.xlsx(file)

  file = paste0(toxval.config()$datapath,"clowder_v3/toxval_document_map_ccte.xlsx")
  map.ccte = openxlsx::read.xlsx(file)

  map.icf = fix.non_ascii.v2(map.icf,"map.icf")
  map.ccte = fix.non_ascii.v2(map.ccte,"map.ccte")
  if(source=="HESS") {
    for(dn in unique(res$document_name)) {
      if(is.element(dn,map.icf$document_name)) {
        cid = map.icf[is.element(map.icf$document_name,dn),"clowder_id"]
        res[is.element(res$document_name,dn),"clowder_id"] = cid
      }
    }
    cat("clowder_id and document_name set for ",source,"\n")
    return(res)
  }

  # Switch map for easy sources with 1 document
  easy_map = switch(source,
                    "Alaska DEC" = list(clowder_id = "610038e1e4b01a90a3f9ae63",
                                        document_name = "53dec438dd4a7efab7ca19ffd32e9e45-Alaska Department of Environmental Conservation-2008-Clean-up L.pdf"),
                    "ATSDR MRLs 2020" = list(clowder_id="610036c7e4b01a90a3f9879c",
                                             document_name="a6a427952aa24d5d9e1d1a229109ba7e-Agency for Toxic Substances and Disease Registry-2020.pdf"),
                    "ATSDR PFAS" = list(clowder_id = "6238e943e4b0b18cb57ced5a",
                                        document_name = "tp200-c2.pdf"),
                    "ATSDR PFAS 2021" = list(clowder_id = "6238b97ae4b0b18cb57ce4f6",
                                             document_name = "tp200.pdf"),
                    "Chiu" = list(clowder_id = "61003953e4b01a90a3f9b6d1",
                                  document_name = "08b44893c3ec2ed2c917bd2962aefca2-Chiu-2018-Beyond the.pdf"),
                    "DOD" = list(clowder_id = "61003ab1e4b01a90a3f9ce11",
                                 document_name = "3606a83ea4293730c355bceca0900d9c-Anonymous-2013-Technical .pdf"),
                    "DOE Wildlife Benchmarks" = list(clowder_id = "61003756e4b01a90a3f99471",
                                                     document_name = "ee2b2fc62d5fa7a1e025dd6a119bdd16-Sample-1996-Toxicologi.pdf"),
                    "EnviroTox_v2" = list(clowder_id = "61f14c70e4b0ebacf2ec476c",
                                          document_name = "Connors_2019a.pdf"),
                    "EPA AEGL" = list(clowder_id = "61003a84e4b01a90a3f9ca2f",
                                      document_name = "4627c891c8ea494fb8ea7846b220bd14-United States Environmental Protection Agency (USEPA)-2020-Acute Expo.pdf"),
                    "EPA OPP" = list(clowder_id = "610038d7e4b01a90a3f9ada1",
                                     document_name = "44b5bf69be26a74522e947eea7a44e4c-United States Environmental Protection Agency (USEPA)-2017-Huma.pdf"),
                    "Health Canada" = list(clowder_id = "61003a57e4b01a90a3f9c305",
                                           document_name = "60683b9de75ea6aced60e004a919370b-Health Canada-2010-Part II H.pdf"),
                    "NIOSH" = list(clowder_id = "61fabd3de4b04a563fdc9b99",
                                   document_name = "ToxValQA33091630_NIOSH_2020_ImmediatelyDangerous-(IDLH)Values.pdf"),
                    "OW Drinking Water Standards" = list(clowder_id = "610036ede4b01a90a3f98ae0",
                                                         document_name = "b5ffe2b7e16578b78213213141cfc3ad-United States Environmental Protection Agency (USEPA)-2018-2018 Drink.pdf"),
                    "Pennsylvania DEP MCLs" = list(clowder_id = "61003849e4b01a90a3f9a409",
                                                   document_name = "3d84a7d7e81b35b4e5ac9b820558be27-Pennsylvania Department of Environmental Protection (DEP)-2012.pdf "),
                    "Pennsylvania DEP ToxValues" = list(clowder_id = "61003849e4b01a90a3f9a409",
                                                        document_name = "3d84a7d7e81b35b4e5ac9b820558be27-Pennsylvania Department of Environmental Protection (DEP)-2012.pdf "),
                    "USGS HBSL" = list(clowder_id = "61fabdc3e4b04a563fdc9c0b",
                                       document_name = "ToxValQA29186291_USGS_2018_Health-BasedScreening-Water-QualityData.pdf"),
                    "WHO IPCS" = list(clowder_id = "61bb6895e4b0d5d4d1e36956",
                                      document_name = "FAOWHO_2008_Pesticideresidues-Report2008.pdf"),
                    "OSHA Air contaminants" = list(clowder_id = "61fabd47e4b04a563fdc9bb8",
                                                   document_name = "ToxValQA29180809_OSHA_TABLEZ-1LimitsforAirContaminants.pdf"),
                    "FDA CEDI" = list(clowder_id = "619d2972e4b0993a3937de4f",
                                      document_name = "ToxValQA29176904_FDA_CumulativeEstimatedDailyIntake.pdf"),
                    "Wignall" = list(clowder_id = "62b30a1ee4b07abf29f56811",
                                     document_name = "ToxValDBQA Wignall EHP 2014.pdf")
  )

  # Set the easy ones - NULL means no mapped source doc information
  if(!is.null(easy_map)){
    res$clowder_id = easy_map$clowder_id
    res$document_name = easy_map$document_name
    cat("clowder_id and document_name set for ",source,"\n")
    return(res)
  }

  if(source=="RSL") {
    res[,"clowder_id"] = "61fabdc0e4b04a563fdc9bdd"
    res[,"document_name"] = "ToxValQA33099727_EPA_2021_RegionalScreeningLevels-(TR=1E-06,THQ=1.0).pdf"
    res[res$risk_assessment_class=="subchronic","clowder_id"] = "61fabdc0e4b04a563fdc9be1"
    res[res$risk_assessment_class=="subchronic","document_name"] = "ToxValQA33106078_EPA_2021_RegionalScreeningLevels-SubchronicToxicityValues.pdf"
    res[res$toxval_subtype=="Thq =  1","clowder_id"] = "61fabdc0e4b04a563fdc9bdd"
    res[res$toxval_subtype=="Thq =  1","document_name"] = "ToxValQA33099727_EPA_2021_RegionalScreeningLevels-(TR=1E-06,THQ=1.0).pdf"
    res[res$toxval_subtype=="Thq =  0.1","clowder_id"] = "61fabdc0e4b04a563fdc9bdf"
    res[res$toxval_subtype=="Thq =  0.1","document_name"] = "ToxValQA33100390_EPA_2021_RegionalScreeningLevels-(TR=1E-06,THQ=0.1).pdf"
    cat("clowder_id and document_name set for ",source,"\n")
    return(res)
  }

  if(source %in% c("DOD ERED",
                          "HEAST",
                          "PFAS 150 SEM",
                          "Mass. Drinking Water Standards",
                          "California DPH",
                          "PFAS Summary PODs")) {
    cat("This is a source that needs to get fixed clowder_id code\n")
    res$clowder_id="-"
    res$document_name="-"
    cat("clowder_id and document_name set for ",source,"\n")
    return(res)
  }

  ##########################################################
  cat("Do the non-easy sources\n")
  ##########################################################

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
      res$title3 = gsub("\\s*\\([^\\)]+\\)", "", res$title2)
      for(i in 1:nrow(map)) {
        cid = map[i,"clowder_id"]
        docname = map[i,"document_name"]
        title2 = str_trim(tolower(map[i,"study_name"])) %>%
          gsub('\\.$','',.)
        # Remove trailing . information
        title2b = sub('\\..*', '', title2)
        # Remove parenthetical information and trailing . information
        title3 = gsub("\\s*\\([^\\)]+\\)", "", title2) %>%
          gsub('\\.$','',.)

        # Attempt various matching schemes
        if(any(is.element(res$title2,title2))){
          res[is.element(res$title2,title2),"clowder_id"] = cid
          res[is.element(res$title2,title2),"document_name"] = docname
        } else if(any(is.element(res$title3,title2))){
          res[is.element(res$title3,title2),"clowder_id"] = cid
          res[is.element(res$title3,title2),"document_name"] = docname
        } else if(any(is.element(res$title2,title2b))){
          res[is.element(res$title2,title2b),"clowder_id"] = cid
          res[is.element(res$title2,title2b),"document_name"] = docname
        } else if(any(grepl(title3, res$title3))){
          res[grepl(title3, res$title3),"clowder_id"] = cid
          res[grepl(title3, res$title3),"document_name"] = docname
        }
      }
      n1 = nrow(res)
      n2 = nrow(res[res$clowder_id!="-",])
      res2 = res[res$clowder_id=="-",]
      n3 = length(unique(res2$title))
      cat("matching for source",source,":",n2," out of ",n1," missing unique documents:",n3,"\n")
      # Remove title2
      res$title2 = NULL
      res$title3 = NULL
      return(res)
    }
  }

  # Match IRIS Clower ID's
  if (source == "IRIS") {
    # cut the map down to just the webpage PDF documents, not screenshots or supplements
    map_file <- map_file[which(map_file$parentPath == "IRIS"),]
    # Clear any old mappings
    res$clowder_id = NULL
    res$document_name = NULL
    # Match by chemical name first
    res = res %>%
      left_join(map_file %>%
                  select(chemical_name, clowder_id, document_name),
                by=c("name" = "chemical_name"))
    # Filter to those without a match
    res2 = res %>%
      filter(is.na(clowder_id))
    res = res %>%
      filter(!is.na(clowder_id))
    # Match by casrn
    res2 = res2 %>%
      select(-clowder_id, -document_name) %>%
      left_join(map_file %>%
                  select(casrn, clowder_id, document_name),
                by="casrn")
    # Recombine all matches
    res = rbind(res, res2)
    # Report any that did not match
    if(any(is.na(res$clowder_id))){
      cat("IRIS records not matched to Clowder ID: ", nrow(res[is.na(res$clowder_id),]))
    }
    return(res)
  }

  # Clowder id and document name matching for source_pprtv_ornl
  if (source == "PPRTV (ORNL)"){
    # Clear any old mappings
    res$clowder_id = NA
    res$document_name = NA
    # Filter to the "_webpage_" PDF Clowder document
    map_file = map_file[grepl("_webpage_", map_file$document_name) &
                          grepl(".pdf", map_file$document_name), ]
    for (i in 1:nrow(res)){
      #Will perform matching based on casrn and chemical name fields
      res_cas_num = res[i,'casrn']
      res_chem_name = res[i,'name']
      #Get rid of the leading zeros added by excel
      res_cas_num <- sub("^0+","",res_cas_num)

      #Match first based on exact chemical name (most consistently populated in key and res)
      row = match(res_chem_name,map_file$chemical_name)
      clowder_id = map_file[row,'clowder_id']
      doc_name = map_file[row,'document_name']
      #Some chemicals have additional abbreviations in the document map. Use grep to look for
      #the chemical name from res is contained in the chemical name row (different than exact matching)
      if (is.na(clowder_id)){
        rows = grep(res_chem_name,map_file$chemical_name)
        clowder_id = map_file$clowder_id[rows[1]]
        document_name = map_file$document_name[rows[1]]
      }
      #Final match criteria is the casrn number. Res has all casrns but document map does not
      #PPRTV ORNL source listed some casrn numbers as "various" instead of specific numbers
      if(is.na(clowder_id)){
        #If didn't match from chemical name, try to match by casrn
        row = match(res_cas_num,map_file$casrn)
        clowder_id = map_file[row,'clowder_id']
        doc_name = map_file[row,'document_name']
      }
      #Populate clowder id and document name fields with matched info from key
      res[i,'clowder_id'] = clowder_id
      res[i,'document_name'] = doc_name
    }
    return(res)
  }

  if (source == "Cal OEHHA"){
    # cut the map down to just the webpage PDF documents, no screenshots
    map_file <- map_file %>%
      filter(subDir1 == "pdf") %>%
      dplyr::rename(clowder_id = uuid, document_name = `File Name`)
    # clear old names
    res$clowder_id = NULL
    res$document_name = NULL
    # Match by chemical name first
    res = res %>%
      left_join(map_file %>%
                  select(Chemical, clowder_id, document_name),
                by=c("name" = "Chemical"))
    # Filter to those without a match
    res2 = res %>%
      filter(is.na(clowder_id))
    res = res %>%
      filter(!is.na(clowder_id))
    # Match by casrn
    res2 = res2 %>%
      select(-clowder_id, -document_name) %>%
      left_join(map_file %>%
                  select(casrn=CASRN, clowder_id, document_name),
                by="casrn")
    # Recombine all matches
    res = rbind(res, res2)
    # Report any that did not match
    if(any(is.na(res$clowder_id))){
      cat("CAL OEHHA records not matched to Clowder ID: ", nrow(res[is.na(res$clowder_id),]))
    }
    return(res)
  }

  # Match EFSA2 records
  if (source == "efsa2") {
    # Update map_file so it only contains mapped clowder_id values with long_refs
    map_file = map_file %>%
      select(clowder_id, document_name, long_ref) %>%
      distinct() %>%
      filter(!is.na(clowder_id))
    # clear old names
    res$clowder_id <- NULL
    res$document_name <- NULL

    # match by longref
    res <- res %>%
      left_join(select(map_file, long_ref, clowder_id, document_name),
                     by = "long_ref") %>%
      distinct()

    # This introduces a few duplicates, which we then remove
    # count <- res %>% dplyr::count(source_id)
    # duplicates <- count[which(count$n > 1),]
    # res <- filter(res, !(source_id %in% duplicates$source_id & is.na(clowder_id)))

    # report any that did not match
    if(any(is.na(res$clowder_id))){
      cat("EFSA2 records not matched to Clowder ID: ", nrow(res[is.na(res$clowder_id),]))
    }
  }

  cat("try the v8 records\n")
  #browser()
  return(res)
}
