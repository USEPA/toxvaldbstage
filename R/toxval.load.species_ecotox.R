#--------------------------------------------------------------------------------------
#'
#' Load the species_ecotox table and the species_id column in toxval
#'
#' This function replaces fix.species
#' This function precedes toxvaldb.load.species
#'
#' @param toxval.db The version of the database to use
#' @param restart If TRUE, rest all of the species_id values in toxval
#' @param date.string Date suffix on the input species dictionary
#' @export
#--------------------------------------------------------------------------------------
toxval.load.species_ecotox <- function(toxval.db,restart=F,date_string="2022-05-25") {
  printCurrentFunction()
  file = paste0(toxval.config()$datapath,"species/ecotox_species_dictionary_",date_string,".xlsx")
  dict = openxlsx::read.xlsx(file)
  file = paste0(toxval.config()$datapath,"species/ecotox_species_synonyms_",date_string,".xlsx")
  synonyms = openxlsx::read.xlsx(file)
  file = paste0(toxval.config()$datapath,"species/toxvaldb_extra_species_",date_string,".xlsx")
  extra = openxlsx::read.xlsx(file)

  dict2 = extra[,c("species_id","common_name","latin_name","ecotox_group")]
  dict2 = dict2[!is.element(dict2$species_id,dict$species_id),]
  dict2$kingdom = NA
  dict2$phylum_division = NA
  dict2$subphylum_div = NA
  dict2$superclass = NA
  dict2$class = NA
  dict2$tax_order = NA
  dict2$family = NA
  dict2$genus = NA
  dict2$species = NA
  dict2$subspecies = NA
  dict2$variety = NA
  dict2 = dict2[,names(dict)]
  dict = rbind(dict,dict2)
  count = runQuery("select count(*) from species_ecotox where species_id=-1",toxval.db)[1,1]
  if(count==0) {
    runInsert("insert into species_ecotox (species_id,common_name,latin_name) values (-1,'Not Specified','Not Specified')",toxval.db,do.halt=T)
  }
  if(restart) {
    cat("Reset species_id\n")
    runQuery("update toxval set species_id=-1",toxval.db)
    runQuery("delete from species_ecotox where species_id>=0",toxval.db)
    runInsertTable(dict,"species_ecotox",toxval.db)
  }

  # Do the special steps for Rat and Mouse
  runQuery("update species_ecotox set common_name='Norway Rat' where species_id=4510",toxval.db)
  runQuery("update species_ecotox set common_name='House Mouse' where species_id=4913",toxval.db)
  runQuery("update species_ecotox set common_name='Domestic Dog' where species_id=4928",toxval.db)
  runQuery("update species_ecotox set common_name='Dog Family' where species_id=7630",toxval.db)
  runQuery("update species_ecotox set common_name='Old World Rabbits' where species_id=22808",toxval.db)
  runQuery("update species_ecotox set common_name='Cat Family' where species_id=7378",toxval.db)

  dict$latin_name = tolower(dict$latin_name)
  dict$common_name = tolower(dict$common_name)
  synonyms$latin_name = tolower(synonyms$latin_name)
  extra$common_name = tolower(extra$common_name)
  extra$latin_name = tolower(extra$latin_name)
  extra$species_original = tolower(extra$species_original)

  slist = runQuery("select species_id from species_ecotox",toxval.db)[,1]
  extra2 = extra[!is.element(extra$species_id,slist),]
  if(nrow(extra2)>0) {
    cat("Need to add records to species_ecotox\n")
    extra2 = extra2[,c("species_id","common_name","latin_name","ecotox_group")]
    runInsertTable(extra2, "species_ecotox", toxval.db,verbose=T)
  }
  if(restart) {
    so = runQuery("select distinct species_original from toxval where species_id= -1",toxval.db)[,1]
    count.good = 0
    cat("Start setting species_id\n")
    if(length(so)>0) {
      for(i in 1:length(so)) {
        tag = so[i]
        tag = tolower(tag)
        #browser()
        #tag = str_replace_all(tag,"\'","\\\\'")
        tag0 = tag
        nc = nchar(tag)
        tagend = substr(tag,nc-2,nc)
        if(tagend==" sp") {
          tag = paste0(tag,".")
        }
        slist = c("other aquatic arthropod ","other aquatic crustacea: ","other aquatic mollusc: ",
                  "other aquatic worm: ","other,","other,other algae: ","other: ")
        for(x in slist) {
          if(contains(tag,x)) tag = str_replace(tag,x,"")
        }
        sid = -1
        if(is.element(tag,dict$common_name)) {
          sid = dict[is.element(dict$common_name,tag),"species_id"][1]
        }
        else if(is.element(tag,dict$latin_name)) {
          sid = dict[is.element(dict$latin_name,tag),"species_id"][1]
        }
        else if(is.element(tag,synonyms$latin_name)) {
          sid = dict[is.element(synonyms$latin_name,tag),"species_id"][1]
        }
        else if(is.element(tag,extra$species_original)) {
          sid = extra[is.element(extra$species_original,tag),"species_id"][1]
        }
        else if(is.element(tag0,dict$common_name)) {
          sid = dict[is.element(dict$common_name,tag0),"species_id"][1]
        }
        else if(is.element(tag0,dict$latin_name)) {
          sid = dict[is.element(dict$latin_name,tag0),"species_id"][1]
        }
        else if(is.element(tag0,synonyms$latin_name)) {
          sid = dict[is.element(synonyms$latin_name,tag0),"species_id"][1]
        }
        else if(is.element(tag0,extra$species_original)) {
          sid = extra[is.element(extra$species_original,tag0),"species_id"][1]
        }
        else if(is.element(tag,extra$species_original)) {
          sid = extra[is.element(extra$species_original,tag),"species_id"][1]
        }
        if(sid>=0) {
          count.good = count.good+1
          query = paste0("update toxval set species_id=",sid," where species_original='",str_replace_all(tag0,"\\\'","\\\\'"),"'")
          runQuery(query,toxval.db)
        }
        else {
          cat(tag,"\n")
          #browser()
        }
        if(i%%100==0) cat("finished",i,"out of",length(so),":",count.good,"\n")
      }
    }
    # Do the special steps for Rat and Mouse
    runQuery("update species_ecotox set common_name='Rat' where species_id=4510",toxval.db)
    runQuery("update species_ecotox set common_name='Mouse' where species_id=4913",toxval.db)
    runQuery("update species_ecotox set common_name='Dog' where species_id=4928",toxval.db)
    runQuery("update species_ecotox set common_name='Dog' where species_id=7630",toxval.db)
    runQuery("update species_ecotox set common_name='Rabbit' where species_id=22808",toxval.db)
    runQuery("update species_ecotox set common_name='Cat' where species_id=7378",toxval.db)

    runQuery("update toxval set species_id=4510 where species_id=23410",toxval.db)

  #   # Add default species to particular sources
  #   human.list = c("Alaska DEC","Cal OEHHA","California DPH","DOD","DOE Protective Action Criteria",
  #                  "EPA AEGL","EPA OPP","FDA CEDI","Health Canada","IRIS","Mass. Drinking Water Standards",
  #                  "OSHA Air contaminants","OW Drinking Water Standards","Pennsylvania DEP MCLs",
  #                  "Pennsylvania DEP ToxValues","PFAS Summary PODs","PPRTV (NCEA)","PPRTV (ORNL)",
  #                  "RSL","USGS HBSL","Wignall")
  #   for(src in human.list) {
  #     query = paste0("update toxval set species_id=2000000 where species_id=1000000 and source='",src,"'")
  #     runQuery(query,toxval.db)
  #   }
  #
  #   # Set human_eco
  #   cat("set human eco\n")
  #   runQuery("update toxval set human_eco='eco'",toxval.db)
  #   runQuery("update toxval set human_eco='not specified' where species_id in (select species_id from species_ecotox where ecotox_group='Not Specified')",toxval.db)
  #   runQuery("update toxval set human_eco='human health' where species_id in (select species_id from species_ecotox where ecotox_group='Mammals')",toxval.db)
  }
}
