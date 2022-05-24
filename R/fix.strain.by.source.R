#--------------------------------------------------------------------------------------
#'
#' Update the strain_group in toxval from strain_dictionary_2022-03-07.xlsx. 
#'
#' @param toxval.db The version of the database to use
#' @export
#--------------------------------------------------------------------------------------
fix.strain.by.source <- function(toxval.db, source) {
  printCurrentFunction(paste(toxval.db,":", source))
  
  
  tox_fields <- runQuery(paste0("select distinct strain_original,strain,strain_group,species_original from toxval where source like '",source,"'"),toxval.db)
  names(tox_fields) <- c("strain_original","strain","strain_group","species_original")
  print(dim(tox_fields))
  
  # there are strain_original values in both toxval and dict which have double quotes and single quotes within the value, instead of changing quotes in original subset those values and handle them seperately
  print(unique(tox_fields[grep('\\"',tox_fields$strain_original),"strain_original"]))
  
  # there are species_original values in both toxval and dict which have double quotes and single quotes within the value, instead of changing quotes in original subset those values and handle them seperately
  print(unique(tox_fields[grep('\\"',tox_fields$species_original),"species_original"]))
  
  
  # tox_fields <- tox_fields[!tox_fields$strain_original %in% tox_fields[grep('\\"',tox_fields$strain_original),"strain_original"], ]
  # print(dim(tox_fields))
  # tox_fields <- tox_fields[!tox_fields$species_original %in% tox_fields[grep('\\"',tox_fields$species_original),"species_original"], ]
  # print(dim(tox_fields))
  print(View(tox_fields))
  
  file <- paste0(toxval.config()$datapath,"dictionary/2021_dictionaries/strain_dictionary_2022-03-07.xlsx")
  dict <- read.xlsx(file)
  dict <- dict[,-5]
  dict <- unique(dict)
  print(View(dict))
  
  
  missing_str <- setdiff(tox_fields[,-3], dict[,-3])
  print(View(missing_str))
  
  new_str_group <- tox_fields[,-3] %>% left_join(dict)
  new_str_group <- unique(new_str_group)
  print(View(new_str_group))
  
  # since strain field values mismatch between dict and toxval, subset by excluding strain field
  new_str_group2 <- tox_fields[,-c(3,2)] %>% left_join(dict[,-2])
  new_str_group2 <- unique(new_str_group2)
  print(View(new_str_group2))
  
  
  
  #####################################################################
  cat("map strain_group to toxval\n")
  #####################################################################

  for(i in 1:nrow(new_str_group2)) {
    str_original <- new_str_group2[i,"strain_original"]
    str_grp <- new_str_group2[i,"strain_group"]
    sps_original <- new_str_group2[i,"species_original"]
    query <- paste0("update toxval set strain_group =\"",str_grp,"\" where strain_original =\"",str_original,"\" and species_original =\"",sps_original,"\" and source =\"",source,"\" ")
    #query <- paste0("update toxval set strain_group =\'",str_grp,"\' where strain_original =\'",str_original,"\' and species_original =\'",sps_original,"\' and source =\'",source,"\' ")
    runInsert(query,toxval.db,T,F,T)

  }

  runQuery(paste0("update toxval set strain_group='Not Specified' where  strain_group='NA' and source like '",source,"'"),toxval.db)
  runQuery(paste0("update toxval set strain_group='Not Specified' where  strain_group='-' and source like '",source,"'"),toxval.db)
  
  
}
  
  