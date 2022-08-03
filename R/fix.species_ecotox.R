#--------------------------------------------------------------------------------------
#'
#' Update the species ecotox table with habitat information from ecotox dictionary. 
#'
#' @param toxval.db The version of the database to use
#' @export
#--------------------------------------------------------------------------------------
fix.species_ecotox <- function(toxval.db) {
  printCurrentFunction(toxval.db)
  
  sps_eco <- runQuery("select distinct common_name, latin_name, ecotox_group from species_ecotox",toxval.db)
  names(sps_eco) <- c("common_name","latin_name","ecotox_group")
  sps_eco <- unique(sps_eco)
  print(View(sps_eco))
  
  file <- paste0(toxval.config()$datapath,"ecotox/ecotox_files/ECOTOX_dictionary_2022-03-03.xlsx")
  mat <- read.xlsx(file)
  mat <- unique(mat)
  mat <- mat[,c(2,1,3,4)]
  print(View(mat))
  # scenario where duplicated entries with exception of habitat
  mat1 <- mat[duplicated(mat[,-4]) | duplicated(mat[,-4], fromLast = TRUE),]
  #print(dim(mat1))
  # subset by removing the duplicated entries
  mat_new <- setdiff(mat,mat1)
  mat_new <- unique(mat_new)
  print(dim(mat_new))
  
  # assign habitat column from dictionary to values in species_ecotox table
  
  new_sps_eco <- merge(sps_eco, mat_new, by.x=c('common_name','latin_name','ecotox_group'), by.y=c('species_common_name','species_scientific_name','species_group'))
  # print(View(new_sps_eco))
  
  # find records which are missing from dictionary
  missing_sps <- setdiff(sps_eco, new_sps_eco[,-4])
  #print(View(missing_sps))
  
  # There are species_group values in species_ecotox that lack "; ", which is present in ecotox species dictionary
  # eg: FishStandard Test Species in sps_eco and Fish; Standard Test Species in non duplicated dictionary(mat_new)
  
  # replace "; " from dictionary species_group before performing habitat replacement
  mat_new[grep("(;\\s+)",mat_new$species_group),"species_group"] <- gsub("(;\\s+)","",mat_new[grep("(;\\s+)",mat_new$species_group),"species_group"])
  
  # habitat replacement
  new_sps_eco <- merge(sps_eco, mat_new, by.x=c('common_name','latin_name','ecotox_group'), by.y=c('species_common_name','species_scientific_name','species_group'))
  #print(View(new_sps_eco))
  
  # There are missing values that are present in species_ecotox table but absent from ecotox species dictionary from datahub
  missing_sps <- setdiff(sps_eco, new_sps_eco[,-4])
  #print(dim(missing_sps))
  
  # priortizing aquatic over terrestrial in duplicates
  mat_2 <- mat1[grep("Aquatic",mat1$habitat),]
  # removing "; " from duplicated subset
  mat_2[grep("(;\\s+)",mat_2$species_group),"species_group"] <- gsub("(;\\s+)","",mat_2[grep("(;\\s+)",mat_2$species_group),"species_group"])
  mat_3 <- rbind(mat_new,mat_2)
  
  print(dim(mat_3))
  
  # habitat replacement for combined non-duplicated dictionary values and aquatic habitat dict values
  new_sps_eco <- merge(sps_eco, mat_3, by.x=c('common_name','latin_name','ecotox_group'), by.y=c('species_common_name','species_scientific_name','species_group'))
  #print(View(new_sps_eco))

  # there are missing values that are present in species_ecotox table but absent from ecotox species dictionary from datahub
  missing_sps <- setdiff(sps_eco, new_sps_eco[,-4])
  #print(View(missing_sps))
  #15000
  
  
  # There are scenarios where the group in dictionary is "AlgaeStandard Test Species" and sps_eco is "Standard Test SpeciesAlgae"
  # replace AlgaeStandard Test Species in dictionary with Standard Test SpeciesAlgae
  #print(unique(mat_3[grep("(AlgaeStandard Test Species)",mat_3$species_group),"species_group"]))
  
  
  mat_3[grep("^AlgaeStandard Test Species$",mat_3$species_group),"species_group"] <- gsub("^AlgaeStandard Test Species", "Standard Test SpeciesAlgae",mat_3[grep("^AlgaeStandard Test Species$",mat_3$species_group),"species_group"])
  
  # habitat replacement for combined non-duplicated dictionary values and aquatic habitat dict values with algaestandard test species fixed
  new_sps_eco <- merge(sps_eco, mat_3, by.x=c('common_name','latin_name','ecotox_group'), by.y=c('species_common_name','species_scientific_name','species_group'))
  new_sps_eco <- unique(new_sps_eco)
  print(View(new_sps_eco))
  
  # there are missing values that are present in species_ecotox table but absent from ecotox species dictionary from datahub
  missing_sps <- setdiff(sps_eco, new_sps_eco[,-4])
  print(View(missing_sps))
  #14977
  
  #####################################################################
  cat("map habitat to species_ecotox table\n")
  #####################################################################

  for(i in 1:nrow(new_sps_eco)) {
    common_sps <- new_sps_eco[i,"common_name"]
    latin_sps <- new_sps_eco[i,"latin_name"]
    ecotox_grp <- new_sps_eco[i,"ecotox_group"]
    habitat_sps <- new_sps_eco[i,"habitat"]
    query <- paste0("update species_ecotox set habitat =\"",habitat_sps,"\" where common_name=\"",common_sps,"\" and latin_name=\"",latin_sps,"\" and ecotox_group=\"",ecotox_grp,"\" ")
    runInsert(query,toxval.db,T,F,T)

  }
  
}
  