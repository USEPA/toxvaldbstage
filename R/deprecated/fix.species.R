#--------------------------------------------------------------------------------------
#'
#' Update the species information. This method has to be run iteratively to find
#' the remaining species_original names that don't match what is in the database
#'
#' @param toxval.db The version of the database to use
#--------------------------------------------------------------------------------------
fix.species <- function(toxval.db) {
  printCurrentFunction()

  file <- paste0(toxval.config()$datapath,"species/species_dictionary_2021-04-22.xlsx")
  mat <- read.xlsx(file)
  mat <- mat[,-which(names(mat) %in% "source")]
  #print(mat[grep("_x005F",mat$species_scientific), "species_scientific"])
  mat[grep("chironomus;_x005F_x0007_",mat$species_scientific), "species_scientific"] <- gsub("chironomus;_x005F_x0007_","chironomus;_x0007_",mat[grep("chironomus;_x005F_x0007_",mat$species_scientific), "species_scientific"])
  #print(mat[grep("chironomus;",mat$species_scientific), "species_scientific"])

  print(dim(mat))
  mat <- unique(mat)
  print(dim(mat))
  print(names(mat))

  # file <- paste0(toxval.config()$datapath,"species/ECOTOX_dictionary_2018-11-27.xlsx")
  # mat2 <- read.xlsx(file)
  # names(mat2) <- names(mat1)
  # mat <- rbind(mat1,mat2)
  # print(dim(mat))
  # mat <- unique(mat)
  # print(dim(mat))

  runQuery("delete from species",toxval.db)
  runInsertTable(mat,"species",toxval.db,T,T,F)
  runQuery("update toxval set species_original='-' where species_original=''",toxval.db)
  runQuery("update toxval set species_original='-' where species_original is null",toxval.db)
  runQuery("update toxval set species_id= -1",toxval.db)
  spc <- runQuery("select species_id,species_scientific,species_common from species",toxval.db)
  for(i in 1:dim(spc)[1]) {
    sid <- spc[i,"species_id"]
    n1 <- spc[i,"species_scientific"]
    #source_casrn <- str_replace_all(source_casrn,"\'","\\\\'")

    n1 <-  str_replace_all(n1,"\'","\\\\'")
    n2 <- spc[i,"species_common"]
    n2 <-  str_replace_all(n2,"\'","\\\\'")
    query <- paste0("update toxval set species_id=",sid," where species_original='",n1,"'")
    runQuery(query,toxval.db)
    query <- paste0("update toxval set species_id=",sid," where species_original='",n2,"'")
    runQuery(query,toxval.db)
    if(i%%1000==0) cat("finished",i,"of ",dim(spc)[1],"\n")
  }
  res <- runQuery("select distinct species_original from toxval where species_id<0",toxval.db)
  cat("Number of missing species:",nrow(res),"\n")
  # file <- paste0(toxval.config()$datapath,"species/missing_species",Sys.Date(),".xlsx")
  # write.xlsx(res,file)
}
