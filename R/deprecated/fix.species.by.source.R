#--------------------------------------------------------------------------------------
#'
#' Update the species information. This method has to be run iteratively to find
#' the remaining species_original names that don't match what is in the database
#'
#' @param toxval.db The version of the database to use
#--------------------------------------------------------------------------------------
fix.species.by.source <- function(toxval.db, source) {
  printCurrentFunction(paste(toxval.db,":", source))

  dist_sps <- runQuery(paste0("select distinct(species_original) from toxval where source like '",source,"'"),toxval.db)
  names(dist_sps) <- "species_original"
  print(dim(dist_sps))

  file <- paste0(toxval.config()$datapath,"species/species_dictionary_2021-04-22.xlsx")
  mat <- read.xlsx(file)
  mat <- mat[,-which(names(mat) %in% "source")]
  #print(mat[grep("_x005F",mat$species_scientific), "species_scientific"])
  mat[grep("chironomus;_x005F_x0007_",mat$species_scientific), "species_scientific"] <- gsub("chironomus;_x005F_x0007_","chironomus;_x0007_",mat[grep("chironomus;_x005F_x0007_",mat$species_scientific), "species_scientific"])
  #print(mat[grep("chironomus;",mat$species_scientific), "species_scientific"])

  print(dim(mat))
  mat <- unique(mat)
  print(dim(mat))

  mat1 <- mat[which(mat$species_scientific %in% dist_sps$species_original),]
  #print(View(mat1))
  mat2 <- mat[which(mat$species_common %in% dist_sps$species_original),]
  #print(View(mat2))
  mat_new <- rbind(mat1,mat2)
  mat_new <- unique(mat_new)
  print(dim(mat_new))
  #print(View(mat_new))
  #spc_count <- runQuery("select count(*) from species",toxval.db)
  #print(View(spc_count))
  #8500

  #commented out on jan 5 22
  #runQuery(paste0("delete from species where species_scientific in (select species_original from toxval where source like '",source,"')"),toxval.db)


  #spc_count <- runQuery("select count(*) from species",toxval.db)
  #print(View(spc_count))
  #8393

  #commented out on jan 5 22
  #runQuery(paste0("delete from species where species_common in (select species_original from toxval where source like '",source,"')"),toxval.db)



  #spc_count <- runQuery("select count(*) from species",toxval.db)
  #print(View(spc_count))
  #8304
  runInsertTable(mat_new,"species",toxval.db,T,T,F)
  #spc_count <- runQuery("select count(*) from species",toxval.db)
  #print(View(spc_count))
  #8500
  runQuery(paste0("update toxval set species_original='-' where species_original='' and source like '",source,"'"),toxval.db)
  runQuery(paste0("update toxval set species_original='-' where species_original is null and source like '",source,"'"),toxval.db)
  runQuery(paste0("update toxval set species_id= -1 where source like '",source,"'"),toxval.db)
  #spc_id <- runQuery(paste0("select distinct(species_id) from toxval where source like '",source,"'"),toxval.db)
  #print(View(spc_id))
  #-1
  spc <- runQuery("select species_id,species_scientific,species_common from species",toxval.db)

  for(i in 1:dim(spc)[1]) {
    sid <- spc[i,"species_id"]
    n1 <- spc[i,"species_scientific"]
    n1 <-  str_replace_all(n1,"\'","\\\\'")
    n2 <- spc[i,"species_common"]
    n2 <-  str_replace_all(n2,"\'","\\\\'")
    query <- paste0("update toxval set species_id=",sid," where species_original='",n1,"' and source like '",source,"'")
    runQuery(query,toxval.db)
    query <- paste0("update toxval set species_id=",sid," where species_original='",n2,"' and source like '",source,"'")
    runQuery(query,toxval.db)
    if(i%%1000==0) cat("finished",i,"of ",dim(spc)[1],"\n")
  }

  res1 <- runQuery(paste0("select distinct(species_id) from toxval where source like '",source,"'"),toxval.db)
  cat("Number of species in toxval for the particular source:",nrow(res1),"\n")

  res2 <- runQuery(paste0("select species_id from species where species_id in(select species_id from toxval where source like '",source,"')"),toxval.db)
  cat("Number of species in species for the particular source:",nrow(res2),"\n")


  res3 <- runQuery(paste0("select distinct species_original from toxval where species_id<0 and source like '",source,"'"),toxval.db)
  cat("Number of missing species:",nrow(res3),"\n")

  # file <- paste0(toxval.config()$datapath,"species/missing_species_in_",source,"_",Sys.Date(),".xlsx")
  # write.xlsx(res,file)
}
