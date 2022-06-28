#--------------------------------------------------------------------------------------
#'
#' Update the species_id in toxval from species_ecotox.
#'
#' @param toxval.db The version of the database to use
#' @export
#--------------------------------------------------------------------------------------
fix.species.ecotox.by.source <- function(toxval.db, source) {
  printCurrentFunction(paste(toxval.db,":", source))

  n = runQuery("select count(*) from species_ecotox",toxval.db)[1,1]
  if(n==0) toxval.load.species_ecotox(toxval.db)
  dist_sps <- runQuery(paste0("select distinct(species_original) from toxval where source like '",source,"'"),toxval.db)
  names(dist_sps) <- "species_original"
  print(dim(dist_sps))

  sps_ecotox <- runQuery("select * from species_ecotox ;",toxval.db)
  print(dim(sps_ecotox))
  sps_ecotox <- unique(sps_ecotox)
  print(dim(sps_ecotox))

  mat1 <- sps_ecotox[which(sps_ecotox$latin_name %in% dist_sps$species_original),]

  runQuery(paste0("update toxval set species_original='-' where species_original='' and source like '",source,"'"),toxval.db)
  runQuery(paste0("update toxval set species_original='-' where species_original is null and source like '",source,"'"),toxval.db)
  runQuery(paste0("update toxval set species_id= -1 where source like '",source,"'"),toxval.db)

  for(i in 1:dim(sps_ecotox)[1]) {
    sid <- sps_ecotox[i,"species_id"]
    n1 <- tolower(sps_ecotox[i,"latin_name"])
    n2 <- tolower(sps_ecotox[i,"common_name"])

    query <- paste0("update toxval set species_id=",sid," where species_original='",n1,"' and source like '",source,"'")
    runQuery(query,toxval.db)

    query <- paste0("update toxval set species_id=",sid," where species_original='",n2,"' and source like '",source,"'")
    runQuery(query,toxval.db)
    if(i%%1000==0) cat("finished",i,"of ",dim(sps_ecotox)[1],"\n")
  }

  res3 <- runQuery(paste0("select distinct species_original from toxval where species_id<0 and source like '",source,"'"),toxval.db)
  cat("Number of missing species:",nrow(res3),"\n")

  file <- paste0(toxval.config()$datapath,"species/missing_species_in_",source,"_",Sys.Date(),".xlsx")
  write.xlsx(res3,file)
}
