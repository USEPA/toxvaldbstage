#-------------------------------------------------------------------------------------
#' Load the Arnot BAF / BCF data
#'
#' @param source.db The source database to use.
#' @param verbose If TRUE, print out extra diagnostic messages
#' @export
#--------------------------------------------------------------------------------------
toxval.load.bcfbaf <- function(toxval.db, verbose=F) {
  printCurrentFunction(toxval.db)

  name.list <- c(
    "casrn","name",
    "units",
    "species_supercategory",
    "species_scientific",
    "species_common",
    "author",
    "title",
    "year",
    "journal",
    "logbaf",
    "logbcf",
    "tissue",
    "calc_method",
    "comments",
    "logkow",
    "logkow_reference",
    "water_conc",
    "radiolabel",
    "exposure_duration",
    "exposure_type",
    "temperature",
    "exposure_route",
    "media",
    "pH",
    "total_organic_carbon",
    "wet_weight",
    "lipid_content")
  file <- "./PB/Arnot_Gobas_supplementary information_open.xls_20190328.xlsx"
  res <- read.xlsx(file)
  res$units <- "L/kg"
  res <- res[,name.list]
  for(i in 1:nrow(res)) res[i,"casrn"] <- fix.casrn(res[i,"casrn"])
  res <- res[,name.list]
  res <- res[!is.na(res[,"casrn"]),]
  cas.list = res[,1:2]
  cid.list = get.cid.list.toxval(toxval.db, cas.list,"bcfbaf")
  res$chemical_id <- cid.list$chemical_id
  #res <- merge(res,cid.list)
  res <- res[,!is.element(names(res),c("casrn","name"))]
  res <- unique(res)
  res$bcfbaf_uuid <- '-'
  for(i in 1:nrow(res)) {
    row <- res[i,2:ncol(res)]
    res[i,"bcfbaf_uuid"] <- digest(paste0(row,collapse=""), serialize = FALSE)
    res[i,"qa_level"] <- -1
  }
  #runQuery("delete from bcfbaf",db)
  for(i in 1:nrow(res)) res[i,"bcfbaf_uuid"] <- UUIDgenerate()
  runInsertTable(res, "bcfbaf", toxval.db,verbose)
}
