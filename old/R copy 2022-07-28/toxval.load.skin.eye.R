#--------------------------------------------------------------------------------------
#' Load the Skin eye data
#'
#' @param toxval.db Database version
#' @param verbose if TRUE, print diagnostic messages along the way
#' @export
#--------------------------------------------------------------------------------------
toxval.load.skin.eye <- function(toxval.db,verbose=F) {
  printCurrentFunction(toxval.db)
  runQuery("delete from skin_eye",toxval.db)

  cat("  load GHS data\n")
  file <- "./skin_eye/GHS skin and eye data v3.xlsx"
  mat <- read.xlsx(file)
  mat[is.element(mat[,"score"],"N/A"),"score"] <- "NC"
  mat$record_url <- "-"
  file <- "./skin_eye/GHS_sources.xlsx"
  dict <- read.xlsx(file)
  source.list <- unique(dict[,"Abbreviation"])
  for(source in source.list) {
    url <- dict[is.element(dict[,"Abbreviation"],source),"Source.URL"]
    mat[is.element(mat[,"source"],source),"record_url"] <- url
  }

  names(mat) <- c("casrn","name","endpoint","source","score","route","classification","hazard_code",
                  "hazard_statement","rationale","note","note2","valueMassOperator","valueMass","valueMassUnits","authority","record_url")
  mat$result_text <- paste(mat[,"hazard_statement"],mat[,"hazard_code"],mat[,"rationale"])
  mat <- mat[,c("casrn","name","endpoint","source","score","classification","result_text","authority","record_url")]

  casrn.list <- mat[,1:2]
  cid.list <- get.cid.list.toxval(toxval.db, casrn.list,"GHS Skin Eye")
  mat$chemical_id <- cid.list$chemical_id
  #res <- merge(res,cid.list)
  mat <- mat[,3:dim(mat)[2]]
  mat$study_type <- tolower(mat$endpoint)
    for(i in 1:nrow(mat)) mat[i,"skin_eye_uuid"] <- UUIDgenerate()

  runInsertTable(mat, "skin_eye", toxval.db, verbose)

  cat("  load eChemPortal data\n")
  file <- "./skin_eye/echemportal_skin_eye_parsed.xlsx"
  mat <- read.xlsx(file)
  names(mat)[is.element(names(mat),"result")] <- "result_text"
  mat$source <- "ECHA eChemPortal"
  casrn.list <- mat[,1:2]
  cid.list <- get.cid.list.toxval(toxval.db, casrn.list,"eChemPortal Skin Eye",verbose=verbose)
  mat$chemical_id <- cid.list$chemical_id
  #mat <- merge(mat,cid.list)

  mat <- mat[,3:dim(mat)[2]]
  mat[is.element(mat[,"study_type"],"Eye irritation"),"study_type"] <- "eye irritation"
  mat[is.element(mat[,"study_type"],"Skin irritation / corrosion"),"study_type"] <- "skin irritation"
  mat[is.element(mat[,"study_type"],"Skin sensitisation"),"study_type"] <- "skin sensitization"
  mat$authority <- "Screening"
  file <- "./skin_eye/skin_eye raw.xlsx"
  write.xlsx(mat,file)
  runInsertTable(mat, "skin_eye", toxval.db, verbose=verbose)
}
