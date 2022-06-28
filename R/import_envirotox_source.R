#--------------------------------------------------------------------------------------
#' Load EnviroTox.V2 Source data into dev_toxval_source_v4.
#' @param db The version of toxval into which the source info is loaded.
#' @param infile The input file ./envirotox/envirotox_files/envirotox_taxonomy.xlsx
#--------------------------------------------------------------------------------------
import_envirotox_source <- function(db,
                                    infile="../envirotox/envirotox_files/envirotox_taxonomy clean casrn.xlsx",
                                    chem.check.halt=F) {
  printCurrentFunction(db)
  #####################################################################
  cat("Read envirotox file sheet1(test) as res \n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile)

  #####################################################################
  cat("change colnames to lowercase and convert dots in names to underscore \n")
  #####################################################################
  names(res) <- tolower(names(res))
  names(res) <- gsub("\\.","_",names(res))

  #####################################################################
  cat("fix casrn for cas and original_cas fields \n")
  #####################################################################
  clist = unique(res$cas)
  for(cas0 in clist) {
    cas = fix.casrn(cas0)
    res[is.element(res$cas,cas0),"cas"] = cas
  }
  clist = unique(res$original_cas)
  for(cas0 in clist) {
    cas = fix.casrn(cas0)
    res[is.element(res$original_cas,cas0),"original_cas"] = cas
  }
  # for (i in 1:dim(res)[1]){
  #   res[i, "cas"] <- fix.casrn(res[i, "cas"])
  # }
  # for (i in 1:dim(res)[1]){
  #   res[i, "original_cas"] <- fix.casrn(res[i, "original_cas"])
  # }
  names.list <- names(res)
  # res[,"source_id"] <- c(1:length(res[,1]))
  # res[,"source_hash"] <- "-"
  # res[,"clowder_id"] <- "61f14c70e4b0ebacf2ec476c"
  # res <- res[,c("source_id","source_hash","clowder_id", names.list)]

  res = res[!is.element(res$cas,"NOCAS"),]

  names(res) = c("casrn","name","latin_name",
                 "trophic_level","effect","effect_value",
                 "unit","test_type","test_statistic",
                 "duration","duration_days","duration_hours",
                 "effect_is_5x_above_water_solubility","source","version",
                 "reported_chemical_name","original_cas")
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="EnviroTox_v2",table="source_envirotox",res=res,F,T,T)
}
