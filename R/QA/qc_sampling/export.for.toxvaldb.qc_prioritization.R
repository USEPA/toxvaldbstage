library(digest)
#-----------------------------------------------------------------------------------
#' Build a data frame of the data for the toxval manuscript
#'
#' @param toxval.db Database version
#' @param source The source to be updated
#' @return Write a file with the results
#'
#-----------------------------------------------------------------------------------
export.for.toxvaldb.qc_prioritization <- function(toxval.db) {
  printCurrentFunction(toxval.db)
  dir = "Repo/qc_sampling/sampling_input"
  slist = runQuery("select distinct source from toxval",toxval.db)[,1]
  #slist = slist[!is.element(slist,c("ECOTOX"))]
  res = NULL
  for(src in slist) {
    cat(src,"\n")
    query = paste0("SELECT
                    a.dtxsid,a.name,
                    b.source,
                    b.human_eco,
                    b.toxval_type,
                    c.toxval_type_supercategory,
                    b.toxval_units,
                    b.study_type,
                    b.source_hash,
                    b.source_table
                    FROM
                    toxval b
                    INNER JOIN source_chemical a on a.chemical_id=b.chemical_id
                    INNER JOIN toxval_type_dictionary c on b.toxval_type=c.toxval_type
                    WHERE
                    b.source='",src,"'")
 #                   and toxval_type_supercategory in ('Point of Departure','Lethality Effect Level','Toxicity Value')")
    mat = runQuery(query,toxval.db,T,F)
    mat = unique(mat)
    if(nrow(mat)>0) res = rbind(res,mat)
  }
  file = file.path(dir,"toxval_for_qc_prioritization.RData")
  save(res,file=file)
}
