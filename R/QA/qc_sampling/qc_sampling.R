#-----------------------------------------------------------------------------------
#' Sample ToxVal source tables
#'
#' @param toxval.db Database version
#' @param source.db Source database version
#' @param sys.date Date string for ToxValDB export
#' @param curation_method 'automated' or 'manual', Determines what sampling rule to use
#' @return sampled toxval_source records
#-----------------------------------------------------------------------------------
qc_sampling <- function(toxval.db="res_toxval_v95",source.db='res_toxval_source_V5',fraction=0.1,source=NULL,curation_method=NULL) {
  printCurrentFunction(toxval.db)

  sampled_records <- prioritize.toxval.records(toxval.db='res_toxval_v95',fraction=0.1,curation_method='manual')

  stlist = sort(unique(sampled_records$source_table))

  # Pull data profiling source_hashes
  # Unsure how to balance these and the sampled_records


  for(stable in stlist){
    query = paste0("select * from ",stable)
    full_source = runQuery(query, source.db)
    full_records = merge(x=sampled_records, y=full_source, by='source_hash', all.x=FALSE, all.y=FALSE)
    file = paste0(dir,"toxval_qc_", stable, "_", fraction*100,"_",Sys.Date(),".xlsx")
    write.xlsx(full_records,
               file)
  }
}
