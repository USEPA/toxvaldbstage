#--------------------------------------------------------------------------------------
#' print out the dize of each of the tables
#'
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param indir The directory where the output file will be placed
#' @param infile The input file ./chiu/chiu_files/Full_RfD_databaseQAed-FINAL.xlsx
#' @param chem.chek.halt If TRUE and there are bad chemical names or casrn,
#' stop to look at the results in indir/chemcheck.xlsx
#--------------------------------------------------------------------------------------
source.size <- function(db="res_toxval_source_v5") {
  printCurrentFunction(db)

  tlist = runQuery("show tables",sdb)[,1]
  for(table in tlist) {
    if(substr(table,1,7)=="source_") {
      query = paste0("select count(*) from ",table)
      n=runQuery(query,db)[1,1]
      cat(table,":",n,"\n")
    }
  }

}
