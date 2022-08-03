#-----------------------------------------------------------------------------------
#
#' Export the BCF / BAF data data
#'
#' @param toxval.db Database version
#'
#' @return writes and Excel file with the name
#'  ../qc_export/toxval_genetox_details_[Sys.Date].xlsx
#'
#-----------------------------------------------------------------------------------
export.bcfbaf <- function(toxval.db,dir="./export") {
  printCurrentFunction(toxval.db)

  query <- paste0("SELECT
                  a.casrn,a.name,a.dtxsid,
                  b.bcfbaf_hash,
                  b.bcfbaf_uuid,
                  b.value_type,
                  b.units,
                  b.score,
                  b.species_supercategory,
                  b.species_scientific,
                  b.species_common,
                  b.author,
                  b.title,
                  b.year,
                  b.journal,
                  b.logbaf,
                  b.logbcf,
                  b.tissue,
                  b.calc_method,
                  b.comments,
                  b.qa_level,
                  b.logkow,
                  b.logkow_reference,
                  b.water_conc,
                  b.radiolabel,
                  b.exposure_duration,
                  b.exposure_type,
                  b.temperature,
                  b.exposure_route,
                  b.media,
                  b.pH,
                  b.total_organic_carbon,
                  b.wet_weight,
                  b.lipid_content,
                  b.bcfbaf_id,
                  b.chemical_id
                  FROM
                  chemical a
                  INNER JOIN bcfbaf b on a.dtxsid=b.dtxsid
                  ")
  mat <- runQuery(query,toxval.db,T,F)
  print(dim(mat))
  file <- paste0(dir,"/toxval_bcfbaf_",Sys.Date(),".xlsx")
  sty <- createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  write.xlsx(mat,file,firstRow=T,headerStyle=sty)
}
