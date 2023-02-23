#--------------------------------------------------------------------------------------
#' qa_prioritize_by_chemical_list
#' Compare toxval_source chemical information against input chemical lists to
#' prioritize for QC
#'
#' @param db the name of the database
#' @param chem_list XLSX filepath for an input list of chemicals to compare against, with fields
#' 'DTXSID', 'name', and 'cas'
#' @export
#--------------------------------------------------------------------------------------
qa_prioritize_by_chemical_list <- function(db, chem_list){

  if(is.null(chem_list) || is.na(chem_list)) stop("Must provide an input 'chem_list' filepath")
  # Check if string and XLSX file extension
  if(!is.character(chem_list) || strsplit(chem_list, ".", fixed=T)[[1]][-1] != 'xlsx') stop("Must be a string filepath of type 'XLSX'")

  # Load input chemical list
  chem_list = chem_list %>%
    readxl::read_xlsx()

  cat("\nPulling toxval_source chemicals to compare...")
  # Pull source_chemical table from toxval_source
  src_chem = runQuery(query = "SELECT chemical_id, source, raw_casrn, raw_name, cleaned_casrn, cleaned_name, dtxsid FROM source_chemical", db=db)

  cat("\n...Matching...")
  # Match by DTXSID first
  matches = src_chem %>%
    filter(dtxsid %in% chem_list$DTXSID)

  # Filter out matches
  src_chem = src_chem %>%
    filter(!chemical_id %in% matches$chemical_id)

  # Match by clean name
  matches = src_chem %>%
    filter(cleaned_name %in% chem_list$name) %>%
    rbind(matches)

  # Filter out matches
  src_chem = src_chem %>%
    filter(!chemical_id %in% matches$chemical_id)

  # Match by clean cas
  matches = src_chem %>%
    filter(cleaned_casrn %in% chem_list$cas) %>%
    rbind(matches)

  # Filter out matches
  src_chem = src_chem %>%
    filter(!chemical_id %in% matches$chemical_id)

  # Filter by raw name
  matches = src_chem %>%
    filter(raw_name %in% chem_list$name) %>%
    rbind(matches)

  # Filter out matches
  src_chem = src_chem %>%
    filter(!chemical_id %in% matches$chemical_id)

  # Filter by raw cas
  matches = src_chem %>%
    filter(raw_casrn %in% chem_list$cas) %>%
    rbind(matches)

  cat("\nFound ", nrow(matches), " matches of ", (nrow(matches)+nrow(src_chem)), " from list of ", nrow(chem_list))

  # Summarize and export
  out = list(matches = matches,
             summary = matches %>%
               group_by(source) %>%
               dplyr::summarise(n_matches = n()))

  return (out)
}
