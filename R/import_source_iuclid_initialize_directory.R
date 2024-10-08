#--------------------------------------------------------------------------------------
#' @description Initialize Source IUCLID Directory into subdirectory based on input files
#' @return None, file directory structure generated
#' @title FUNCTION_TITLE
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname initialize_source_iuclid_directory
#' @export 
#
#--------------------------------------------------------------------------------------
initialize_source_iuclid_directory <- function() {
  # Don't run the script unless you're in the right directory
  # Eventually change to using an input parameter directory
  if(!grepl(paste0(toxval.config()$datapath,"iuclid$"), getwd())) {
    stop("Working directory must be in 'Repo/iuclid'")
  }

  # Create subdirectories and move files - only IUCLID files
  files <- list.files(pattern=".xlsx") %>%
    .[!. %in% c("iuclid_field_map.xlsx")]
  for(f in files){
    # Get source name (remove date and filetype stem)
    source = gsub("_[0-9]+.xlsx", "", f) %>%
      paste0("iuclid_", .) %>%
      tolower()
    # Create source directory if not already present
    if(!dir.exists(source)){
      dir.create(source)
    }
    # Create source subdirectories if not already present
    for(s in c("_files", "_MySQL", "_code")){
      n_dir = paste0(source, "/", source, s)
      if(!dir.exists(n_dir)) dir.create(n_dir)
    }
    # Move file into /[source]_files subdirectory
    file.rename(f, paste0(source, "/", source, "_files/", f))
  }
}
