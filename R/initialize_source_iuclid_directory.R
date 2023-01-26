#--------------------------------------------------------------------------------------
#' Initialize Source IUCLID Directory into subdirectory based on input files
#' @return None, file directory structure generated
#
#--------------------------------------------------------------------------------------
initialize_source_iuclid_directory <- function() {
  # Don't run the script unless you're in the right directory
  # Eventually change to using an input parameter directory
  if(getwd() != "/ccte/ACToR1/ToxValDB9/Repo/iuclid") {
    stop("Working directory must be '/ccte/ACToR1/ToxValDB9/Repo/iuclid'")
  }

  # Create subdirectories and move files - only IUCLID files
  files <- list.files(pattern=".xlsx")
  for(f in files){
    # Get source name
    #source <- substring(f, 1, nchar(f)-16)
    source = gsub(".xlsx", "", f) %>%
      paste0("iuclid_", .) %>%
      tolower()
    # Create source directory if not already present
    if(!dir.exists(source)){
      dir.create(source)
    }
    # Create source subdirectories if not already present
    for(s in c("_files", "_MySQL", "_R")){
      n_dir = paste0(source, "/", source, s)
      if(!dir.exists(n_dir)) dir.create(n_dir)
    }
    # Move file into /[source]_files subdirectory
    file.rename(f, paste0(source, "/", source, "_files/", f))
  }
}
