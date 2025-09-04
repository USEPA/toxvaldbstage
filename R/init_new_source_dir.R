#' @title Initialize New Source Directory
#' @description Create file directory for input source table names.
#' @param source_table Character string or vector of source tables names, Default: NULL
#' @return None. Folders are created.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  init_new_source_dir(source_table = "new_source")
#'  init_new_source_dir(source_table = c("new_source", "new_source_2))
#'  }
#' }
#' @rdname init_new_source_dir
#' @export
init_new_source_dir <- function(source_table = NULL){
  # Check for NULL or NA input source_table
  if(any(is.null(source_table)) || any(is.na(source_table))) stop("'source_table' parameter must be a character string or a vector of character strings, not NULL or NA or contain NULL or NA values.")
  # Create source directory root
  dir_root = file.path(toxval.config()$datapath, source_table)
  dir_exists_list = dir_root[dir.exists(dir_root)]
  dir_root = dir_root[!dir_root %in% dir_exists_list]
  source_table = source_table[!source_table %in% basename(dir_exists_list)]
  # Check if directory exists
  if(length(dir_exists_list)) {
    cat(paste0("Directory for source_table already exist: \n\t- ", paste0(dir_exists_list, collapse = "\n\t- ")))
  }

  # Create new source directory
  if(!length(dir_root)){
    message("No new source_table directories to create.")
    return()
  }

  # Subfolders to create
  default_dirs = c("files", "MySQL", "code")
  # Duplicate so there are the same number of entries for each default_dir
  # for each source_table
  dir_root = sort(rep(dir_root, each = length(default_dirs)))
  # Get subfolders with full path
  dir_root = file.path(dir_root,
                       paste(basename(dir_root), default_dirs,
                             sep = "_")
  )

  # Loop through dirs and create them
  for(new_dir in dir_root)  dir.create(new_dir, recursive = TRUE)

  cat(paste0("Directory created for source_table: \n\t- ",
             paste0(source_table, collapse = "\n\t- ")))
}
