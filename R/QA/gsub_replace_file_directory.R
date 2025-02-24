library(xfun)

#' @title gsub_replace_file_directory
#' @description Function to pattern replace string found in all files within an
#' input file directory path.
#' @param repo_path Path to repository of files to perform pattern replacement.
#' @param pattern String regex pattern to match by.
#' @param replacement String replacement for all regex pattern matches.
#' @return None. Output log file is written showing which files were changed.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[xfun]{gsub_file}}
#'  \code{\link[dplyr]{bind_rows}}
#'  \code{\link[writexl]{write_xlsx}}
#' @rdname gsub_replace_file_directory
#' @export
#' @importFrom xfun gsub_file
#' @importFrom dplyr bind_rows
#' @importFrom writexl write_xlsx
gsub_replace_file_directory <- function(repo_path, pattern, replacement) {
  message("Working: ", repo_path)

  # Check if directory exists
  if (!dir.exists(repo_path)) {
    message("Error: Directory does not exist! - ", repo_path)
    return(NULL)
  }

  files <- list.files(repo_path, full.names = TRUE, recursive = TRUE) %>%
    # Ignore select subdirectories or file types
    .[!grepl("\\/deprecated\\/|\\/Repo\\/|\\/Repo_old\\/|\\/man\\/|\\.pdf$", .)]

  # Check if any files present in directory
  if(!length(files)){
    message("Error: No files found in directory.")
    return(NULL)
  }
  message("Detected ", length(files)," files")

  # Loop through each file in list
  out = lapply(files, function(file){
    message("Processing file: ", file %>% gsub(repo_path, "", ., fixed = TRUE),
            " (", which(file == files), " of ", length(files), ")")

    # Store original contents
    original_contents <- readLines(file, warn = FALSE)
    # message("Original Contents:")
    # cat(paste(original_contents, collaspe = "\n"))
    # Make pattern replacement
    xfun::gsub_file(file, pattern, replacement)

    # Store updated contents
    updated_contents <- readLines(file, warn = FALSE)
    # message("Updated contents:\n", paste(updated_contents, collaspe = "\n"))

    # Return dataframe comparing old to new file content for logging
    return(data.frame(filepath = file,
                      filename = basename(file),
                      pattern = pattern,
                      replacement = replacement,
                      file_changed = !identical(original_contents, updated_contents)))
  }) %>%
    dplyr::bind_rows()

  message("Completed replacements in: ", repo_path)
  # Export log of file replacements
  writexl::write_xlsx(out, paste0(toxval.config()$datapath,
                                  "log/replace_text_func_log", Sys.Date(), ".xlsx"))
}

# # [Testing]
# test_repo <- getwd()
#
# # [Testing] Running replacement on test directory: 'test_repo'
# replace_text_func(repo_path = test_repo,
#                   pattern = "critical_effect",
#                   replacement = "toxicological_effect")
