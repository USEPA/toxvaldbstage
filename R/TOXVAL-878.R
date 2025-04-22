library(xfun)

## Actual path to repository where change is desired.
# repos <- c(
#   "Path/to/stage"
#   "Path/to/main"
# )

# [Testing] Does not remain in script
test_repo <- "test_repo"

replace_text_func <- function(repo_path, pattern, replacement, rename_files = FALSE) {
  message("Working: ", repo_path)

  # Preliminary Error Handling.
  if (!dir.exists(repo_path)) {
    message("Error: Directory does not exist!", repo_path)
    return(NULL)
  }

  files <- list.files(
    path = repo_path,
    pattern = "\\.(R|Rmd|txt|csv|json)$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
    )

    message("Detected files: ")
    print(files)

  log_df <- data.frame(
    file_path = character(),
    file_renamed = logical(),
    filename_orginal = character(),
    filename_final = character(),
    stringsAsFactors = FALSE
  )

  for (file in files) {
    message("Processing file: ", file)

    original_contents <- readLines(file, warn = FALSE)
    message("Original Contents:\n", paste(original_contents, collaspe = "\n"))

    xfun::gsub_file(file, pattern, replacement)

    updated_contents <- readLines(file, warn = FALSE)
    message("Updated contents:\n", paste(updated_contents, collaspe = "\n"))

    orginal_filename <- basename(file)
    new_filename <- sub(pattern, replacement, orginal_filename)

    file_renamed <- FALSE

    final_path <- file

    if (rename_files && orginal_filename != new_filename) {
      new_path <- file.path(dirname(file), new_filename)
      file.rename(file, new_path)
      file_renamed <- TRUE
      final_path <- new_path
    }

    log_df <- rbind(log_df, data.frame(
      file_path = file,
      file_renamed = file_renamed,
      filename_orignal = orginal_filename,
      filename_final = basename(final_path),
      stingsAsFactors = FALSE
    ))

  }

  message("Completed replacements in: ", repo_path)
  return(log_df)
}
