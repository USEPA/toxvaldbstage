library(xfun)

## Actual path to repository where change is desired.
# repos <- c(
#   "Path/to/stage"
#   "Path/to/main"
# )

# [Testing] Does not remain in script.
test_repo <- "test_repo"

replace_text_func <- function(repo_path, pattern, replacement) {
  message("Working: ", repo_path)
  
  # Preliminary Error Handling.
  if (!dir.exists(repo_path)) {
    message("Error: Directory does not exist!", repo_path)
    return(NULL)
  }
  files <- list.files(repo_path, full.names = TRUE, recursive = TRUE)
  message("Detected files: ")
  print(files)
  
  for (file in files) {
    message("Processing file: ", file)
    
    original_contents <- readLines(file, warn = FALSE)
    message("Original Contents:\n", paste(original_contents, collaspe = "\n"))
    gsub_file(file, pattern, replacement)
    
    updated_contents <- readLines(file, warn = FALSE)
    message("Updated contents:\n", paste(updated_contents, collaspe = "\n"))
  }
  
  ## Replacement for various occurrences. 
  # gsub_dir(
  #   pattern = pattern,
  #   replacement = replacement, 
  #   dir = repo_path, 
  #   ext = "[.R|.Rmd|.txt|.csv|.json|"
  # )
  message("Completed replacements in: ", repo_path)
}

# [Testing] Running replacement on test directory: 'test_repo'
replace_text_func(test_repo, "critical_effect", "toxicological_effect")
message("Replacements have been completed.")