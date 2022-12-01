# QA script to find functions not named the same as their source file, as
# per he toxvaldb09 naming convention

qa_function_file_name_check <- function(){
  # ls the folder containing all R scripts
  file_list = list.files("R", full.names=TRUE)
  
  # remove deprecated and QA folder
  file_list <- file_list[!file_list %in% c('R/deprecated', 'R/QA')]
  
  # load all R functions in the toxvaldb repo by source()-ing each R file
  invisible(sapply(file_list, source,.GlobalEnv))
  
  # get basename without file extension (functions supposed to be same name)
  file_list_trimmed <- tools::file_path_sans_ext(basename(file_list))
  
  #https://stackoverflow.com/questions/5103194/get-the-list-of-functions-loaded-in-rs-global-environment
  fn_list <- ls()[sapply(ls(), function(obj) "function"==class(eval(parse(text = obj)))[1])]
  
  # compare lists of functions and files
  extra_functions <- fn_list[which(!(fn_list %in% file_list_trimmed))]
  
  # list parents of unmatched functions (https://stackoverflow.com/a/32749240)
  parent_files <- sapply(extra_functions,
                         function(x){attr(attr(get(x), 'srcref'), 'srcfile')$filename})
  # See results
  View(parent_files)
}