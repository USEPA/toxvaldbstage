# ls the folder containing all R scripts
file_list = list.files("R", full.names=TRUE)

# remove deprecated folder
file_list <- file_list[which(file_list != 'R/deprecated')]

# load all R functions in the toxvaldb repo by source()-ing each R file
invisible(sapply(file_list, source,.GlobalEnv))

# trim script names down to more easily match function names
file_list_trimmed <- substring(file_list, 3, nchar(file_list) - 2)

# compare lists of functions and files
extra_functions <- fn_list[which(!(fn_list %in% file_list_trimmed))]

# list parents of unmatched functions (https://stackoverflow.com/a/32749240)
parent_files <- sapply(extra_functions,
                       function(x){attr(attr(get(x), 'srcref'), 'srcfile')$filename})
