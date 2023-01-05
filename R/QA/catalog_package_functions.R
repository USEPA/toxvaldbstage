# Function to catalog list of functions used in an input directory
catalog_package_functions <- function(pk_dir="R", exclude="deprecated"){
  # install.packages("NCmisc)
  library(dplyr); library(NCmisc)

  r_files = list.files(pk_dir, full.names = TRUE) %>%
    .[!grepl(paste0(exclude, collapse = "|"), .)]

  # Ran in 155 seconds for 297 .R files for toxval ()
  system.time({
    tmp = lapply(r_files, function(f){
      tmp0 = NCmisc::list.functions.in.file(f)
      lapply(names(tmp0), function(f_list){
        if(length(f_list))
          tmp0[[f_list]] %>%
          data.frame(func_list=.) %>%
          mutate(orig_file = f,
                 package_parent = ifelse(f_list == "character(0)", "local package", f_list))
      }) %>%
        dplyr::bind_rows() %>%
        distinct() %>%
        return()
    }) %>%
      dplyr::bind_rows()
  })

  return(tmp)
}

get_package_catalog <- function(package_name = "toxvaldb092", package_path = getwd()){
  # devtools::document() # to update NAMESPACE of deprecated function exports
  pk_catalog = catalog_package_functions(exclude=c("deprecated", "QA"))
  # Combine into dataframe
  pk_catalog = data.frame(func_name = pk_catalog$func_list,
                          orig_file = pk_catalog$orig_file,
                          parent = pk_catalog$package_parent,
                          stringsAsFactors = FALSE) %>%
    filter(!grepl("package:", parent))

  # Install package
  install.packages(package_path,
                   repos = NULL,
                   type = "source")
  # Load package
  invisible(lapply(c(package_name), require, character.only = TRUE))
  # Get list of functions in package (even private/hidden ones without @export)
  # https://stackoverflow.com/questions/8696158/find-all-functions-including-private-in-a-package
  pk_functions = ls(getNamespace(package_name), all.names=TRUE) # ls(paste0("package:", package_name))
  # Uninstall package
  remove.packages(package_name)

  # Get list of functions NOT in the pk_catalog (not used)
  dep_candidates = pk_functions[!pk_functions %in% pk_catalog$func_name]

  return(list(pk_functions=pk_functions %>% data.frame(pk_functions=.),
              pk_catalog=pk_catalog,
              deprecation_candidates = dep_candidates %>% data.frame(dep_candidates=.)))
}
