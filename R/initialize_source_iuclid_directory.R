# Don't run the script unless you're in the right directory
if(getwd() != "/ccte/ACToR1/ToxValDB9/Repo/iuclid"){return(cat("Wrong working directory"))}

# Create subdirectories and move files
files <- list.files()
for(f in files){
  # Don't move the script itself
  if(f == "initialize_source_iuclid_directory.R"){next}
  # Get source name
  source <- substring(f, 1, nchar(f)-16)
  # Create source directory
  dir.create(source)
  # Create source subdirectories
  for(s in c("_files", "_MySQL", "_R")){dir.create(paste0(source, "/", source, s))}
  # Move file into /[source]_files subdirectory
  file.rename(f, paste0(source, "/", source, "_files/", source, ".csv"))
}
