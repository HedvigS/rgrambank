library(fs)

get_zenodo_dir <- function(url, exdir, remove_git_commit_dir = T){
  
#  url <- c("https://zenodo.org/record/7740822/files/grambank/grambank-analysed-v1.0.zip")
#  exdir = c("../grambank-analysed/")
#  remove_git_commit_dir = T
#setting up a tempfile path where we can put the zipped files before unzipped to a specific location
filepath <- file.path(tempfile())

utils::download.file(file.path(url), destfile = filepath)
utils::unzip(zipfile = filepath, exdir = exdir)

if(remove_git_commit_dir == T){
#Zenodo locations contain a dir with the name of the repos and the commit in the release. This is not convenient for later scripts, so we move the contents up one level

#move dirs  
old_fn <- list.dirs(exdir, full.names = T, recursive = F)
old_fn_dirs <- list.dirs(old_fn, full.names = T, recursive = F)

for(fn_dir in 1:length(old_fn_dirs)){

#fn_dir <- 3

  fs::dir_copy(path = old_fn_dirs[fn_dir],new_path = exdir)
  unlink(old_fn_dirs[fn_dir], recursive = T)
}

#move files

old_fn_files <- list.files(old_fn, full.names = T, recursive = F, include.dirs = F)

x <- file.copy(from = old_fn_files, to = exdir)

#remove old dir
unlink(old_fn, recursive = T)
}
cat(paste0("Done with downloading ", exdir, ".\n"))
}