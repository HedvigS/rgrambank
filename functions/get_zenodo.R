#' Downloads files form Zenodo and stores them locally
#'
#' @param url character vector. URL string, e..g "https://zenodo.org/record/7740822/files/grambank/grambank-analysed-v1.0.zip"
#' @param exdir character vector. name of folder where the files are to be written to
#' @param drop_dir_level logical. If FALSE, a dir will exist inside the exdir that bears the name from the zip-file from Zenodo. If TRUE, this dir-evel will be removed, the contents of the zipped-file will be placed directly in exdir.
#' @author Hedvig Skirgård
#' @export

get_zenodo_dir <- function(url, exdir, drop_dir_level = TRUE){
  
#  url <- c("https://zenodo.org/record/7740822/files/grambank/grambank-analysed-v1.0.zip")
#  exdir = c("../grambank-analysed/")
#  drop_dir_level = T

#setting up a tempfile path where we can put the zipped files before unzipped to a specific location
filepath <- file.path(tempfile())

utils::download.file(file.path(url), destfile = filepath)
utils::unzip(zipfile = filepath, exdir = exdir)

if(drop_dir_level == TRUE){
#Zenodo locations contain a dir with the name of the repos and the commit in the release. This is not convenient for later scripts, so we move the contents up one level

#move dirs  
old_fn <- list.dirs(exdir, full.names = TRUE, recursive = FALSE)
old_fn_dirs <- list.dirs(old_fn, full.names = TRUE, recursive = FALSE)

for(fn_dir in 1:length(old_fn_dirs)){

#fn_dir <- 3

  fs::dir_copy(path = old_fn_dirs[fn_dir],new_path = exdir)
  unlink(old_fn_dirs[fn_dir], recursive = TRUE)
}

#move files

old_fn_files <- list.files(old_fn, full.names = TRUE, recursive = FALSE, include.dirs = FALSE)

x <- file.copy(from = old_fn_files, to = exdir)

#remove old dir
unlink(old_fn, recursive = TRUE)
}
cat(paste0("Done with downloading ", exdir, ".\n"))
}