animint2pages <- function(plot.list, github_repo, commit_message = "Commit from animint2pages", ...) {
  
  res <- animint2dir(plot.list, open.browser = FALSE, ...)
  # Ensure required packages are installed.
  if (!requireNamespace("git2r")) {
    stop("Please run \n",
         "install.packages('git2r')",
         "before using this function")
  }
  if (!requireNamespace("webshot")) {
    stop("Please run \n",
         "install.packages('webshot')",
         "before using this function")
  }
  # The below are copied from `animint2gist`
  # use a flat file structure!
  vendor.path <- file.path(res$out.dir, "vendor")
  vendor.files <- list.files(vendor.path)
  vendor.path.files <- file.path(vendor.path, vendor.files)
  copied <- file.copy(vendor.path.files, file.path(res$out.dir, vendor.files))
  file.remove(vendor.path.files)
  file.remove(vendor.path)
  # reflect script path in index.html to reflect the change in file structure
  index.file <- file.path(res$out.dir, "index.html")
  html <- readLines(index.file)
  html <- gsub("vendor/", "", html)
  cat(html, file = index.file, sep = "\n")
  ## Figure out which files to post.
  all.files <- Sys.glob(file.path(res$out.dir, "*"))
  all.file.info <- file.info(all.files)
  is.empty <- all.file.info$size == 0
  is.tilde <- grepl("~$", all.files)
  is.png <- grepl("[.]png$", all.files)
  is.ignored <- all.file.info$isdir | is.empty | is.tilde
  ## TODO: delete the next line when gist_create can upload PNGs.
  is.ignored <- is.ignored | is.png
  to.post <- all.files[!is.ignored]

  # Clone the repository to a temporary directory.
  tmp_dir <- tempfile()
  repo <- git2r::clone(github_repo, tmp_dir)
  url <- paste0("file://", tmp_dir, "/index.html")
  
  # Take a screenshot and save as Capture.PNG.
  webshot::webshot(url, file = file.path(tmp_dir, "Capture.PNG"))

  git2r::add(repo, "*")
  git2r::commit(repo, commit_message)
  
  # Push the changes to the remote repository.
  # NOTE: This assumes that the remote named 'origin' is correctly set.
  # It also assumes the credentials are properly set in the environment
  # (SSH keys or PAT), which might not be the case in a real scenario.
  git2r::push(repo, "origin", refs = "refs/heads/main")
  
  # Return the repo for further use if needed.
  return(repo)
}
