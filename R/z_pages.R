animint2pages <- function(plot.list, github_repo, commit_message = "Commit from animint2pages", ...) {
  res <- animint2dir(plot.list, open.browser = FALSE, ...)
  # Ensure required packages are installed.
  if (!requireNamespace("git2r")) {
    stop(
      "Please run \n",
      "install.packages('git2r')",
      "before using this function"
    )
  }
  if (!requireNamespace("webshot")) {
    stop(
      "Please run \n",
      "install.packages('webshot')",
      "before using this function"
    )
  }
  # The below are copied from `animint2gist`
  # but we don't need a flat file structure now
  ## Figure out which files to post.
  all.files <- Sys.glob(file.path(res$out.dir, "*"))
  all.file.info <- file.info(all.files)
  is.empty <- all.file.info$size == 0
  is.tilde <- grepl("~$", all.files)
  is.ignored <- all.file.info$isdir | is.empty | is.tilde
  to.post <- all.files[!is.ignored]

  tmp_dir <- tempfile()
  # check if the repo already exists on GH
  gh_repos <- "/" + user_name + "/" + github_repo
  repo_exists <- any(sapply(github_repo, function(x) x$name) == github_repo)
  if(!repo_exists){
    gh("POST /user/repos", name = github_repo)
  }
  repo <- git2r::repository(tmp_dir)

  # TODO: Take a screenshot and save as Capture.PNG.
  # Commit the changes
  lapply(to.post, function(file) git2r::add(repo, file))
  git2r::commit(repo, commit_message)

  # Push the changes to the remote repository, replace 'credentials' with your GitHub credentials
  git2r::push(repo, credentials = git2r::cred_user_pass("username", "password"))

  repo
}
