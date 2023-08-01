animint2pages <- function(plot.list, user_name, github_repo, commit_message = "Commit from animint2pages", ...) {
  res <- animint2dir(plot.list, open.browser = FALSE, ...)
  if (!requireNamespace("git2r")) {
    stop(
      "Please run \n",
      "`install.packages('git2r')` ",
      "before using this function"
    )
  }

  # The below are copied from `animint2gist`
  ## Figure out which files to post.
  all.files <- Sys.glob(file.path(res$out.dir, "*"))
  all.file.info <- file.info(all.files)
  is.empty <- all.file.info$size == 0
  is.tilde <- grepl("~$", all.files)
  is.ignored <- all.file.info$isdir | is.empty | is.tilde
  to.post <- all.files[!is.ignored]

  tmp_dir <- tempfile()

  # check if the repo already exists on GH
  gh_repos <- gh::gh("/user/repos")
  repo_exists <- any(sapply(gh_repos, function(x) x$name) == github_repo)
  if (!repo_exists) {
    gh::gh("POST /user/repos", name = github_repo)
    repo <- git2r::init(tmp_dir)
    git2r::config(repo, user.name = user_name)
  } else {
    github_url <- paste0("https://github.com/", user_name, "/", github_repo, ".git")
    repo <- git2r::clone(github_url, tmp_dir, credentials = git2r::cred_token())
  }

  # Check if there are any commits in the repo
  # have a initial commit to avoid error
  if (length(git2r::commits(repo)) == 0) {
    # Perform initial commit
    readme_file_path <- file.path(tmp_dir, "README.md")
    writeLines("## New Repo", readme_file_path)
    git2r::add(repo, "README.md")
    git2r::commit(repo, "Initial commit")
  }

  # Check if the 'gh-pages' branch exists, if not, create it
  branches <- git2r::branches(repo)
  if (!"gh-pages" %in% names(branches)) {
    git2r::branch_create(repo, "gh-pages")
  }

  git2r::checkout(repo, "gh-pages")
  file.copy(to.post, tmp_dir, recursive = TRUE)

  # TODO: Take a screenshot and save as Capture.PNG.
  # Commit the changes
  lapply(to.post, function(file) git2r::add(repo, file))
  git2r::commit(repo, commit_message)
  git2r::push(repo, "origin", "gh-pages", credentials = git2r::cred_token())

  repo
}
