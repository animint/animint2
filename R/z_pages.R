animint2pages <- function(plot.list, github_repo, commit_message = "Commit from animint2pages", private = FALSE, ...) {
  # Check for required packages
  if (!requireNamespace("gert")) {
    stop("Please run `install.packages('gert')` before using this function")
  }
  
  # Generate plot files
  res <- animint2dir(plot.list, open.browser = FALSE, ...)

  # Select non-ignored files to post
  all_files <- Sys.glob(file.path(res$out.dir, "*"))
  file_info <- file.info(all_files)
  to_post <- all_files[!(file_info$size == 0 | grepl("~$", all_files))]
  
  tmp_dir <- tempfile()
  
  # Get GitHub user info
  whoami <- suppressMessages(gh::gh_whoami())
  if (is.null(whoami)) {
    stop("A GitHub token is required to create and push to a new repo.")
  }
  
  # Check for existing repository
  owner <- whoami$login
  if (!check_no_github_repo(owner, github_repo)) {
    create <- gh::gh("POST /user/repos", name = github_repo, private = private)
    origin_url <- create$clone_url
    repo <- gert::git_init(path = tmp_dir)
    gert::git_remote_add(name = "origin", url = origin_url, repo = repo)
  } else {
    origin_url <- paste0("https://github.com/", owner, "/", github_repo, ".git")
    repo <- gert::git_clone(origin_url, tmp_dir)
  }
  
  if (length(git2r::commits(repo)) == 0) {
    initial_commit(tmp_dir, repo)
  }
  
  # Handle gh-pages branch
  manage_gh_pages(repo, to_post, tmp_dir, commit_message)
  message("Visualization will be available at https://", whoami$login, ".github.io/", github_repo, 
          "\nDeployment via GitHub Pages may take a few minutes...")
  
  repo
}

initial_commit <- function(tmp_dir, repo) {
  readme_file_path <- file.path(tmp_dir, "README.md")
  writeLines("## New animint visualization", readme_file_path)
    gert::git_add("README.md", repo = repo)
    gert::git_commit("Initial commit", repo = repo)
    gert::git_branch_move(branch = "master", new_branch = "main", repo = repo)
    gert::git_push(repo = repo, remote = "origin", set_upstream = TRUE)
}

manage_gh_pages <- function(repo, to_post, tmp_dir, commit_message) {
  branches <- gert::git_branch_list(local = TRUE, repo = repo)
  
  if (!"gh-pages" %in% branches$name) {
    gert::git_branch_create(repo = repo, branch = "gh-pages")
  }
  
  gert::git_branch_checkout("gh-pages", repo = repo)
  file.copy(to_post, tmp_dir, recursive = TRUE)
    gert::git_add(files = ".", repo = repo)
    gert::git_commit(message = commit_message, repo = repo)
    gert::git_push(remote = "origin", set_upstream = TRUE, repo = repo, force = TRUE)
}

check_no_github_repo <- function(owner, repo) {
  tryCatch({
    gh::gh("/repos/{owner}/{repo}", owner = owner, repo = repo)
    TRUE
  }, "http_error_404" = function(err) FALSE)
}