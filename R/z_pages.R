#' Publish a list of ggplots as interactive visualizations on a GitHub repository
#'
#' This function takes a named list of ggplots, generates interactive animations,
#' and pushes the generated files to a specified GitHub repository. You can
#' choose to keep the repository private or public.
#' Before using this function set your appropriate git 'user.username' and 'user.email'
#'
#' @param plot.list A named list of ggplots and option lists.
#' @param github_repo The name of the GitHub repository to which the files will be pushed.
#' @param commit_message A string specifying the commit message for the pushed files.
#' @param private A logical flag indicating whether the GitHub repository should be private or not.
#' @param ... Additional options passed onto \code{animint2dir}.
#'
#' @return The function returns the initialized GitHub repository object.
#'
#' @examples
#' \dontrun{
#' library(animint2)
#' p1 <- ggplot(mtcars, aes(x = mpg, y = wt)) +
#'   geom_point()
#' p2 <- ggplot(mtcars, aes(x = hp, y = wt)) +
#'   geom_point()
#' viz <- list(plot1 = p1, plot2 = p2)
#' animint2pages(viz, github_repo = "my_animint2_plots", commit_message = "New animint", private = TRUE)
#' }
#'
#' @export
animint2pages <- function(plot.list, github_repo, commit_message = "Commit from animint2pages", private = FALSE, ...) {

  # Check for required packages
  if (!requireNamespace("gert")) {
    stop("Please run `install.packages('gert')` before using this function")
  }

  if (!requireNamespace("gh")) {
    stop("Please run `install.packages('gh')` before using this function")
  }

  # Generate plot files
  res <- animint2dir(plot.list, open.browser = FALSE, ...)

  # Select non-ignored files to post
  all_files <- Sys.glob(file.path(res$out.dir, "*"))
  file_info <- file.info(all_files)
  to_post <- all_files[!(file_info$size == 0 | grepl("~$", all_files))]

  tmp_dir <- tempfile()

  tryCatch(
    {
      creds <- gitcreds::gitcreds_get()
    },
    error = function(e) {
      stop(
        "A GitHub token is required to create and push to a new repository. \n",
        "To create a GitHub token, follow these steps:\n",
        "1. Go to https://github.com/settings/tokens/new?scopes=repo&description=animint2pages\n",
        "2. Confirm your password if prompted.\n",
        "3. Ensure that the 'repo' scope is checked.\n",
        "4. Click 'Generate token' at the bottom of the page.\n",
        "5. Copy the generated token.\n",
        "After creating the token, you can set it up in your R environment by running: \n",
        "Sys.setenv(GITHUB_PAT=\"yourGithubPAT\") \n",
        "gert::git_config_global_set(\"user.name\", \"yourUserName\") \n",
        "gert::git_config_global_set(\"user.email\", \"yourEmail\") \n"
      )
    }
  )

  # Raise error if github_repo contains '/'
  if (grepl("/", github_repo)) {
    stop("The github_repo argument should not contain '/'.")
  }
  
  # Check for existing repository
  whoami <- suppressMessages(gh::gh_whoami())
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

  viz_url <- paste0("https://", whoami$login, ".github.io/", github_repo)
  if (length(git2r::commits(repo)) == 0) {
    initial_commit(tmp_dir, repo, viz_url)
  }

  # Handle gh-pages branch
  manage_gh_pages(repo, to_post, tmp_dir, commit_message)
  message(
    "Visualization will be available at ", viz_url,
    "\nDeployment via GitHub Pages may take a few minutes..."
  )

  repo
}

initial_commit <- function(tmp_dir, repo, viz_url) {
  readme_file_path <- file.path(tmp_dir, "README.md")
  header <- "## New animint visualization\n"
  url_hyperlink <- sprintf("[%s](%s)\n", viz_url, viz_url)
  full_content <- paste0(header, url_hyperlink)
  writeLines(full_content, readme_file_path)

  gert::git_add("README.md", repo = repo)
  gert::git_commit("Initial commit", repo = repo)
  df_or_vec <- gert::git_branch(repo)
  # check if it is a data frame or an atomic vector
  if (is.data.frame(df_or_vec)) {
    all_branches <- df_or_vec[["name"]]
    current_master <- all_branches[df_or_vec$active]
  } else {
    all_branches <- df_or_vec
    current_master <- df_or_vec
  }
  # do not attempt to rename a branch to "main" when a branch with that name already exists
  if (current_master != "main" && !"main" %in% all_branches) {
    gert::git_branch_move(branch = current_master, new_branch = "main", repo = repo)
  }
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
  tryCatch(
    {
      gh::gh("/repos/{owner}/{repo}", owner = owner, repo = repo)
      TRUE
    },
    "http_error_404" = function(err) FALSE
  )
}
