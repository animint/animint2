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
#' @param required_opts Character vector of plot.list element names which are checked (stop with an error if not present). Use required_opts=NULL to skip check.
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
#' animint2pages(
#'   viz,
#'   github_repo = "my_animint2_plots",
#'   commit_message = "New animint",
#'   private = TRUE)
#' }
#'
#' @export
animint2pages <- function(plot.list, github_repo, commit_message = "Commit from animint2pages", private = FALSE, required_opts = c("title","source"), ...) {

  for(opt in required_opts){
    if(!opt %in% names(plot.list)){
      stop(sprintf("plot.list does not contain option named %s, which is required by animint2pages", opt))
    }
  }

  # Check for required packages
  for(pkg in c("gert", "gh")){
    if (!requireNamespace(pkg)) {
      stop(sprintf("Please run `install.packages('%s')` before using this function", pkg))
    }
  }

  # Generate plot files
  res <- animint2dir(plot.list, open.browser = FALSE, ...)

  # Select non-ignored files to post
  all_files <- Sys.glob(file.path(res$out.dir, "*"))
  file_info <- file.info(all_files)
  to_post <- all_files[!(file_info$size == 0 | grepl("~$", all_files))]

  tmp_dir <- tempfile()

  tryCatch({
    creds <- gitcreds::gitcreds_get()
  }, error = function(e) stop("A GitHub token is required to create and push to a new repository. \nTo create a GitHub token, follow these steps:\n1. Go to https://github.com/settings/tokens/new?scopes=repo&description=animint2pages\n2. Confirm your password if prompted.\n3. Ensure that the 'repo' scope is checked.\n4. Click 'Generate token' at the bottom of the page.\n5. Copy the generated token.\nAfter creating the token, you can set it up in your R environment by running: \nSys.setenv(GITHUB_PAT=\"yourGithubPAT\") \ngert::git_config_global_set(\"user.name\", \"yourUserName\") \ngert::git_config_global_set(\"user.email\", \"yourEmail\") \n"))

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
  # check if repo has commit, if not, give it first commit, this can avoid error
  has_commits <- FALSE
  try(
    {
      if (nrow(gert::git_log(repo = repo)) > 0) {
        has_commits <- TRUE
      }
    },
    silent = TRUE
  )
  if (!has_commits) {
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

get_pages_info <- function(viz_user_repo){
  viz_dir <- tempfile()
  origin_url <- paste0("https://github.com/", viz_user_repo, ".git")
  gert::git_clone(origin_url, viz_dir)
  gert::git_branch_checkout("gh-pages", repo=viz_dir)
  Capture.PNG <- file.path(viz_dir, "Capture.PNG")
  if(!file.exists(Capture.PNG)){
    stop(sprintf("gh-pages branch of %s should contain file named Capture.PNG (screenshot of data viz)", viz_user_repo))
  }
  plot.json <- file.path(viz_dir, "plot.json")
  jlist <- RJSONIO::fromJSON(plot.json)
  commit.row <- gert::git_log(max=1, repo=viz_dir)
  repo.row <- data.table(
    viz_user_repo, Capture.PNG, commit.POSIXct=commit.row$time)
  to.check <- c(
    source="URL of data viz source code",
    title="string describing the data viz")
  for(attr.name in names(to.check)){
    attr.value <- jlist[[attr.name]]
    if(
      is.character(attr.value)
      && length(attr.value)==1
      && !is.na(attr.value)
      && nchar(attr.value)>0
    ){
      set(repo.row, j=attr.name, value=attr.value)
    }else{
      stop(sprintf("plot.json file in gh-pages branch of %s should have element named %s which should be %s", viz_user_repo, attr.name, to.check[[attr.name]]))
    }
  }
  repo.row
}

##' A gallery is a collection of animints that have been published to
##' github pages. First repos.txt is read, then we clone each repo
##' which is not already present in meta.csv, and parse meta-data
##' (title, source, Capture.PNG) from the gh-pages branch, and
##' write/commit the data, re-render index.Rmd in gallery, and push
##' gallery to origin.
##' @title Update gallery
##' @param gallery_path path to local github repo with gh-pages active.
##' @return named list of data tables (meta and error).
##' @author Toby Dylan Hocking
##' @export
update_gallery <- function(gallery_path="~/R/gallery"){
  repos.txt <- file.path(gallery_path, "repos.txt")
  repos.dt <- fread(repos.txt,header=FALSE,col.names="viz_user_repo")
  meta.csv <- file.path(gallery_path, "meta.csv")
  old.meta <- fread(meta.csv)
  todo.meta <- repos.dt[!old.meta, on="viz_user_repo"]
  meta.dt.list <- list(old.meta)
  error.dt.list <- list()
  add.POSIXct <- Sys.time()
  for(viz_user_repo in todo.meta[["viz_user_repo"]]){
    tryCatch({
      meta.row <- data.table(add.POSIXct, get_pages_info(viz_user_repo))
      meta.dt.list[[viz_user_repo]] <- meta.row
      Capture.PNG <- meta.row[["Capture.PNG"]]
      repo.png <- file.path(
        gallery_path, "repos", paste0(viz_user_repo, ".png"))
      user.dir <- dirname(repo.png)
      dir.create(user.dir, showWarnings = FALSE, recursive = TRUE)
      file.copy(Capture.PNG, repo.png, overwrite = TRUE)
    }, error=function(e){
      error.dt.list[[viz_user_repo]] <<- data.table(
        add.POSIXct, viz_user_repo, error=e$message)
    })
  }
  (meta.dt <- rbindlist(meta.dt.list))
  (error.dt <- rbindlist(error.dt.list))
  fwrite(meta.dt, meta.csv)
  fwrite(error.dt, file.path(gallery_path, "error.csv"))
  rmarkdown::render(file.path(gallery_path, "index.Rmd"))
  to_add <- c("*.csv", file.path("repos","*","*.png"), "index.html")
  gert::git_add(to_add, repo=gallery_path)
  gert::git_commit(paste("update", add.POSIXct), repo=gallery_path)
  gert::git_push("origin", repo=gallery_path)
  list(meta=meta.dt, error=error.dt)
}
