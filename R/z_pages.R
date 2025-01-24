#' Publish a list of ggplots as interactive visualizations on a GitHub repository
#'
#' This function takes a named list of ggplots, generates interactive animations,
#' and pushes the generated files to a specified GitHub repository. You can
#' choose to keep the repository private or public.
#' Before using this function set your appropriate git 'user.username' and 'user.email'
#'
#' @param plot.list A named list of ggplots and option lists.
#' @param github_repo The name of the GitHub repository to which the
#'   files will be pushed.
#' @param owner The user/org under which the repo will be created, default comes from \code{gh::gh_whoami}.
#' @param commit_message A string specifying the commit message for
#'   the pushed files.
#' @param private A logical flag indicating whether the GitHub
#'   repository should be private or not (default FALSE).
#' @param required_opts Character vector of plot.list element names
#'   which are checked (stop with an error if not present). Use
#'   required_opts=NULL to skip check.
#' @param chromote_sleep_seconds if numeric, chromote will be used to take a screenshot of the data viz, pausing this number of seconds to wait for rendering (experimental).
#' @param ... Additional options passed onto \code{animint2dir}.
#'
#' @return The function returns the initialized GitHub repository object.
#'
#' @examples
#' \dontrun{
#' library(animint2)
#' mtcars$Cyl <- factor(mtcars$cyl)
#' viz <- animint(
#'   ggplot(mtcars, aes(x = mpg, y = disp, color=Cyl)) +
#'     geom_point(),
#'   ggplot(mtcars, aes(x = hp, y = wt, color=Cyl)) +
#'     geom_point(),
#'   title="Motor Trend Cars data viz",
#'   source="https://github.com/animint/animint2/blob/master/R/z_pages.R"
#' )
#' animint2pages(viz, "animint2pages-example-mtcars")
#' }
#' 
#' @export
animint2pages <- function(plot.list, github_repo, owner=NULL, commit_message = "Commit from animint2pages", private = FALSE, required_opts = c("title","source"), chromote_sleep_seconds=NULL, ...) {
  for(opt in required_opts){
    if(!opt %in% names(plot.list)){
      stop(sprintf("plot.list does not contain option named %s, which is required by animint2pages", opt))
    }
  }
  ## Check for required packages
  for(pkg in c("gert", "gh")){
    if (!requireNamespace(pkg)) {
      stop(sprintf("Please run `install.packages('%s')` before using this function", pkg))
    }
  }
  res <- animint2dir(plot.list, open.browser = FALSE, ...)
  if(requireNamespace("chromote") && requireNamespace("magick") && is.numeric(chromote_sleep_seconds)) {
    chrome.session <- chromote::ChromoteSession$new()
    #Find available port and start server
    portNum <- servr::random_port()
    normDir <- normalizePath(res$out.dir, winslash = "/", mustWork = TRUE)
    start_servr(serverDirectory = normDir, port = portNum, tmpPath = normDir)
    Sys.sleep(chromote_sleep_seconds)
    url <- sprintf("http://localhost:%d", portNum)
    chrome.session$Page$navigate(url)
    screenshot_path <- file.path(res$out.dir, "Capture.PNG")
    screenshot_full <- file.path(res$out.dir, "Capture_full.PNG")
    Sys.sleep(chromote_sleep_seconds)
    ## Capture screenshot
    chrome.session$screenshot(screenshot_full, selector = ".plot_content")
    image_raw <- magick::image_read(screenshot_full)
    image_trimmed <- magick::image_trim(image_raw)
    magick::image_write(image_trimmed, screenshot_path)
    unlink(screenshot_full)
    chrome.session$close()
    # Stop the server
    stop_servr(normDir)
  }
  all_files <- Sys.glob(file.path(res$out.dir, "*"))
  file_info <- file.info(all_files)
  to_post <- all_files[!(file_info$size == 0 | grepl("~$", all_files))]
  tryCatch({
    gitcreds::gitcreds_get()
  }, error = function(e) stop("A GitHub token is required to create and push to a new repository. \nTo create a GitHub token, follow these steps:\n1. Go to https://github.com/settings/tokens/new?scopes=repo&description=animint2pages\n2. Confirm your password if prompted.\n3. Ensure that the 'repo' scope is checked.\n4. Click 'Generate token' at the bottom of the page.\n5. Copy the generated token.\nAfter creating the token, you can set it up in your R environment by running: \nSys.setenv(GITHUB_PAT=\"yourGithubPAT\") \ngert::git_config_global_set(\"user.name\", \"yourUserName\") \ngert::git_config_global_set(\"user.email\", \"yourEmail\") \n"))
  # Raise error if github_repo contains '/'
  if (grepl("/", github_repo)) {
    stop("The github_repo argument should not contain '/'.")
  }
  # Check for existing repository
  if(is.null(owner)){
    whoami <- suppressMessages(gh::gh_whoami())
    owner <- whoami[["login"]]
  }
  viz_owner_repo <- paste0(owner, "/", github_repo)
  local_clone <- tempfile()
  if (!check_no_github_repo(owner, github_repo)) {
    create <- gh::gh("POST /user/repos", name = github_repo, private = private)
    origin_url <- create$clone_url
    repo <- gert::git_init(path = local_clone)
    gert::git_remote_add(name = "origin", url = origin_url, repo = repo)
  } else {
    origin_url <- paste0("https://github.com/", viz_owner_repo, ".git")
    repo <- gert::git_clone(origin_url, local_clone)
  }
  viz_url <- paste0("https://", owner, ".github.io/", github_repo)
  ## check if repo has commit, if not, give it first commit, this can avoid error
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
    title <- plot.list[["title"]]
    if(!is.character(title))title <- "New animint visualization"
    initial_commit(local_clone, repo, viz_url, title)
  }
  ## Handle gh-pages branch
  manage_gh_pages(repo, to_post, local_clone, commit_message)
  message(sprintf(
    "Visualization will be available at %s\nDeployment via GitHub Pages may take a few minutes...", viz_url))
  list(owner_repo=viz_owner_repo, local_clone=local_clone, viz_url=viz_url, gh_pages_url=sprintf("https://github.com/%s/tree/gh-pages", viz_owner_repo))
}

initial_commit <- function(local_clone, repo, viz_url, title) {
  readme_file_path <- file.path(local_clone, "README.md")
  header <- sprintf("## %s\n", title)
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

manage_gh_pages <- function(repo, to_post, local_clone, commit_message) {
  branches <- gert::git_branch_list(local = TRUE, repo = repo)
  if (!"gh-pages" %in% branches$name) {
    gert::git_branch_create(repo = repo, branch = "gh-pages")
  }
  gert::git_branch_checkout("gh-pages", repo = repo)
  file.copy(to_post, local_clone, recursive = TRUE)
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

##' A gallery is a collection of meta-data about animints that have
##' been published to github pages. A gallery is defined as a github
##' repo that should have two source files in the gh-pages branch:
##' repos.txt (list of github repositories, one owner/repo per line)
##' and index.Rmd (source for web page with links to animints). To
##' perform the update, first repos.txt is read, then we clone each
##' repo which is not already present in meta.csv, and parse meta-data
##' (title, source, Capture.PNG) from the gh-pages branch, and write
##' the meta.csv/error.csv/Capture.PNG files, render index.Rmd to
##' index.html, commit, and push origin. For an example, see the main
##' gallery, \url{https://github.com/animint/gallery/tree/gh-pages}
##' which is updated using this function.
##' @title Update gallery
##' @param gallery_path path to local github repo with gh-pages
##'   active.
##' @return named list of data tables (meta and error).
##' @author Toby Dylan Hocking
##' @importFrom utils download.file
##' @export
update_gallery <- function(gallery_path="~/R/gallery"){
  commit.POSIXct <- title <- NULL
  ## Above to avoid CRAN NOTE.
  repos.txt <- file.path(gallery_path, "repos.txt")
  repos.dt <- fread(repos.txt,header=FALSE,col.names="viz_owner_repo")
  meta.csv <- file.path(gallery_path, "meta.csv")
  if(file.exists(meta.csv)){
    old.meta <- fread(meta.csv)
    todo.meta <- repos.dt[!old.meta, on="viz_owner_repo"]
  }else{
    old.meta <- NULL
    todo.meta <- repos.dt
  }
  meta.dt.list <- list(old.meta)
  error.dt.list <- list()
  add.POSIXct <- Sys.time()
  for(viz_owner_repo in todo.meta[["viz_owner_repo"]]){
    tryCatch({
      viz_url <- function(filename)sprintf(
        "https://raw.githubusercontent.com/%s/refs/heads/gh-pages/%s",
        viz_owner_repo, filename)
      repo.png <- file.path(
        gallery_path, "repos", paste0(viz_owner_repo, ".png"))
      if(!file.exists(repo.png)){
        download.file(viz_url("Capture.PNG"), repo.png)
      }
      local.json <- tempfile()
      download.file(viz_url("plot.json"), local.json)
      jlist <- RJSONIO::fromJSON(local.json)
      to.check <- c(
        source="URL of data viz source code",
        title="string describing the data viz")
      repo.row <- data.table()
      repo.row$video <- if("video" %in% names(jlist)){
        jlist$video
      }else{
        NA_character_
      }
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
          stop(sprintf("plot.json file in gh-pages branch of %s should have element named %s which should be %s", viz_owner_repo, attr.name, to.check[[attr.name]]))
        }
      }
      meta.dt.list[[viz_owner_repo]] <- data.table(
        add.POSIXct, viz_owner_repo, repo.row)
    }, error=function(e){
      error.dt.list[[viz_owner_repo]] <<- data.table(
        add.POSIXct, viz_owner_repo, error=e$message)
    })
  }
  (meta.dt <- rbindlist(meta.dt.list))
  (error.dt <- rbindlist(error.dt.list))
  fwrite(meta.dt, meta.csv)
  fwrite(error.dt, file.path(gallery_path, "error.csv"))
  rmarkdown::render(file.path(gallery_path, "index.Rmd"))
  to_add <- c(
    "*.csv",
    "repos.txt",
    file.path("repos","*","*.png"),
    "index.html",
    "index.Rmd")
  gert::git_add(to_add, repo=gallery_path)
  gert::git_commit(paste("update", add.POSIXct), repo=gallery_path)
  gert::git_push("origin", repo=gallery_path)
  list(meta=meta.dt, error=error.dt)
}
