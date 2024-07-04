acontext("GitHub Pages")
library(gh)

server <- function() {
  out.dir <- tempfile()
  
  dir.create(out.dir, showWarnings=FALSE, recursive=TRUE)
  portNum <- 8080
  normDir <- normalizePath(out.dir, winslash = "/", mustWork = TRUE)
  code = sprintf("servr::httd(dir='%s', port=%d)", normDir,portNum)
  system2("Rscript", c("-e", shQuote(code)), wait = FALSE)
  #list(out.dir)
  return(list(output.dir = normDir))
}

s<-server()
url <- sprintf("http://localhost:%d",8080)


viz <- animint(
  title="one to ten",
  source="https://github.com/animint/animint2/tree/master/tests/testthat/test-compiler-ghpages.R",
  p=ggplot(data.frame(x = 1:10, y = 1:10), aes(x, y)) +
    geom_point())

test_that("error for viz with no title", {
  #browseURL(url)
  viz.no.title <- viz
  viz.no.title$title <- NULL
  expect_error({
    animint2pages(viz.no.title, "no-title",server=s)
  }, "plot.list does not contain option named title, which is required by animint2pages")
})

test_that("error for viz with no source", {
  #browseURL(url)
  viz.no.source <- viz
  viz.no.source$source <- NULL
  expect_error({
    animint2pages(viz.no.source, "no-source",server=s)
  }, "plot.list does not contain option named source, which is required by animint2pages")
})

test_that("animint2pages() returns owner/repo string", {
  #browseURL(url)
  viz_owner_repo <- animint2pages(viz, github_repo = "animint2pages_test_repo",server=s)
  expect_is(viz_owner_repo, "character")
})

test_that("check if animint2pages() successfully uploads screenshot", {

  viz_owner_repo <- animint2pages(viz, github_repo = "animint2pages_test_repo",server=s)
  split_viz_owner_repo <- strsplit(viz_owner_repo, "/")[[1]]
  repo_owner <- split_viz_owner_repo[1]
  repo_name <- split_viz_owner_repo[2]
  
  file_exists <- {
    gh("GET /repos/:owner/:repo/contents/:path",
       owner = repo_owner, repo = repo_name, path = "screenshot.png",ref ="gh-pages")
    TRUE  # If the call succeeds, the file exists
  }
  expect_true(file_exists, info = "The screenshot should exist in the repository.")
})

test_that("animint2pages raises an error if no GitHub token is present", {
  env.names <- c("GITHUB_PAT", "GITHUB_PAT_GITHUB_COM")
  env.old <- Sys.getenv(env.names)
  Sys.unsetenv(env.names)
  ## removing env vars is necessary but not sufficient for this test,
  ## because if they do not exist, then gitcreds::gitcreds_get() will
  ## be called to set the env vars/token.
  repo.root <- system("git rev-parse --show-toplevel", intern=TRUE)
  config.file <- file.path(repo.root, ".git", "config")
  config.old <- file.path(repo.root, ".git", "config.old")
  file.copy(config.file, config.old, overwrite = TRUE)
  cat("[credential]\n\tusername = FOO", file=config.file, append=TRUE)
  expect_error({
    animint2pages(viz, github_repo = "test_repo",server=s)
  }, "A GitHub token is required to create and push to a new repository. \nTo create a GitHub token, follow these steps:\n1. Go to https://github.com/settings/tokens/new?scopes=repo&description=animint2pages\n2. Confirm your password if prompted.\n3. Ensure that the 'repo' scope is checked.\n4. Click 'Generate token' at the bottom of the page.\n5. Copy the generated token.\nAfter creating the token, you can set it up in your R environment by running: \nSys.setenv(GITHUB_PAT=\"yourGithubPAT\") \ngert::git_config_global_set(\"user.name\", \"yourUserName\") \ngert::git_config_global_set(\"user.email\", \"yourEmail\") \n", fixed=TRUE)
  do.call(Sys.setenv, as.list(env.old))
  file.copy(config.old, config.file, overwrite = TRUE)
})
