acontext("GitHub Pages")
library(animint2)
viz <- animint(
  title="one to ten",
  source="https://github.com/animint/animint2/tree/master/tests/testthat/test-compiler-ghpages.R",
  p=ggplot(data.frame(x = 1:10, y = 1:10), aes(x, y)) +
    geom_point())

test_that("error for viz with no title", {
  viz.no.title <- viz
  viz.no.title$title <- NULL
  expect_error({
    animint2pages(viz.no.title, "no-title")
  }, "plot.list does not contain option named title, which is required by animint2pages")
})

test_that("error for viz with no source", {
  viz.no.source <- viz
  viz.no.source$source <- NULL
  expect_error({
    animint2pages(viz.no.source, "no-source")
  }, "plot.list does not contain option named source, which is required by animint2pages")
})

## The test below requires a github token with repo delete
## permission. Read
## https://github.com/animint/animint2/wiki/Testing#installation to
## see how to set that up on your local computer, or on github
## actions.
test_that("animint2pages() returns list of meta-data", {
  ## https://docs.github.com/en/rest/repos/repos?apiVersion=2022-11-28#delete-a-repository says The fine-grained token must have the following permission set: "Administration" repository permissions (write) gh api --method DELETE -H "Accept: application/vnd.github+json" -H "X-GitHub-Api-Version: 2022-11-28" /repos/OWNER/REPO
  gh::gh("DELETE /repos/animint-test/animint2pages_test_repo")
  ## https://docs.github.com/en/rest/repos/repos?apiVersion=2022-11-28#create-an-organization-repository says The fine-grained token must have the following permission set: "Administration" repository permissions (write)
  gh::gh("POST /orgs/animint-test/repos", name="animint2pages_test_repo")
  ## first run of animint2pages creates new data viz.
  result_list <- animint2pages(viz, "animint2pages_test_repo", owner="animint-test")
  result_list
  expect_match(result_list$owner_repo, "animint2pages_test_repo")
  expect_match(result_list$viz_url, "github.io/animint2pages_test_repo")
  expect_match(result_list$gh_pages_url, "animint2pages_test_repo/tree/gh-pages")
  README.md <- file.path(result_list$local_clone, "README.md")
  README.lines <- readLines(README.md)
  expected.line <- paste("##", viz$title)
  expect_identical(README.lines[1], expected.line)
  get_tsv <- function(L)Sys.glob(file.path(L$local_clone, "*tsv"))
  tsv_files_created <- get_tsv(result_list)
  expect_equal(length(tsv_files_created), 1)
  expect_Capture <- function(L){
    expect_gt(file.size(file.path(L$local_clone,"Capture.PNG")), 0)
  }
  expect_Capture(result_list)
  ## second run of animint2pages updates data viz.
  viz.more <- viz
  viz.more$five <- ggplot()+
    geom_point(aes(
      x, x),
      data=data.frame(x=1:5))
  update_list <- animint2pages(viz.more, "animint2pages_test_repo", owner="animint-test")
  tsv_files_updated <- get_tsv(update_list)
  expect_equal(length(tsv_files_updated), 2)
  expect_Capture(update_list)
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
    animint2pages(viz, github_repo = "test_repo")
  }, "A GitHub token is required to create and push to a new repository. \nTo create a GitHub token, follow these steps:\n1. Go to https://github.com/settings/tokens/new?scopes=repo&description=animint2pages\n2. Confirm your password if prompted.\n3. Ensure that the 'repo' scope is checked.\n4. Click 'Generate token' at the bottom of the page.\n5. Copy the generated token.\nAfter creating the token, you can set it up in your R environment by running: \nSys.setenv(GITHUB_PAT=\"yourGithubPAT\") \ngert::git_config_global_set(\"user.name\", \"yourUserName\") \ngert::git_config_global_set(\"user.email\", \"yourEmail\") \n", fixed=TRUE)
  do.call(Sys.setenv, as.list(env.old))
  file.copy(config.old, config.file, overwrite = TRUE)
})
