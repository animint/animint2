acontext("GitHub Pages")
library(animint2)
test_repo_name <- sprintf(
  "animint2pages_test_repo_%s",
  tolower(Sys.getenv("TEST_SUITE", unset = "local"))
)
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

get_tsv <- function(L)Sys.glob(file.path(L$local_clone, "*tsv"))
expect_Capture <- function(L){
  expect_gt(file.size(file.path(L$local_clone,"Capture.PNG")), 0)
}
expect_no_Capture <- function(L){
  expect_false(file.exists(file.path(L$local_clone,"Capture.PNG")))
}
## The test below requires a github token with repo delete
## permission. Read
## https://github.com/animint/animint2/wiki/Testing#installation to
## see how to set that up on your local computer, or on github
## actions.
reset_test_repo <- function(){
  ## https://docs.github.com/en/rest/repos/repos?apiVersion=2022-11-28#delete-a-repository says The fine-grained token must have the following permission set: "Administration" repository permissions (write) gh api --method DELETE -H "Accept: application/vnd.github+json" -H "X-GitHub-Api-Version: 2022-11-28" /repos/OWNER/REPO
  tryCatch({
    gh::gh(sprintf("DELETE /repos/animint-test/%s", test_repo_name))
  }, http_error_404=function(e){
    print(e) #in case it is already deleted.
    ##<github_error/http_error_404/rlang_error/error/condition>
    ##Error in gh::gh(DELETE .../REPO): GitHub API error (404): Not Found
  })
  ## https://docs.github.com/en/rest/repos/repos?apiVersion=2022-11-28#create-an-organization-repository says The fine-grained token must have the following permission set: "Administration" repository permissions (write)
  gh::gh("POST /orgs/animint-test/repos", name = test_repo_name)
  Sys.sleep(3)
}
test_that("ghpages test repo name is unique per CI job", {
  name_r <- sprintf("animint2pages_test_repo_%s", tolower("R_coverage"))
  name_js <- sprintf("animint2pages_test_repo_%s", tolower("JS_coverage"))
  expect_false(identical(name_r, name_js))
  expect_identical(test_repo_name, sprintf("animint2pages_test_repo_%s", tolower(Sys.getenv("TEST_SUITE", unset = "local"))))
  expect_false(identical(test_repo_name, "animint2pages_test_repo"))
})
test_that("animint2pages(chromote_sleep_seconds=3) creates Capture.PNG", {
  reset_test_repo()
  ## first run of animint2pages creates new data viz.
  result_list <- animint2pages(viz, test_repo_name, owner="animint-test", chromote_sleep_seconds=3)
  result_list
  expect_match(result_list$owner_repo, test_repo_name, fixed = TRUE)
  expect_match(result_list$viz_url, sprintf("github.io/%s", test_repo_name), fixed = TRUE)
  expect_match(result_list$gh_pages_url, sprintf("%s/tree/gh-pages", test_repo_name), fixed = TRUE)
  README.md <- file.path(result_list$local_clone, "README.md")
  README.lines <- readLines(README.md)
  expected.line <- paste("##", viz$title)
  expect_identical(README.lines[1], expected.line)
  tsv_files_created <- get_tsv(result_list)
  expect_equal(length(tsv_files_created), 1)
  expect_Capture(result_list)
  ## second run of animint2pages updates data viz.
  viz.more <- viz
  viz.more$five <- ggplot()+
    geom_point(aes(
      x, x),
      data=data.frame(x=1:5))
  update_list <- animint2pages(viz.more, test_repo_name, owner="animint-test", chromote_sleep_seconds=3)
  tsv_files_updated <- get_tsv(update_list)
  expect_equal(length(tsv_files_updated), 2)
  expect_Capture(update_list)
})

test_that("animint2pages(chromote_sleep_seconds=NULL) does not create Capture.PNG", {
  reset_test_repo()
  Sys.sleep(3)
  result_list <- animint2pages(viz, test_repo_name, owner="animint-test", chromote_sleep_seconds=NULL)
  Sys.sleep(3)
  expect_match(result_list$owner_repo, test_repo_name, fixed = TRUE)
  expect_match(result_list$viz_url, sprintf("github.io/%s", test_repo_name), fixed = TRUE)
  expect_match(result_list$gh_pages_url, sprintf("%s/tree/gh-pages", test_repo_name), fixed = TRUE)
  README.md <- file.path(result_list$local_clone, "README.md")
  README.lines <- readLines(README.md)
  expected.line <- paste("##", viz$title)
  expect_identical(README.lines[1], expected.line)
  tsv_files_created <- get_tsv(result_list)
  expect_equal(length(tsv_files_created), 1)
  expect_no_Capture(result_list)
  ## clone and add Capture.PNG
  new_clone <- tempfile()
  github_url <- paste0("https://github.com/", result_list$owner_repo, ".git")
  gert::git_clone(github_url, new_clone)
  branch_name <- gert::git_branch(new_clone)
  expect_identical(branch_name, "gh-pages", info=paste(branch_name, "should be gh-pages"))
  cat("FOO", file=file.path(new_clone, "Capture.PNG"))
  gert::git_add("Capture.PNG", repo=new_clone)
  gert::git_commit(message="add Capture.PNG", repo=new_clone)
  gert::git_push(repo=new_clone)
  ## second run of animint2pages updates data viz.
  viz.more <- viz
  viz.more$five <- ggplot()+
    geom_point(aes(
      x, x),
      data=data.frame(x=1:5))
  Sys.sleep(3)
  update_list <- animint2pages(viz.more, test_repo_name, owner="animint-test", chromote_sleep_seconds=NULL)
  Sys.sleep(3)
  tsv_files_updated <- get_tsv(update_list)
  expect_equal(length(tsv_files_updated), 2)
  expect_Capture(update_list)
})

# This test is skipped under covr coverage collection due to environment manipulation.
test_that("animint2pages raises an error if no GitHub token is present", {
  if (identical(Sys.getenv("R_COVR"), "true")) skip("Skip on covr: environment manipulation not supported")
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
