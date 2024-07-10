acontext("GitHub Pages")

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

test_that("animint2pages() returns owner/repo string", {
  viz_owner_repo <- animint2pages(viz, github_repo = "animint2pages_test_repo")
  expect_is(viz_owner_repo, "character")
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
  # to be reverted, test only
  config.content <- readLines(config.file)
  print(config.content)
  #
  config.old <- file.path(repo.root, ".git", "config.old")
  file.copy(config.file, config.old, overwrite = TRUE)
  cat("[credential]\n\tusername = FOO", file=config.file, append=TRUE)
  expect_error({
    animint2pages(viz, github_repo = "test_repo")
  }, "A GitHub token is required to create and push to a new repository. \nTo create a GitHub token, follow these steps:\n1. Go to https://github.com/settings/tokens/new?scopes=repo&description=animint2pages\n2. Confirm your password if prompted.\n3. Ensure that the 'repo' scope is checked.\n4. Click 'Generate token' at the bottom of the page.\n5. Copy the generated token.\nAfter creating the token, you can set it up in your R environment by running: \nSys.setenv(GITHUB_PAT=\"yourGithubPAT\") \ngert::git_config_global_set(\"user.name\", \"yourUserName\") \ngert::git_config_global_set(\"user.email\", \"yourEmail\") \n", fixed=TRUE)
  do.call(Sys.setenv, as.list(env.old))
  file.copy(config.old, config.file, overwrite = TRUE)
})

test_that("animint2pages() default branch is gh-pages", {
  whoami <- suppressMessages(gh::gh_whoami())
  owner <- whoami[["login"]]
  local_repo_path <- tempfile(pattern = "repo_clone_")
  # to be reverted, test only
  print(whoami)
  print(Sys.getenv("GITHUB_REPOSITORY"))
  #
  gert::git_clone(url = paste0("https://github.com/", owner, "/animint2pages_test_repo.git"), path = local_repo_path)
  # Check the default branch after clone
  default_branch <- gert::git_branch(repo = local_repo_path)
  expect_equal(default_branch, "gh-pages")
})