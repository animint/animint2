acontext("GitHub Pages")

Sys.setenv(GITHUB_PAT = Sys.getenv("GITHUB_PAT"))
gh_username <- Sys.getenv("GITHUB_USERNAME")

test_that("animint2pages() returns an object of class 'git_repository'", {
  repo <- animint2pages(list(p = ggplot2::qplot(1:10)), github_repo = repo_name)
  expect_is(repo, "git_repository")
})
