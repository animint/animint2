acontext("GitHub Pages")

Sys.setenv(GITHUB_PAT = Sys.getenv("GITHUB_PAT"))

plot <- ggplot(data.frame(x = 1:10), aes(x)) +
  geom_point()

test_that("animint2pages() returns an object of class 'git_repository'", {
  repo <- animint2pages(list(p = plot), github_repo = "test_repo")
  expect_is(repo, "git_repository")
})

test_that("animint2pages raises an error if no GitHub token is present", {
  withr::local_envvar(c(GITHUB_PAT = NULL), {
    expect_error(
      animint2pages(list(p = plot), github_repo = "test_repo"),
      paste0(
        "A GitHub token is required to create and push to a new repository. \n",
        "To create a GitHub token, follow these steps:\n",
        "1. Go to https://github.com/settings/tokens/new?scopes=repo&description=animint2pages\n",
        "2. Confirm your password if prompted.\n",
        "3. Ensure that the 'repo' scope is checked.\n",
        "4. Click 'Generate token' at the bottom of the page.\n",
        "5. Copy the generated token.\n",
        "After creating the token, you can set it up in your R environment by running: \n",
        "gitcreds::gitcreds_set()\n",
        "And then paste the token when prompted."
      )
    )
  })
})
