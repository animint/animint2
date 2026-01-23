acontext("legend without showSelected")
## Test for issue #140: legend forces showSelected, no way to opt out
## https://github.com/animint/animint2/issues/140
##
## The problem: when using aes(color=variable), animint2 automatically:
## 1. Creates a legend (good)
## 2. Adds showSelected behavior so clicking legend filters data (unwanted)
##
## Users want option 4: legend visible but NO showSelected filtering.
## Create test data similar to the issue example
test_data <- data.frame(
  x = c(-10, -8, -6, -4, -2, -10, -8, -6, -4, -2),
  y = c(0.1, 0.2, 0.15, 0.25, 0.3, 0.05, 0.1, 0.08, 0.12, 0.15),
  comparison = rep(c("control", "treatment"), each = 5),
  facet_var = rep(c("all", "other"), 5)
)
test_that("default: legend with color aesthetic creates selector for interactivity", {
  viz <- list(
    plot1 = ggplot(test_data, aes(x, y, color = comparison)) +
      geom_point() +
      facet_wrap(~facet_var)
  )
  info <- animint2dir(viz, open.browser = FALSE)
  legend_info <- info$plots$plot1$legend$comparison
  ## Legend should exist with entries
  expect_false(is.null(legend_info))
  expect_true(length(legend_info$entries) > 0)
  ## Selector should be created (current default behavior)
  expect_identical(legend_info$selector, "comparison")
  expect_true("comparison" %in% names(info$selectors))
})
test_that("showSelected=character() should allow legend without selector", {
  ## Proposed API: empty character vector disables auto showSelected from legend
  ## This is consistent with existing API where showSelected accepts character vectors
  viz_no_ss <- list(
    plot1 = ggplot(test_data, aes(x, y, color = comparison)) +
      geom_point(showSelected = character()) +
      facet_wrap(~facet_var)
  )
  info <- animint2dir(viz_no_ss, open.browser = FALSE)
  legend_info <- info$plots$plot1$legend$comparison
  ## Legend should STILL exist and render with entries
  expect_false(is.null(legend_info))
  expect_true(length(legend_info$entries) > 0)
  ## But selector should NOT be created - legend is display-only
  expect_null(legend_info$selector)
  expect_false("comparison" %in% names(info$selectors))
})
