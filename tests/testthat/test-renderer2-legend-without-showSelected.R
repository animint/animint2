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
  expect_gt(length(legend_info$entries), 0)
  ## Selector should be created (current default behavior)
  expect_identical(legend_info$selector, "comparison")
  expect_identical("comparison" %in% names(info$selectors), TRUE)
})
test_that("showSelected=character() keeps legend selector but opts layer out", {
  ## Proposed API: empty character vector disables auto showSelected injection
  ## for this layer, but keeps legend interactivity intact.
  viz_no_ss <- list(
    plot1 = ggplot(test_data, aes(x, y, color = comparison)) +
      geom_point(showSelected = character()) +
      facet_wrap(~facet_var)
  )
  info <- animint2dir(viz_no_ss, open.browser = FALSE)
  legend_info <- info$plots$plot1$legend$comparison
  expect_gt(length(legend_info$entries), 0)
  ## Legend should still be clickable via selector metadata.
  expect_identical(legend_info$selector, "comparison")
  expect_identical("comparison" %in% names(info$selectors), TRUE)
  ## The opted-out layer should not have auto-generated showSelected aesthetics.
  geom_names <- names(info$geoms)
  point_geom_name <- geom_names[grepl("_point_plot1$", geom_names)]
  expect_identical(length(point_geom_name), 1L)
  show_aes_names <- names(info$geoms[[point_geom_name]]$aes)
  expect_identical(any(grepl("^showSelected", show_aes_names)), FALSE)
})
