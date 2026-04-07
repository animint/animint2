acontext("Issue #279: facet_grid spacing with custom height")

test_that("facet_grid SVG height is proportional to theme_animint height, no excess space (#279)", {
  skip_on_cran()

  task_data <- data.frame(
    x = rep(1:5, 5),
    y = rep(1:5, 5),
    task_id = rep(c("sonar", "spam", "vowel", "waveform", "zip"), each = 5)
  )

  viz_default <- list(
    plot = ggplot() +
      geom_point(aes(x, y), data = task_data) +
      facet_grid(task_id ~ .) +
      theme_bw()
  )

  viz_custom <- list(
    plot = ggplot() +
      geom_point(aes(x, y), data = task_data) +
      facet_grid(task_id ~ .) +
      theme_bw() +
      theme_animint(height = 600)
  )

  info_default <- animint2HTML(viz_default)
  info_custom  <- animint2HTML(viz_custom)

  svg_default <- XML::getNodeSet(info_default$html, "//svg[contains(@id,'plot_plot')]")
  expect_equal(length(svg_default), 1L)
  h_default <- as.numeric(XML::xmlAttrs(svg_default[[1]])[["height"]])
  expect_lt(h_default, 400 * 2)

  svg_custom <- XML::getNodeSet(info_custom$html, "//svg[contains(@id,'plot_plot')]")
  expect_equal(length(svg_custom), 1L)
  h_custom <- as.numeric(XML::xmlAttrs(svg_custom[[1]])[["height"]])

  expect_lt(h_custom, 600 * 2,
            label = "SVG height should not be 600*num_facets — regression from issue #279")
  expect_gt(h_custom, h_default)
})
