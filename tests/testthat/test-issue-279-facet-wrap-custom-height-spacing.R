test_that("facet_wrap SVG height is proportional to theme_animint height, no excess space (#279)", {
  skip_on_cran()
  task_data <- data.frame(
    x = rep(1:5, 5),
    y = rep(1:5, 5),
    task_id = rep(c("sonar", "spam", "vowel", "waveform", "zip"), each = 5)
  )
  n_facets <- length(unique(task_data$task_id))
  base_plot <- ggplot() +
    geom_point(aes(x, y), data = task_data) +
    facet_wrap(~ task_id, ncol = 1) +
    theme_bw()
  viz_list <- list(
    default = list(plot = base_plot),
    custom  = list(plot = base_plot + theme_animint(height = 600))
  )
  info_list <- lapply(viz_list, animint2HTML)
  h_list <- lapply(info_list, function(info) {
    svg_node <- XML::getNodeSet(info$html, "//svg[contains(@id,'plot_plot')]")
    expect_equal(length(svg_node), 1L)
    as.numeric(XML::xmlAttrs(svg_node[[1]])[["height"]])
  })
  # SVG height must not scale as height * n_facets (regression from issue #279)
  expect_lt(h_list$default, 400 * n_facets)
  expect_lt(h_list$custom, 600 * n_facets,
            label = "SVG height should not be 600*num_facets — regression from issue #279")
  expect_gt(h_list$custom, h_list$default)
})