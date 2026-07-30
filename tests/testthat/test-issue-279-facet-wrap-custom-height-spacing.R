test_that("facet_wrap SVG height is proportional to theme_animint height, no excess space (#279)", {
  task_data <- data.frame(
    x = rep(1:5, 5),
    y = rep(1:5, 5),
    task_id = rep(c("sonar", "spam", "vowel", "waveform", "zip"), each = 5)
  )
  n_facets <- length(unique(task_data$task_id))
  svg_xpath <- "//svg[@id='plot_plot']"
  svg_sel <- "svg#plot_plot"
  parent_sel <- "td:has(> svg#plot_plot)"
  base_plot <- ggplot() +
    geom_point(aes(x, y), data = task_data) +
    facet_wrap(~ task_id, ncol = 1) +
    theme_bw()
  viz_list <- list(
    default = list(plot = base_plot),
    custom = list(plot = base_plot + theme_animint(height = 600))
  )
  results <- lapply(viz_list, function(viz) {
    info <- animint2HTML(viz)
    svg_node <- getNodeSet(info$html, svg_xpath)
    expect_equal(length(svg_node), 1L)
    svg_attrs <- xmlAttrs(svg_node[[1]])
    display <- getStyleValue(info$html, svg_xpath, "display")
    expect_equal(display, "block",
      label = "plot SVG should have inline display:block to prevent whitespace below it")
    svg_box <- get_element_bbox(svg_sel)
    parent_box <- get_element_bbox(parent_sel)
    bottom_gap <- (parent_box$top + parent_box$height) - (svg_box$top + svg_box$height)
    expect_lte(bottom_gap, 1,
      label = "no excess rendered gap below plot SVG (issue #279)")
    list(height = as.numeric(svg_attrs[["height"]]))
  })
  h_list <- lapply(results, `[[`, "height")
  expect_lt(h_list$default, 400 * n_facets)
  expect_lt(h_list$custom, 600 * n_facets,
    label = "SVG height should not be 600*num_facets — regression from issue #279")
  expect_gt(h_list$custom, h_list$default)
})
