test_that("facet_wrap SVG height is proportional to theme_animint height, no excess space (#279)", {
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
  results <- lapply(viz_list, function(viz) {
    info <- animint2HTML(viz)
    svg_node <- XML::getNodeSet(info$html, "//svg[contains(@id,'plot_plot')]")
    expect_equal(length(svg_node), 1L)
    svg_attrs <- XML::xmlAttrs(svg_node[[1]])
    svg_id <- svg_attrs[["id"]]
    list(
      height = as.numeric(svg_attrs[["height"]]),
      display = runtime_evaluate(sprintf(
        "window.getComputedStyle(document.getElementById('%s')).display", svg_id))
    )
  })
  expect_equal(results$default$display, "block",
    label = "plot SVG should use display:block to prevent whitespace below it")
  expect_equal(results$custom$display, "block",
    label = "plot SVG should use display:block to prevent whitespace below it")
  h_list <- lapply(results, `[[`, "height")
  expect_lt(h_list$default, 400 * n_facets)
  expect_lt(h_list$custom, 600 * n_facets,
    label = "SVG height should not be 600*num_facets — regression from issue #279")
  expect_gt(h_list$custom, h_list$default)
  bottom_gap <- runtime_evaluate("(() => { const svg = document.querySelector(\"svg[id*='plot_plot']\"); const parent = svg.parentElement; return parent.getBoundingClientRect().bottom - svg.getBoundingClientRect().bottom; })()")
  expect_lte(bottom_gap, 1,
    label = "no excess rendered gap below plot SVG (issue #279)")
})
