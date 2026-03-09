acontext("panel.margin positive values - Issue #180 - renderer")
library(XML)
panel_y <- function(node_list) {
  attrs <- sapply(node_list, xmlAttrs)
  as.numeric(attrs["y", ])
}
panel_h <- function(node_list) {
  attrs <- sapply(node_list, xmlAttrs)
  as.numeric(attrs["height", ])
}
viz.zero <- list(
  plot1 = ggplot() +
    theme_bw() +
    theme(panel.margin = grid::unit(0, "lines")) +
    geom_point(aes(Sepal.Width, Sepal.Length, colour = Species), data = iris) +
    facet_grid(Species ~ .)
)
viz.positive <- list(
  plot1 = ggplot() +
    theme_bw() +
    theme(panel.margin = grid::unit(2, "lines")) +
    geom_point(aes(Sepal.Width, Sepal.Length, colour = Species), data = iris) +
    facet_grid(Species ~ .)
)
info.zero <- animint2HTML(viz.zero)
info.positive <- animint2HTML(viz.positive)
bg.zero <- getNodeSet(
  info.zero$html,
  '//svg[@id="plot_plot1"]//rect[@class="background_rect"]')
bg.positive <- getNodeSet(
  info.positive$html,
  '//svg[@id="plot_plot1"]//rect[@class="background_rect"]')
test_that("three panels rendered with zero panel.margin", {
  expect_equal(length(bg.zero), 3)
})
test_that("three panels rendered with positive panel.margin", {
  expect_equal(length(bg.positive), 3)
})
test_that("panel y-positions are strictly increasing (vertically stacked)", {
  y.vals <- sort(panel_y(bg.positive))
  expect_true(all(diff(y.vals) > 0))
})
test_that("positive panel.margin produces larger inter-panel gap than zero margin", {
  gap_between_panels <- function(node_list) {
    y <- sort(panel_y(node_list))
    h <- panel_h(node_list)[order(panel_y(node_list))]
    y[2] - (y[1] + h[1])
  }
  gap.zero <- gap_between_panels(bg.zero)
  gap.positive <- gap_between_panels(bg.positive)
  expect_gt(gap.positive, 0)
  expect_gt(gap.positive, gap.zero)
})
