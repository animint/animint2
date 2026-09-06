acontext("geom_text interactive updates - issue #345")
library(animint2)
text.df <- data.frame(
  item = c("a", "b"),
  x = c(1, 9),
  y = 1,
  label = "marker",
  key = "label")
rect.df <- data.frame(
  item = c("a", "b"),
  xmin = c(0.6, 8.6),
  xmax = c(1.4, 9.4),
  id = c("select_a", "select_b"))
viz <- animint(
  plot1 = ggplot() +
    theme_animint(width = 400) +
    scale_x_continuous(limits = c(0, 10)) +
    geom_text(aes(x, y, label = label, key = key),
              showSelected = "item", data = text.df) +
    geom_tallrect(aes(xmin = xmin, xmax = xmax, id = id),
                  clickSelects = "item", data = rect.df),
  duration = list(item = 500),
  first = list(item = "a"))
info <- animint2HTML(viz)
get_geom_text_center_x <- function() {
  get_element_bbox("svg#plot_plot1 g.geom1_text_plot1 text.geom")$center_x
}
x_start <- get_geom_text_center_x()
clickID("select_b")
Sys.sleep(1)
test_that("geom_text x position updates after showSelected changes", {
  expect_gt(get_geom_text_center_x() - x_start, 50)
})
