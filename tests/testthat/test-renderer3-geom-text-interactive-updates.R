acontext("geom_text interactive updates - issue #345")
library(animint2)
text.df <- data.frame(
  item = c("a", "b"),
  x = c(1, 9),
  y = 1,
  label = "marker",
  key = "label")
reference.df <- data.frame(
  x = 9,
  y = 2,
  label = "marker")
rect.df <- data.frame(
  item = c("a", "b"),
  xmin = c(0.6, 8.6),
  xmax = c(1.4, 9.4),
  id = c("select_a", "select_b"))
viz <- animint(
  plot1 = ggplot() +
    geom_text(aes(x, y, label = label, key = key),
              showSelected = "item", data = text.df) +
    geom_text(aes(x, y, label = label), data = reference.df) +
    geom_tallrect(aes(xmin = xmin, xmax = xmax, id = id),
                  clickSelects = "item", data = rect.df),
  duration = list(item = 500),
  first = list(item = "a"))
info <- animint2HTML(viz)
get_interactive_text_x <- function() {
  get_element_bbox("svg#plot_plot1 g.geom1_text_plot1 text.geom")$center_x
}
get_reference_text_x <- function() {
  get_element_bbox("svg#plot_plot1 g.geom2_text_plot1 text.geom")$center_x
}
reference_x <- get_reference_text_x()
interactive_x <- get_interactive_text_x()
test_that("interactive geom_text starts left of reference", {
  expect_lt(interactive_x, reference_x)
})
clickID("select_b")
Sys.sleep(1)
test_that("geom_text x position matches non-interactive geom after update", {
  expect_equal(get_interactive_text_x(), reference_x)
})
