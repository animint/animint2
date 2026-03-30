library(testthat)
library(XML)

acontext("update_axes with geom_bar")
# issue was raised in https://github.com/animint/animint2/issues/48

bar.data <- data.frame(
  x = c(2, 4, 6, 8,
        22, 24, 26, 28,
        42, 44, 46, 48),
  y = c(10, 14, 12, 16,
        11, 15, 13, 17,
         9, 13, 11, 15),
  set = c(rep("few", 4), rep("medium", 4), rep("many", 4))
)

no_axes <- ggplot() +
  geom_bar(aes(x, y, fill = set),
           showSelected = "set",
           clickSelects = "set",
           data = bar.data,
           stat = "identity")
x_axes <- no_axes +
  theme_animint(update_axes = "x")
y_axes <- no_axes +
  theme_animint(update_axes = "y")
both_axes <- no_axes +
  theme_animint(update_axes = c("x", "y"))

viz <- list(
  neither=no_axes, 
  x = x_axes,
  y = y_axes, 
  both = both_axes,
  selector.types = list(set = "single")
)

expect_warning(
  info <- animint2HTML(viz),
  "so created a plot with no updates for X axis"
)

test_that("geom_bar plots render x and y axes", {
  x_axis_nodes <- getNodeSet(info$html, "//svg[@id='plot_y']//g[contains(@class, 'xaxis')]")
  y_axis_nodes <- getNodeSet(info$html, "//svg[@id='plot_y']//g[contains(@class, 'yaxis')]")

  expect_gt(length(x_axis_nodes), 0)
  expect_gt(length(y_axis_nodes), 0)
})

rect_path <- "//svg[@id='plot_y']//g[contains(@class, '%saxis')]"

x_before <- getNodeSet(info$html, sprintf(rect_path, "x"))[[1]]
y_before <- getNodeSet(info$html, sprintf(rect_path, "y"))[[1]]

original_tick_diff_x <- getTickDiff(x_before, axis = "x")
original_tick_diff_y <- getTickDiff(y_before, axis = "y")

clickID("plot_y_set_variable_medium")
Sys.sleep(0.5)
html_updated <- getHTML()

x_after <- getNodeSet(html_updated, sprintf(rect_path, "x"))[[1]]
y_after <- getNodeSet(html_updated, sprintf(rect_path, "y"))[[1]]

updated_tick_diff_x <- getTickDiff(x_after, axis = "x")
updated_tick_diff_y <- getTickDiff(y_after, axis = "y")

# y-axis does not change after selection, even when update_axes = "y"
# this will be reversed after fixing issue #48
test_that("geom_bar y-axis does not update after selection change", {
  expect_equal(updated_tick_diff_y, original_tick_diff_y)
  # expect_true(unequal(updated_tick_diff_y, original_tick_diff_y))
})

test_that("geom_bar x-axis stays unchanged for update_axes='y'", {
  expect_equal(updated_tick_diff_x, original_tick_diff_x)
})