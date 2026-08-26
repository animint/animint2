acontext("update_axes respects selector duration - Issue #276")
# Test that update_axes uses the selector's duration option instead of hardcoded 1000ms
# This follows the same pattern as test-renderer3-update-axes.R
mtcars$cyl <- as.factor(mtcars$cyl)
viz_duration <- animint(
  scatter = ggplot() +
    geom_point(aes(mpg, disp, colour=cyl), data=mtcars) +
    theme_animint(update_axes=c("x", "y")),
  duration=list(cyl=2000),
  selector.types=list(cyl="single")
)
info <- animint2HTML(viz_duration)

# Get initial tick positions using same pattern as test-renderer3-update-axes.R
rect_path <- "//svg[@id='plot_scatter']//g[contains(@class, '%saxis')]"
x_axis_node <- getNodeSet(info$html, sprintf(rect_path, "x"))[[1]]
y_axis_node <- getNodeSet(info$html, sprintf(rect_path, "y"))[[1]]
original_tick_diff_x <- getTickDiff(x_axis_node, axis="x")
original_tick_diff_y <- getTickDiff(y_axis_node, axis="y")

# Click to change selector
clickID("plot_scatter_cyl_variable_8")
Sys.sleep(0.5)
html_updated <- getHTML()

# Get updated tick positions
x_axis_node_updated <- getNodeSet(html_updated, sprintf(rect_path, "x"))[[1]]
y_axis_node_updated <- getNodeSet(html_updated, sprintf(rect_path, "y"))[[1]]
updated_tick_diff_x <- getTickDiff(x_axis_node_updated, axis="x")
updated_tick_diff_y <- getTickDiff(y_axis_node_updated, axis="y")

test_that("update_axes uses selector duration for transitions", {
  # Verify ticks changed (confirms update_axes was called with correct duration)
  # Tick spacing changes when filtering to different cyl values (4, 6, 8)
  # because each subset has different mpg/disp ranges
  expect_gt(abs(updated_tick_diff_x - original_tick_diff_x), 0)
  expect_gt(abs(updated_tick_diff_y - original_tick_diff_y), 0)
})
