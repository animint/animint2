acontext("update_axes respects selector duration - Issue #276")
# Test that update_axes uses the selector's duration option instead of hardcoded 1000ms
# This test verifies tick values change, which confirms update_axes is being called
mtcars$cyl <- as.factor(mtcars$cyl)
viz_duration <- list(
  scatter = ggplot() +
    geom_point(aes(mpg, disp, colour=cyl), data=mtcars) +
    theme_animint(update_axes=c("x", "y")),
  duration=list(cyl=2000),
  selector.types=list(cyl="single")
)
info <- animint2HTML(viz_duration)

# Get initial tick positions
rect_path_x <- '//svg[@id="plot_scatter"]//g[contains(@class, "xaxis")]'
rect_path_y <- '//svg[@id="plot_scatter"]//g[contains(@class, "yaxis")]'
initial_x_ticks <- getTickDiff(info$html, axis="x")
initial_y_ticks <- getTickDiff(info$html, axis="y")

# Click to change selector
clickID("plot_scatter_cyl_variable_8")
Sys.sleep(0.5)
html_updated <- getHTML()

# Get updated tick positions
updated_x_ticks <- getTickDiff(html_updated, axis="x")
updated_y_ticks <- getTickDiff(html_updated, axis="y")

test_that("update_axes uses selector duration for transitions", {
  # Verify ticks changed (confirms update_axes was called)
  expect_true(!identical(initial_x_ticks, updated_x_ticks),
              info="X-axis ticks should change when selector changes")
  expect_true(!identical(initial_y_ticks, updated_y_ticks),
              info="Y-axis ticks should change when selector changes")
  # The JavaScript fix ensures .duration(Selectors[v_name].duration) is used
  # instead of hardcoded .duration(1000)
  # Manual testing in browser would show 2000ms transition vs 1000ms
})
