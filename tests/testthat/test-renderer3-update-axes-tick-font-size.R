acontext("update_axes tick font-size consistency - Issue #273")
mtcars_test <- mtcars
mtcars_test$cyl <- as.factor(mtcars_test$cyl)
viz_tick_size <- list(
  noUpdate = ggplot() +
    geom_point(aes(mpg, disp, colour=cyl), data=mtcars_test) +
    ggtitle("No update_axes") +
    theme(axis.text = element_text(size=12)),
  withUpdateX = ggplot() +
    geom_point(aes(mpg, disp, colour=cyl), showSelected="cyl", data=mtcars_test) +
    theme_animint(update_axes=c("x")) +
    ggtitle("With update_axes x") +
    theme(axis.text = element_text(size=12)),
  selector.types = list(cyl="single")
)
info <- animint2HTML(viz_tick_size)
test_that("x-axis tick text font-size is consistent with and without update_axes", {
  no_update_xpath <- '//svg[@id="plot_noUpdate"]//g[@class="xaxis axis xaxis_1"]//g[@class="tick major"]//text'
  with_update_xpath <- '//svg[@id="plot_withUpdateX"]//g[@class="xaxis axis xaxis_1"]//g[@class="tick major"]//text'
  no_update_font_size <- getStyleValue(info$html, no_update_xpath, "font-size")
  with_update_font_size <- getStyleValue(info$html, with_update_xpath, "font-size")
  expect_true(length(no_update_font_size) > 0)
  expect_true(length(with_update_font_size) > 0)
  no_update_size_unique <- unique(no_update_font_size)
  with_update_size_unique <- unique(with_update_font_size)
  expect_equal(length(no_update_size_unique), 1)
  expect_equal(length(with_update_size_unique), 1)
  expect_equal(with_update_size_unique, no_update_size_unique, info=sprintf("Axis tick text font-size should be consistent regardless of update_axes. Expected %s but got %s", no_update_size_unique, with_update_size_unique))
})
