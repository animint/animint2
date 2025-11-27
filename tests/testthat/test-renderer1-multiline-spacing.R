acontext("multiline text spacing")
data <- data.frame(x = 1:10, y = 1:10)
viz <- list(
  plot1 = ggplot(data, aes(x, y)) +
    geom_point() +
    ggtitle("Multiline Title\nLine Two\nLine Three") +
    ylab("Y Axis\nLabel Two") +
    theme(text = element_text(size = 20))
)
info <- animint2HTML(viz)
test_that("multiline plot title with large font does not overlap plot area", {
  title_bbox <- get_element_bbox("text.plottitle")
  plot_rect <- get_element_bbox("#plot_plot1 rect.background_rect")
  expect_lt(title_bbox$top + title_bbox$height, plot_rect$top)
})
test_that("multiline y-axis title with large font does not overlap plot area", {
  ytitle_bbox <- get_element_bbox("text.ytitle")
  plot_rect <- get_element_bbox("#plot_plot1 rect.background_rect")
  expect_lt(ytitle_bbox$left + ytitle_bbox$width, plot_rect$left)
})

