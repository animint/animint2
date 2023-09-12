acontext("fill_off")

#
# Test geoms with both fill and colour styles
#
common_aes <- aes(x=wt, y=mpg, fill = disp)

viz_point <- list(
  default_alpha_off = ggplot() + geom_point(data = mtcars, size = 10, aes = common_aes, clickSelects = "gear") +
                      ggtitle("default alpha_off(0.5) style"),
  
  fill_off_specified = ggplot() + geom_point(data = mtcars, fill_off = "transparent", size = 10, aes = common_aes, clickSelects = "gear") +
                        ggtitle("Fill set to transparent when not selected"),
  
  fill_and_alpha_off = ggplot() + geom_point(data = mtcars, alpha_off = 0.5, fill_off = "grey", size = 10, aes = common_aes, clickSelects = "gear") +
                        ggtitle("Both fill and alpha change when selected")
)

viz_info <- animint2HTML(viz.point)

test_that("fill_off only changes fill when clicked, colour does not change", {
  point_xpath <- '//svg[@id="plot_fill_and_alpha_off"]//circle[@id="fill_and_alpha_off_disp275.8gear3wt3.73"]'
  circle_list <- getNodeSet(viz_info$html, point_xpath)
  before_click_color <- getStyleValue(viz_info$html, point_xpath, "stroke")
  before_click_fill <- getStyleValue(viz_info$html, point_xpath, "fill")
  
  clickID('fill_and_alpha_off_disp275.8gear3wt3.73')
  html <- getHTML()
  after_click_color <- getStyleValue(html, point_xpath, "stroke")
  after_click_fill <- getStyleValue(html, point_xpath, "fill")
  
  expect_false(isTRUE(all.equal(before_click_fill, after_click_fill)))
  expect_color(after_click_color, before_click_color)
})

test_that("fill and color are not the same", {
  point_xpath <- '//svg[@id="plot_fill_and_alpha_off"]//circle[@class="geom"]'
  circle_list <- getNodeSet(viz_info$html, point_xpath)
  circle_color <- getStyleValue(viz_info$html, point_xpath, "stroke")
  circle_fill <- getStyleValue(viz_info$html, point_xpath, "fill")
  expect_false(isTRUE(all.equal(circle_color, circle_fill)))
})

rect.data <- data.frame(
  xmin = c(1, 3, 5),
  xmax = c(2, 4, 6),
  ymin = c(1, 2, 3),
  ymax = c(2, 3, 4),
  category = c("A", "B", "C")
)

viz.rect <- ggplot() +
  geom_rect(rect.data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            color="black", fill="blue", fill_off = "transparent", clickSelects="category") +
  coord_cartesian(xlim = c(0, 7), ylim = c(0, 5))

viz_info <- animint2HTML(viz.rect)

test_that("In geom_rect, fill_off only changes fill when clicked, colour does not change", {
  rect_xpath <- '//svg[@id="plot_with_fill_off"]//rect[@id="fill_off_categoryA"]'
  
  rect_list <- getNodeSet(viz_info$html, rect_xpath)

  before_click_color <- getStyleValue(viz_info$html, rect_xpath, "stroke")
  before_click_fill <- getStyleValue(viz_info$html, rect_xpath, "fill")
  
  clickID('fill_off_categoryA')
  
  html <- getHTML()
  
  after_click_color <- getStyleValue(html, rect_xpath, "stroke")
  after_click_fill <- getStyleValue(html, rect_xpath, "fill")
  
  expect_false(isTRUE(all.equal(before_click_fill, after_click_fill)))
  
  expect_equal(after_click_color, before_click_color)
})