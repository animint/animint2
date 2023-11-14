acontext("fill_off")

#
# Test geoms with both fill and colour styles
#
viz.point <- list(
  defaultAlphaOff = ggplot() +
    geom_point(
      data = mtcars,
      size = 10,
      aes(
        x = wt, y = mpg,
        colour = disp
      ),
      clickSelects = "gear"
    ) +
    ggtitle("default alpha_off(0.5) style"),
  fillOffSpecified = ggplot() +
    geom_point(
      data = mtcars,
      fill_off = "transparent",
      size = 10,
      aes(
        x = wt, y = mpg,
        fill = disp
      ),
      clickSelects = "gear"
    ) +
    ggtitle("colour corresponding to `disp` group, fill_off=\"transparent\" "),
  fillAndAlphaOff = ggplot() +
    geom_point(
      data = mtcars,
      alpha_off = 0.5,
      fill_off = "grey",
      size = 10,
      aes(
        x = wt, y = mpg,
        fill = disp,
        id = paste0("fillAndAlphaOff_disp", disp, "gear", gear, "wt", wt)
      ),
      clickSelects = "gear"
    ) +
    ggtitle("fill_off + alpha_off")
)

viz_info <- animint2HTML(viz.point)

test_that("fill_off only changes fill when clicked, colour does not change", {
  point.xpath <- '//svg[@id="plot_fillAndAlphaOff"]//circle[@id="fillAndAlphaOff_disp275.8gear3wt3.73"]'
  circle.list <- getNodeSet(viz_info$html, point.xpath)
  before.click.color <- getStyleValue(viz_info$html, point.xpath, "stroke")
  before.click.fill <- getStyleValue(viz_info$html, point.xpath, "fill")

  clickID("fillAndAlphaOff_disp275.8gear3wt3.73")
  html <- getHTML()
  after.click.color <- getStyleValue(html, point.xpath, "stroke")
  after.click.fill <- getStyleValue(html, point.xpath, "fill")

  expect_false(isTRUE(all.equal(before.click.fill, after.click.fill)))
  expect_color(after.click.color, before.click.color)
})

test_that("fill and color are not same", {
  point.xpath <- '//svg[@id="plot_fillAndAlphaOff"]//circle[@class="geom"]'
  circle.list <- getNodeSet(viz_info$html, point.xpath)
  circle.color <- getStyleValue(viz_info$html, point.xpath, "stroke")
  circle.fill <- getStyleValue(viz_info$html, point.xpath, "fill")
  expect_false(isTRUE(all.equal(circle.color, circle.fill)))
})

rect.data <- data.frame(
  xmin = c(1, 3, 5),
  xmax = c(2, 4, 6),
  ymin = c(1, 2, 3),
  ymax = c(2, 3, 4),
  category = c("A", "B", "C")
)

viz.rect <- list(rectFillOff = ggplot() +
  geom_rect(
    data = rect.data, aes(
      xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
      id = paste0("rectFillOff_", category)
    ),
    color = "black", fill = "blue", fill_off = "transparent", clickSelects = "category"
  ))

viz_info <- animint2HTML(viz.rect)

test_that("with fill_off, fill changes when clicked", {
  rect_xpath <- '//svg[@id="plot_rectFillOff"]//rect[@id="rectFillOff_A"]'

  rect_list <- getNodeSet(viz_info$html, rect_xpath)

  before_click_color <- getStyleValue(viz_info$html, rect_xpath, "stroke")
  before_click_fill <- getStyleValue(viz_info$html, rect_xpath, "fill")

  clickID("rectFillOff_B")

  html <- getHTML()

  after_click_color <- getStyleValue(html, rect_xpath, "stroke")
  after_click_fill <- getStyleValue(html, rect_xpath, "fill")
  expect_false(isTRUE(all.equal(before_click_fill, after_click_fill)))
})

vline.data <- data.frame(
  xintercept = c(1, 2, 3),
  category = c("A", "B", "C")
)

viz.vline <- list(
  v = ggplot() +
    geom_vline(
      data = vline.data, aes(xintercept = xintercept, key = category,
      id = paste0("v_", category)),
      fill = "blue", fill_off = "grey", clickSelects = "category"
    ) +
    ggtitle("Click to Select a Vertical Line")
)

test_that("When using fill_off and clickSelects parameter with geom_vline, use default(alpha) selection style", {
  expect_warning(
    viz_info <- animint2HTML(viz.vline),
    "geom1_vline_v has fill_off which is not supported."
  )
  
  vline_xpath <- '//g[@class="geom1_vline_v"]//line[@id="v_A"]'
  
  before_click_color <- getStyleValue(viz_info$html, vline_xpath, "stroke")
  before_click_opacity <- getStyleValue(viz_info$html, vline_xpath, "opacity")
  
  clickID("v_B")
  
  html <- getHTML()
  after_click_color <- getStyleValue(html, vline_xpath, "stroke")
  after_click_opacity <- getStyleValue(html, vline_xpath, "opacity")
 
  expect_color(before_click_color, "black")
  expect_color(after_click_color, before_click_color)

  expect_equal(before_click_opacity, "1")
  expect_equal(after_click_opacity, "0.5")
})
