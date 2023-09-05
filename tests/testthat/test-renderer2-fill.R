acontext("fill_off")

#
# test geom with both fill and colour styles
#
viz.point <- list(pointone = ggplot() + geom_point(
  data = mtcars,
  size = 10,
  aes(x=wt, y=mpg,
      colour = disp),
  clickSelects = "gear")+
  ggtitle("default alpha_off(0.5) style"),

pointtwo = ggplot() + geom_point(
  data = mtcars,
  fill_off="transparent",
  size = 10,
  aes(x=wt, y=mpg,
      fill = disp),
  clickSelects = "gear")+
  ggtitle("colour corresponding to `disp` group, fill_off=\"transparent\" "),

pointthree = ggplot() + geom_point(
  data = mtcars,
  alpha_off=0.5,
  fill_off="grey",
  size = 10,
  aes(x=wt, y=mpg,
      fill = disp,
      id=paste0("pointthree_disp", disp, "gear", gear, "wt", wt)),
  clickSelects = "gear")+
  ggtitle("fill_off + alpha_off"))

info2 <- animint2HTML(viz.point)

test_that("fill_off only changes fill when clicked, colour does not change", {
  point.xpath <- '//svg[@id="plot_pointthree"]//circle[@id="pointthree_disp275.8gear3wt3.73"]'
  circle.list <- getNodeSet(info2$html, point.xpath)
  before.click.color <- getStyleValue(info2$html, point.xpath, "stroke")
  before.click.fill <- getStyleValue(info2$html, point.xpath, "fill")
  
  clickID('pointthree_disp275.8gear3wt3.73')
  html <- getHTML()
  after.click.color <- getStyleValue(html, point.xpath, "stroke")
  after.click.fill <- getStyleValue(html, point.xpath, "fill")
  
  expect_false(isTRUE(all.equal(before.click.fill, after.click.fill)))
  expect_color(before.click.color, after.click.color)
})

test_that("fill and color are not same", {
  point.xpath <- '//svg[@id="plot_pointthree"]//circle[@class="geom"]'
  circle.list <- getNodeSet(info2$html, point.xpath)
  circle.color <- getStyleValue(info2$html, point.xpath, "stroke")
  circle.fill <- getStyleValue(info2$html, point.xpath, "fill")
  expect_false(isTRUE(all.equal(circle.color, circle.fill)))
})
