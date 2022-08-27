acontext("colour_off, color_off")

g1 <- ggplot()+
geom_line(data=economics_long, 
  aes(x=date, y=value01, group = variable),
                clickSelects="variable")+
  ggtitle("default to alpha_off(0.5) style")

g2 <- ggplot() +
  geom_line(data=economics_long, 
  aes(x=date, y=value01, group = variable),
                colour = "red",
                colour_off = "black",
                clickSelects="variable")+
  ggtitle("With colour_off")

g3 <- ggplot() +
geom_line(data=economics_long, 
aes(x=date, y=value01, group = variable),
              colour = "red",
              colour_off = "black",
              alpha_off=0.5,
              clickSelects="variable")+
ggtitle("colour_off + alpha_off")

viz.line <- list(one = g1,
                two = g2,
                three = g3)

info <- animint2HTML(viz.line)

test_that("default clicking line only changes opacity", {
  line.xpath <- '//svg[@id="plot_one"]//path[@class="geom"]'
  node.list <- getNodeSet(info$html, line.xpath)
  opacity.str <- getStyleValue(info$html, line.xpath, "opacity")
  opacity.num <- as.numeric(opacity.str)
  clicked.list <- node.list[opacity.num == 1]
  nonclicked.list <- node.list[opacity.num == 0.5]
  
  # there shall be 1 line shows opacity=1, and the other 4 lines
  # opacity = 0.5/whatever user defines
  expect_equal(length(clicked.list), 1)
  expect_equal(length(nonclicked.list), 4)
  # color doesn't change
  stroke.vec <- getStyleValue(info$html, line.xpath, "stroke")
  color.vec <- rep("black", 5)
  expect_color(stroke.vec, color.vec)
})

test_that("using colour_off, clicking line only changes stroke", {
  line.xpath <- '//svg[@id="plot_two"]//path[@class="geom"]'
  node.list <- getNodeSet(info$html, line.xpath)
  stroke.vec <- getStyleValue(info$html, line.xpath, "stroke")
  colour.off.col <- "black"
  colour <- "red"
  
  # On firefox, stroke is "rgb(127, 127, 127)"
  # On phantomjs, stroke is "#7f7f7f"
  if(grepl("rgb", stroke.vec[1])){
    nonclick.colour <- paste(col2rgb(colour.off.col), collapse=", ")
    click.colour <- paste(col2rgb(colour), collapse=", ")
  } else{
    nonclick.colour <- as.character(toRGB(colour.off.col))
    click.colour <- as.character(toRGB(colour))
  }
  clicked.list <- node.list[grepl(click.colour, stroke.vec)]
  nonclicked.list <- node.list[grepl(nonclick.colour, stroke.vec)]
  expect_equal(length(clicked.list), 1)
  expect_equal(length(nonclicked.list), 4)
  # opacity remains the same
  opacity.str <- getStyleValue(info$html, line.xpath, "opacity")
  opacity.num <- as.numeric(opacity.str)
  opacity.list <- node.list[opacity.num == 1]
  expect_equal(length(opacity.list), 5)
})

test_that("using both alpha_off and colour_off, opacity and stroke change simultaneously", {
  line.xpath <- '//svg[@id="plot_three"]//path[@class="geom"]'
  node.list <- getNodeSet(info$html, line.xpath)
  stroke.vec <- getStyleValue(info$html, line.xpath, "stroke")
  colour.off.col <- "black"
  colour <- "red"
  if(grepl("rgb", stroke.vec[1])){
    nonclick.colour <- paste(col2rgb(colour.off.col), collapse=", ")
    click.colour <- paste(col2rgb(colour), collapse=", ")
  } else{
    nonclick.colour <- as.character(toRGB(colour.off.col))
    click.colour <- as.character(toRGB(colour))
  }
  clicked.list <- node.list[grepl(click.colour, stroke.vec)]
  nonclicked.list <- node.list[grepl(nonclick.colour, stroke.vec)]
  expect_equal(length(clicked.list), 1)
  expect_equal(length(nonclicked.list), 4)
  # opacity changes as well
  opacity.str <- getStyleValue(info$html, line.xpath, "opacity")
  opacity.num <- as.numeric(opacity.str)
  clicked.list <- node.list[opacity.num == 1]
  nonclicked.list <- node.list[opacity.num == 0.5]
  expect_equal(length(clicked.list), 1)
  expect_equal(length(nonclicked.list), 4)
})

viz.point <- list(pointone = ggplot() + geom_point(
  data = mtcars,
  size = 10,
  aes(x=wt, y=mpg,
      colour = disp),
  clickSelects = "gear")+
  ggtitle("default alpha_off(0.5) style"),

pointtwo = ggplot() + geom_point(
  data = mtcars,
  colour="red",
  colour_off="transparent",
  size = 10,
  aes(x=wt, y=mpg,
      fill = disp),
  clickSelects = "gear")+
  ggtitle("colour=\"red\", colour_off=\"transparent\" "),

pointthree = ggplot() + geom_point(
  data = mtcars,
  alpha_off=0.5,
  colour="red",
  colour_off="transparent",
  size = 10,
  aes(x=wt, y=mpg,
      fill = disp,
      id=paste0("pointthree_disp", disp, "gear", gear, "wt", wt)),
  clickSelects = "gear")+
  ggtitle("colour_off + alpha_off"))

info2 <- animint2HTML(viz.point)

test_that("color_off only changes colour/stroke when clicked, fill does not change", {
  point.xpath <- '//svg[@id="plot_pointthree"]//circle[@id="pointthree_disp275.8gear3wt3.73"]'
  circle.list <- getNodeSet(info2$html, point.xpath)
  before.click.color <- getStyleValue(info2$html, point.xpath, "stroke")
  before.click.fill <- getStyleValue(info2$html, point.xpath, "fill")
  
  clickID('pointthree_disp275.8gear3wt3.73')
  html <- getHTML()
  after.click.color <- getStyleValue(html, point.xpath, "stroke")
  after.click.fill <- getStyleValue(html, point.xpath, "fill")
  
  expect_false(isTRUE(all.equal(before.click.color, after.click.color)))
  expect_equal(before.click.fill, after.click.fill)
})

test_that("fill and color are not same", {
  point.xpath <- '//svg[@id="plot_pointthree"]//circle[@class="geom"]'
  circle.list <- getNodeSet(info2$html, point.xpath)
  circle.color <- getStyleValue(info2$html, point.xpath, "stroke")
  circle.fill <- getStyleValue(info2$html, point.xpath, "fill")
  expect_false(isTRUE(all.equal(circle.color, circle.fill)))
})

row.vec <- paste("row", c(1:3))
col.vec <- paste("col", c(1:3))
heat.data <- data.table(row.name = row.vec,
  col.name = rep(col.vec, each=length(row.vec)),
  value = c(2,8,-5,-7,15,3,-1,6,-7.5))
viz.tile <- list(
  vt = ggplot() +
  geom_tile(data=heat.data,
  aes(x = row.name, y = col.name, fill = value,
  id=paste0("value", value)),
  clickSelects = "value"))

info3 <- animint2HTML(viz.tile)

test_that("if colour is null and has clickSelects, after clicking, selection colour/stroke should be black", {
  clickID('value2')
  html <- getHTML()
  stroke.col <- getStyleValue(
  info3$html, '//g[@class="geom1_tile_vt"]//rect[@id="value2"]', "stroke")
  expect.stroke.col <- "black"
  expect_color(stroke.col, expect.stroke.col)
})