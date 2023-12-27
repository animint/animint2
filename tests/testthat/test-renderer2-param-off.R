acontext("colour_off, fill_off, alpha_off")
library(animint2)
## test geom without fill style
viz.line <- list(
  default = ggplot()+
    geom_line(aes(
      x=date, y=value01, group = variable),
      data=economics_long, 
      clickSelects="variable")+
    ggtitle("default to alpha_off(0.5) style"),
  coff=ggplot() +
    geom_line(aes(
      x=date, y=value01, group = variable),
      colour = "red",
      colour_off = "black",
      data=economics_long,
      clickSelects="variable")+
    ggtitle("With colour_off"),
  acoff=ggplot() +
    geom_line(aes(
      x=date, y=value01, group = variable),
      colour = "red",
      colour_off = "black",
      alpha_off=1,
      data=economics_long, 
      clickSelects="variable")+
    ggtitle("colour_off + alpha_off"))

info <- animint2HTML(viz.line)
test_that("default clicking line only changes opacity", {
  opacity.str <- getStyleValue(
    info$html,
    '//svg[@id="plot_default"]//path[@class="geom"]',
    "opacity")
  opacity.tab <- sort(table(opacity.str))
  expect_equal(as.numeric(opacity.tab), c(1, 4))
  expect_equal(names(opacity.tab), c("1","0.5"))
  stroke.str <- getStyleValue(
    info$html,
    '//svg[@id="plot_default"]//path[@class="geom"]',
    "stroke")
  expect_color(stroke.str, rep("black", 5))
})

test_that("setting colour_off makes stroke and opacity change", {
  opacity.str <- getStyleValue(
    info$html,
    '//svg[@id="plot_coff"]//path[@class="geom"]',
    "opacity")
  opacity.tab <- sort(table(opacity.str))
  expect_equal(as.numeric(opacity.tab), c(1, 4))
  expect_equal(names(opacity.tab), c("1","0.5"))
  stroke.str <- getStyleValue(
    info$html,
    '//svg[@id="plot_coff"]//path[@class="geom"]',
    "stroke")
  stroke.tab <- sort(table(stroke.str))
  expect_equal(as.numeric(stroke.tab), c(1, 4))
  expect_color(names(stroke.tab), c("red","black"))
})

test_that("setting alpha_off and colour_off makes only stroke change", {
  opacity.str <- getStyleValue(
    info$html,
    '//svg[@id="plot_acoff"]//path[@class="geom"]',
    "opacity")
  opacity.tab <- sort(table(opacity.str))
  expect_equal(as.numeric(opacity.tab), 5)
  expect_equal(names(opacity.tab), "1")
  stroke.str <- getStyleValue(
    info$html,
    '//svg[@id="plot_acoff"]//path[@class="geom"]',
    "stroke")
  stroke.tab <- sort(table(stroke.str))
  expect_equal(as.numeric(stroke.tab), c(1, 4))
  expect_color(names(stroke.tab), c("red","black"))
})

## test geom with both fill and colour styles
viz.point <- list(
  default = ggplot() +
    geom_point(aes(
      x=wt, y=mpg,
      colour = disp),
      data = mtcars,
      size = 10,
      clickSelects = "gear")+
    ggtitle("default alpha_off(0.5) style"),
  acoff = ggplot() +
    geom_point(aes(
      x=wt, y=mpg,
      fill = disp),
      data = mtcars,
      colour="red",
      colour_off="yellow",
      alpha_off=1,
      size = 10,
      clickSelects = "gear")+
    ggtitle("colour=\"red\", colour_off=\"transparent\" "),
  orange=ggplot()+
    geom_point(aes(
      wt, mpg),
      data=mtcars,
      colour="orange",
      size=10,
      clickSelects="gear"))

info.point <- animint2HTML(viz.point)

test_that("default for point makes only alpha change", {
  opacity.str <- getStyleValue(
    info.point$html,
    '//svg[@id="plot_default"]//circle[@class="geom"]',
    "opacity")
  opacity.tab <- table(opacity.str)
  expect_equal(sort(names(opacity.tab)), c("0.5","1"))
})

test_that("setting alpha_off and colour_off makes only stroke change", {
  opacity.str <- getStyleValue(
    info.point$html,
    '//svg[@id="plot_acoff"]//circle[@class="geom"]',
    "opacity")
  opacity.tab <- table(opacity.str)
  expect_equal(names(opacity.tab), "1")
  stroke.str <- getStyleValue(
    info.point$html,
    '//svg[@id="plot_acoff"]//circle[@class="geom"]',
    "stroke")
  stroke.tab <- sort(table(stroke.str))
  expect_color(names(stroke.tab), c("red","yellow"))
})

test_that("setting colour makes only alpha change, not fill, not stroke", {
  opacity.str <- getStyleValue(
    info.point$html,
    '//svg[@id="plot_orange"]//circle[@class="geom"]',
    "opacity")
  opacity.tab <- table(opacity.str)
  expect_equal(sort(names(opacity.tab)), c("0.5","1"))
  stroke.str <- getStyleValue(
    info.point$html,
    '//svg[@id="plot_orange"]//circle[@class="geom"]',
    "stroke")
  stroke.tab <- table(stroke.str)
  expect_color(names(stroke.tab), "orange")
  fill.str <- getStyleValue(
    info.point$html,
    '//svg[@id="plot_orange"]//circle[@class="geom"]',
    "fill")
  fill.tab <- table(fill.str)
  expect_color(names(fill.tab), "orange")
})

#
# tests for g$geom="rect", originally only support 'stroke' as selection style
#
library(data.table)
row.vec <- paste("row", c(1:3))
col.vec <- paste("col", c(1:3))
heat.data <- data.table(row.name = row.vec,
  col.name = rep(col.vec, each=length(row.vec)),
  value = c(2,8,-5,-7,15,3,-1,6,-7.5))
viz.tile <- list(
  default=ggplot() +
    geom_tile(aes(
      x = row.name, y = col.name, fill = value),
      size = 5,
      data=heat.data,
      clickSelects = "value"),
  colandoff=ggplot() +
    geom_tile(aes(
      x = row.name, y = col.name, fill = value),
      data=heat.data,
      colour="red",
      colour_off="grey50",
      size = 5,
      clickSelects = "value"),
  filloff=ggplot() +
    geom_tile(aes(
      x = row.name, y = col.name, color = value),
      data=heat.data,
      fill="blue",
      fill_off="yellow",
      size = 2,
      clickSelects = "value"))

info.tile <- animint2HTML(viz.tile)

test_that("rect default is black/transparent stroke", {
  opacity.str <- getStyleValue(
    info.tile$html,
    '//svg[@id="plot_default"]//rect[@class="geom"]',
    "opacity")
  opacity.tab <- table(opacity.str)
  expect_equal(names(opacity.tab), "1")
  stroke.str <- getStyleValue(
    info.tile$html,
    '//svg[@id="plot_default"]//rect[@class="geom"]',
    "stroke")
  stroke.tab <- sort(table(stroke.str))
  expect_color(names(stroke.tab), c("black","transparent"))
  expect_equal(as.numeric(stroke.tab), c(1, 8))
})

test_that("rect custom color/off used as stroke", {
  opacity.str <- getStyleValue(
    info.tile$html,
    '//svg[@id="plot_colandoff"]//rect[@class="geom"]',
    "opacity")
  opacity.tab <- table(opacity.str)
  expect_equal(names(opacity.tab), "1")
  stroke.str <- getStyleValue(
    info.tile$html,
    '//svg[@id="plot_colandoff"]//rect[@class="geom"]',
    "stroke")
  stroke.tab <- sort(table(stroke.str))
  expect_color(names(stroke.tab), c("red","grey50"))
  expect_equal(as.numeric(stroke.tab), c(1, 8))
})

test_that("rect custom fill/off used as fill", {
  opacity.str <- getStyleValue(
    info.tile$html,
    '//svg[@id="plot_filloff"]//rect[@class="geom"]',
    "opacity")
  opacity.tab <- table(opacity.str)
  expect_equal(names(opacity.tab), "1")
  stroke.str <- getStyleValue(
    info.tile$html,
    '//svg[@id="plot_filloff"]//rect[@class="geom"]',
    "stroke")
  stroke.tab <- table(stroke.str)
  expect_equal(length(stroke.tab), 9)
  fill.str <- getStyleValue(
    info.tile$html,
    '//svg[@id="plot_filloff"]//rect[@class="geom"]',
    "fill")
  fill.tab <- sort(table(fill.str))
  expect_color(names(fill.tab), c("blue","yellow"))
  expect_equal(as.numeric(fill.tab), c(1, 8))
})
