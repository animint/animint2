library(animint2)
acontext("Inf")

limits <- data.frame(
  i=1:3,
  lower=c(-Inf, 0, -1),
  upper=c(1, 2, Inf))
viz <- list(
  gg=ggplot()+
    theme_bw()+
    theme(
      panel.grid.major=element_line(color="red"),
      panel.grid.minor=element_line(color="blue"),
      panel.margin=grid::unit(0, "lines"))+
    facet_grid(side ~ top, scales="free")+
    geom_segment(aes(
      i, lower, yend=upper, xend=i),
      data=data.frame(limits, side="yInf", top="left"))+
    geom_segment(aes(
      lower+10, i+10, xend=upper+10, yend=i+10),
      data=data.frame(limits, side="xInf", top="right")))
info <- animint2HTML(viz)

## First panel, test y values.
bg.rect <- getNodeSet(
  info$html,
  "//g[contains(@class, 'bgr1')]//rect[@class='background_rect']")[[1]]
attr.vec <- xmlAttrs(bg.rect)
panel.top <- as.numeric(attr.vec[["y"]])
h <- as.numeric(attr.vec[["height"]])
panel.bottom <- panel.top + h
line.list <- getNodeSet(
  info$html,
  "//g[contains(@class, 'PANEL1')]//line")
attr.mat <- sapply(line.list, xmlAttrs)
seg.bottom <- as.numeric(attr.mat["y1",])
seg.top <- as.numeric(attr.mat["y2",])
n.top <- sum(seg.top == panel.top)
n.bottom <- sum(seg.bottom == panel.bottom)
test_that("one y at top of panel", {
  expect_equal(n.top, 1)
})
test_that("one y at bottom of panel", {
  expect_equal(n.bottom, 1)
})

## Last panel, test x values.
bg.rect <- getNodeSet(
  info$html,
  "//g[contains(@class, 'bgr4')]//rect[@class='background_rect']")[[1]]
attr.vec <- xmlAttrs(bg.rect)
panel.left <- as.numeric(attr.vec[["x"]])
w <- as.numeric(attr.vec[["width"]])
panel.right <- panel.left + w
line.list <- getNodeSet(
  info$html,
  "//g[contains(@class, 'PANEL4')]//line")
attr.mat <- sapply(line.list, xmlAttrs)
seg.left <- as.numeric(attr.mat["x1",])
seg.right <- as.numeric(attr.mat["x2",])
n.left <- sum(seg.left == panel.left)
n.right <- sum(seg.right == panel.right)
test_that("one x at left of panel", {
  expect_equal(n.left, 1)
})
test_that("one x at right of panel", {
  expect_equal(n.right, 1)
})


limits <- data.frame(
  i=1:3,
  lower=c(-Inf, 0, -1),
  upper=c(1, 2, Inf))
pfac <- function(x){
  factor(x, c("left", "right"))
}
viz <- list(
  vert=ggplot()+
    ggtitle("vertical segments")+
    theme_bw()+
    theme(
      panel.grid.major=element_line(color="red"),
      panel.grid.minor=element_line(color="blue"),
      panel.margin=grid::unit(0, "lines"))+
    coord_cartesian(ylim=c(-0.5, 1.5))+
    geom_segment(aes(
      i, lower, yend=upper, xend=i),
      data=limits),
  hor=ggplot()+
    ggtitle("horizontal segments")+
    theme_bw()+
    theme(
      panel.grid.major=element_line(color="red"),
      panel.grid.minor=element_line(color="blue"),
      panel.margin=grid::unit(0, "lines"))+
    coord_cartesian(xlim=c(9.5, 11.5))+
    facet_grid(. ~ panel)+
    geom_segment(aes(
      lower+10, i+10,
      xend=upper+10, yend=i+10),
      data=data.frame(limits, panel=pfac("left")))+
    geom_point(aes(
      x, y),
      data=data.frame(
        x=10, y=12,
        panel=pfac(c("left", "right")))))
info <- animint2HTML(viz)

## First plot, test y values.
bg.rect <- getNodeSet(
  info$html,
  "//svg[@id='plot_vert']//rect[@class='background_rect']")[[1]]
attr.vec <- xmlAttrs(bg.rect)
panel.top <- as.numeric(attr.vec[["y"]])
h <- as.numeric(attr.vec[["height"]])
panel.bottom <- panel.top + h
line.list <- getNodeSet(
  info$html,
  "//g[@class='geom1_segment_vert']//line")
attr.mat <- sapply(line.list, xmlAttrs)
seg.bottom <- as.numeric(attr.mat["y1",])
seg.top <- as.numeric(attr.mat["y2",])
n.top <- sum(seg.top == panel.top)
n.bottom <- sum(seg.bottom == panel.bottom)
test_that("two y at top of panel", {
  expect_equal(n.top, 2)
})
test_that("two y at bottom of panel", {
  expect_equal(n.bottom, 2)
})

## second plot, test x values.
bg.rect <- getNodeSet(
  info$html,
  "//svg[@id='plot_hor']//rect[@class='background_rect']")[[1]]
attr.vec <- xmlAttrs(bg.rect)
panel.left <- as.numeric(attr.vec[["x"]])
w <- as.numeric(attr.vec[["width"]])
panel.right <- panel.left + w
line.list <- getNodeSet(
  info$html,
  "//g[@class='geom2_segment_hor']//line")
attr.mat <- sapply(line.list, xmlAttrs)
seg.left <- as.numeric(attr.mat["x1",])
seg.right <- as.numeric(attr.mat["x2",])
n.left <- sum(seg.left == panel.left)
n.right <- sum(seg.right == panel.right)
test_that("two x at left of panel", {
  expect_equal(n.left, 2)
})
test_that("two x at right of panel", {
  expect_equal(n.right, 2)
})
