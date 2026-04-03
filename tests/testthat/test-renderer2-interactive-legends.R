acontext("Interactive Legends")

## extract all <circle> elements under a particular <svg> element.
get_circles <- function(id) {
  getNodeSet(getHTML(), paste0("//svg[@id='plot_", id, "']//circle"))
}

iris$id <- 1:nrow(iris)
p1 <- ggplot() +
  guides(color="none")+
  geom_point(aes(Sepal.Length, Sepal.Width, colour = Species, 
                 size = Petal.Width, id = id), 
            clickSelects = "Species", data = iris) + 
  facet_wrap(~Species, nrow = 2) + 
  ggtitle("Sepal Data")
p2 <- ggplot() + 
  geom_point(data=iris, aes(Petal.Length, Petal.Width, colour = Species, 
                       size = Sepal.Width),
             showSelected = "Species") + 
  ggtitle("Petal Data")

viz <- list(sepal = p1, 
            petal = p2)
info <- animint2HTML(viz)

test_that("compiler adds selector.types and first", {
  expect_match(info$selector.types$Species, "multiple")
  expect_true(all({
    info$first$Species %in% c("setosa", "virginica", "versicolor")
  }))
})

test_that("all points are initially drawn", {
  expect_equal(length(get_circles("sepal")), 150)
  expect_equal(length(get_circles("petal")), 150)
})

test_that("clicking species legend adds and removes points", {
  # virginica points are removed
  clickID("plot_petal_Species_variable_virginica")
  expect_equal(length(get_circles("sepal")), 150)
  expect_equal(length(get_circles("petal")), 100)
  # virginica points are added back
  clickID("plot_petal_Species_variable_virginica")
  expect_equal(length(get_circles("sepal")), 150)
  expect_equal(length(get_circles("petal")), 150)
})

test_that("clicking sepal.width legend does nothing", {
  clickID("plot_petal_Sepal_Width_variable_2_5")
  expect_equal(length(get_circles("sepal")), 150)
  expect_equal(length(get_circles("petal")), 150)
})

test_that("clicking Sepal point doesn't affect sepal plot", {
  clickID("51")
  expect_equal(length(get_circles("petal")), 100)
  expect_equal(length(get_circles("sepal")), 150)
})

mtcars$am <- factor(mtcars$am)
p <- qplot(data=mtcars, mpg, hp, color = am)
info <- animint2HTML(list(p = p))

test_that("A plot with no show/click aes and a legend should be clickable", {
  expect_equal(length(get_circles("p")), 32)
  clickID("plot_p_am_variable_0")
  expect_equal(length(get_circles("p")), 13)
  clickID("plot_p_am_variable_0")
  expect_equal(length(get_circles("p")), 32)
})

viz <- list(p=qplot(data=mtcars, mpg, hp, color = factor(vs)))

test_that("aes(color=factor(vs)) is an error", {
  expect_error({
    info <- animint2HTML(viz)
  }, "need exactly 1 variable name")
})

mtcars$vs.num <- as.numeric(mtcars$vs)
mtcars$vs.fac <- factor(mtcars$vs)
mtcars$vs.fac2 <- factor(mtcars$vs)
seg <- data.frame(x=15, y=100, xend=30, yend=100, vs=1)
viz <- list(
  numeric=ggplot()+
    geom_point(aes(mpg, hp, color = vs, fill=vs),
               data=mtcars)+
    geom_segment(aes(x, y, xend=xend, yend=yend, color=vs),
                 data=seg),
  factor=ggplot()+
    geom_point(aes(mpg, hp, color = vs.fac, fill=vs.fac),
               data=mtcars)+
    geom_segment(aes(x, y, xend=xend, yend=yend),
                 data=seg)
  )

test_that("Two plots with both color and fill", {
  info <- animint2HTML(viz)
  expect_equal(length(get_circles("factor")), 32)
  clickID("plot_factor_vs_fac_variable_0")
  expect_equal(length(get_circles("factor")), 14)
  clickID("plot_factor_vs_fac_variable_0")
  expect_equal(length(get_circles("factor")), 32)
  td.list <- getNodeSet(
    info$html, '//tr[@class="vs_variable"]//td[@class="legend_entry_label"]')
  value.vec <- sapply(td.list, xmlValue)
  expect_identical(value.vec, c("1.00", "0.75", "0.50", "0.25", "0.00"))
  style.mat <- getStyleValue(
    info$html, '//table[@class="legend"]//circle', c("fill", "stroke"))
  expect_identical(style.mat["fill", ], style.mat["stroke", ])
  ## Make sure lines are rendered in the first but not second legend:
  left.lines <- getNodeSet(info$html, '//tr[@class="vs_variable"]//line')
  expect_equal(length(left.lines), 5)
  right.lines <- getNodeSet(info$html, '//tr[@class="vs_fac_variable"]//line')
  expect_equal(length(right.lines), 0)
  right.circles <- getNodeSet(
    info$html, '//tr[@class="vs_fac_variable"]//circle')
  expect_equal(length(right.circles), 2)
  ## Lines should be rendered in both plots:
  left.lines <-
    getNodeSet(info$html, '//g[@class="geom2_segment_numeric"]//line')
  expect_equal(length(left.lines), 1)
  right.lines <-
    getNodeSet(info$html, '//g[@class="geom4_segment_factor"]//line')
  expect_equal(length(right.lines), 1)
})

viz <- list(
  vs=ggplot()+
    geom_point(aes(mpg, hp, fill=factor(vs)),
               color="black",
               data=mtcars),
  am=ggplot()+
    geom_point(aes(mpg, hp, fill=factor(am)),
               color="black",
               data=mtcars)
  )

test_that("two plots with different aes(fill=factor(var))", {
  expect_error({
    info <- animint2HTML(viz)
  }, "need exactly 1 variable name")
})

vs0 <- subset(mtcars, vs == 0)
vs1 <- subset(mtcars, vs == 1)
viz <- list(
  p=ggplot()+
    scale_color_discrete("vs")+
    geom_point(aes(mpg, hp, color = "vs0"),
               data=vs0)+
    geom_point(aes(mpg, hp, color = "vs1"),
               data=vs1))

test_that('aes(color="constant") is an error"', {
  expect_error({
    info <- animint2HTML(viz)
  }, "need exactly 1 variable name")
})

viz <- list(
  p=ggplot()+
    geom_point(aes(mpg, hp, color = vs.num),
               data=vs0)+
    geom_point(aes(mpg, hp, color = vs),
               data=vs1))
test_that('aes(color=vs) aes(color=vs.num) is OK"', {
  info <- animint2HTML(viz)
  expect_equal(length(get_circles("p")), 32)
  tr.list <- getNodeSet(info$html, '//tr[@class="vs_num_legend"]')
  attr.mat <- sapply(tr.list, "xmlAttrs")
  expect_false("title" %in% rownames(attr.mat))
  expect_false("style" %in% rownames(attr.mat))
})

viz <- list(
  p=ggplot()+
    geom_point(aes(mpg, hp, color = vs.num, fill=vs.fac),
               shape=21,
               data=vs0)+
    geom_point(aes(mpg, hp, color = vs, fill=vs.fac),
               shape=21,
               data=vs1))
test_that('aes(color=vs, fill=vs.fac) aes(color=vs.num, fill=vs.fac) is OK"', {
  info <- animint2HTML(viz)
  expect_equal(length(get_circles("p")), 32)
  clickID("plot_p_vs_fac_variable_0")
  expect_equal(length(get_circles("p")), 14)
  clickID("plot_p_vs_fac_variable_0")
  expect_equal(length(get_circles("p")), 32)
  ## Stroke should be constant in the fill legend:
  style.mat <- getStyleValue(
    info$html, '//tr[@class="vs_fac_variable"]//circle', c("fill", "stroke"))
  expected.stroke <- rep(style.mat[["stroke", 1]], ncol(style.mat))
  expect_identical(style.mat["stroke", ], expected.stroke)
  ## Fill should be constant in the stroke legend:
  style.mat <- getStyleValue(
    info$html, '//tr[@class="vs_num_variable"]//circle', c("fill", "stroke"))
  expected.fill <- rep(style.mat[["fill", 1]], ncol(style.mat))
  expect_identical(style.mat["fill", ], expected.fill)
})

viz <- list(
  p=ggplot()+
    scale_color_discrete("vs")+
    geom_point(aes(mpg, hp, color = vs.fac),
               data=vs0)+
    geom_point(aes(mpg, hp, color = vs.fac),
               data=vs1))
test_that('aes(color=vs.fac) is OK"', {
  info <- animint2HTML(viz)
  expect_equal(length(get_circles("p")), 32)
  clickID("plot_p_vs_fac_variable_0")
  expect_equal(length(get_circles("p")), 14)
  clickID("plot_p_vs_fac_variable_0")
  expect_equal(length(get_circles("p")), 32)
})

viz <- list(
  p=ggplot()+
    scale_color_discrete("vs")+
    geom_point(aes(mpg, hp, color = vs.fac),
               data=vs0)+
    geom_point(aes(mpg, hp, color = vs.fac2),
               data=vs1))
test_that('aes(color=something), aes(color=something.else) is an error"', {
  expect_error({
    info <- animint2HTML(viz)
  }, "need exactly 1 variable name")
})

legend.opt.out.df <- data.frame(
  x = c(-10, -8, -6, -4, -2, -10, -8, -6, -4, -2),
  y = c(0.1, 0.2, 0.15, 0.25, 0.3, 0.05, 0.1, 0.08, 0.12, 0.15),
  comparison = rep(c("control", "treatment"), each = 5)
)
legend.opt.out.segments <- data.frame(
  comparison = c("control", "treatment"),
  x = c(-10, -10),
  y = c(0.35, 0.4),
  xend = c(-2, -2),
  yend = c(0.35, 0.4)
)
viz <- list(
  mixed = ggplot() +
    geom_point(
      aes(x, y, color = comparison),
      showSelected = "comparison",
      data = legend.opt.out.df
    ) +
    geom_segment(
      aes(x, y, xend = xend, yend = yend, color = comparison),
      showSelected = character(),
      data = legend.opt.out.segments
    )
)
info <- animint2HTML(viz)
test_that("legend click updates selection while opted-out layer does not auto-filter", {
  legend.info <- info$plots$mixed$legend$comparison
  expect_false(is.null(legend.info))
  expect_gt(length(legend.info$entries), 0)
  ## Expected behavior for #140: legend should remain interactive.
  expect_identical(legend.info$selector, "comparison")
  expect_identical("comparison" %in% names(info$selectors), TRUE)
  points.before <- length(getNodeSet(
    info$html,
    '//g[@class="geom1_point_mixed"]//circle'
  ))
  segments.before <- length(getNodeSet(
    info$html,
    '//g[@class="geom2_segment_mixed"]//line'
  ))
  expect_equal(points.before, 10)
  expect_equal(segments.before, 2)
  clickID("plot_mixed_comparison_variable_control")
  html.after.one.click <- getHTML()
  points.after.one.click <- length(getNodeSet(
    html.after.one.click,
    '//g[@class="geom1_point_mixed"]//circle'
  ))
  segments.after.one.click <- length(getNodeSet(
    html.after.one.click,
    '//g[@class="geom2_segment_mixed"]//line'
  ))
  ## selection changed: one comparison level removed from the point layer
  expect_equal(points.after.one.click, 5)
  ## opted-out segment layer should not be auto-filtered by legend clicks
  expect_equal(segments.after.one.click, segments.before)
  clickID("plot_mixed_comparison_variable_control")
  html.after.two.clicks <- getHTML()
  points.after.two.clicks <- length(getNodeSet(
    html.after.two.clicks,
    '//g[@class="geom1_point_mixed"]//circle'
  ))
  segments.after.two.clicks <- length(getNodeSet(
    html.after.two.clicks,
    '//g[@class="geom2_segment_mixed"]//line'
  ))
  expect_equal(points.after.two.clicks, points.before)
  expect_equal(segments.after.two.clicks, segments.before)
})
