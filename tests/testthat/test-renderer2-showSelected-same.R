acontext("showSelected=var, showSelected2=var")

viz <- list(
  points=a_plot()+
    a_geom_point(a_aes(Sepal.Length, Petal.Length),
               showSelected=c("Species", "Species"),
               data=iris)
  )
info <- animint2HTML(viz)
ss.list <- with(info$geoms$a_geom1_point_points, c(chunk_order, subset_order))
ss.vec <- unlist(ss.list)

test_that("redundant showSelected are optimized out", {
  expect_equal(length(ss.vec), 1)
})

test_that("50 <circle> rendered at first", {
  circle.list <- getNodeSet(
    info$html, '//g[@class="a_geom1_point_points"]//circle')
  expect_equal(length(circle.list), 50)
})

test_that("redundant a_aes not saved to tsv", {
  a_geom.tsv <- Sys.glob(
    file.path("animint-htmltest", "a_geom1_point_points_*.tsv"))
  expect_equal(length(a_geom.tsv), 1)
  expected.names <- sort(c("x", "y", ss.vec))
  tsv.data <- read.table(a_geom.tsv, header=TRUE, comment.char = "")
  computed.names <- sort(names(tsv.data))
  expect_identical(computed.names, expected.names)
})

test_that("selector widgets table initially visible", {
  display <- getStyleValue(
    info$html, '//table[@class="table_selector_widgets"]',
    "display")
  ## If display is NA then display style is undefined and the table is
  ## rendered.
  expect_true(is.na(display))
})

viz <- list(
  length=a_plot()+
    a_geom_point(a_aes(Sepal.Length, Petal.Length,
                   color=Species),
               showSelected="Species",
               data=iris)
  )
info <- animint2HTML(viz)

test_that("150 <circle> rendered in first plot", {
  circle.list <- getNodeSet(
    info$html, '//g[@class="a_geom1_point_length"]//circle')
  expect_equal(length(circle.list), 150)
})

test_that("redundant showSelected and color optimized", {
  var.list <- with(info$geoms$a_geom1_point_length, c(chunk_order, subset_order))
  expect_equal(length(var.list), 1)
})

test_that("selector widgets table initially invisible", {
  display <- getStyleValue(
    info$html, '//table[@class="table_selector_widgets"]',
    "display")
  expect_match(display, "none")
})

viz <- list(
  length=a_plot()+
    a_geom_point(a_aes(Sepal.Length, Petal.Length),
               showSelected="Species",
               data=iris),
  width=a_plot()+
    a_geom_point(a_aes(Sepal.Width, Petal.Width),
               clickSelects="Species",
               data=iris)
  )
info <- animint2HTML(viz)

test_that("50 <circle> rendered in first plot", {
  circle.list <- getNodeSet(
    info$html, '//g[@class="a_geom1_point_length"]//circle')
  expect_equal(length(circle.list), 50)
})

test_that("redundant showSelected and color optimized", {
  var.list <- with(info$geoms$a_geom1_point_length, c(chunk_order, subset_order))
  expect_equal(length(var.list), 1)
})

test_that("selector widgets table initially invisible", {
  display <- getStyleValue(
    info$html, '//table[@class="table_selector_widgets"]',
    "display")
  expect_match(display, "none")
})

## This test responds to @kferris10 who asked the question "What will
## happen if there is only one level? Will it render as an array or an
## object?" https://github.com/tdhock/animint/pull/115 We were talking
## about what to do with the selector widget when there is a
## showSelected variable which has only 1 level. However that is a
## trivial case for a showSelected variable, so the user should be
## warned. Ideally it should be optimized out by the compiler but
## currently that would be too complicated (first tsv files including
## potentially trivial showSelected columns are saved, then levels are
## determined).
iris$kingdom <- "plantae"
viz <- list(
  points=a_plot()+
    a_geom_point(a_aes(Petal.Length, Sepal.Length,
                   color=Species),
               showSelected=c("Species", "kingdom"),
               data=iris)
  )

test_that("compiler warns for showSelected with 1 level", {
  expect_warning({
    info <- animint2HTML(viz)
  }, "showSelected variables with only 1 level")
  circle.list <- getNodeSet(
    info$html, '//g[@class="a_geom1_point_points"]//circle')
  expect_equal(length(circle.list), 150)
  display <- getStyleValue(
    info$html, '//table[@class="table_selector_widgets"]',
    "display")
  expect_match(display, "none")
})

