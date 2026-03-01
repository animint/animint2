library(testthat)
library(animint2)
library(XML)

context("Rosling Bubbles")

# 1. data
set.seed(42)
rosling_data <- data.frame(
  id   = factor(rep(paste("Subject", 1:10), times = 5)),
  year = rep(1:5, each = 10),
  x    = rnorm(50, 5, 2),
  y    = rnorm(50, 5, 2),
  size = runif(50, 1, 5)
)

# 2. viz
viz <- list(
  bubbles = ggplot() +
    geom_point(
      data = rosling_data,
      aes(x = x, y = y, size = size, fill = id, key = id),
      showSelected = "year",
      clickSelects = "id",
      shape = 21
    ) +
    ggtitle("Interactive Bubbles Plot"),
  time = list(variable = "year", ms = 1000)
)

# 3. render
info <- animint2HTML(viz)

# 4. tests
test_that("animint2HTML returns a non-empty HTML document", {
  html_str <- saveXML(info$html)
  expect_true(nchar(html_str) > 0)
})

test_that("SVG container for bubbles plot exists", {
  svg_nodes <- getNodeSet(info$html, '//svg[@id="plot_bubbles"]')
  expect_equal(length(svg_nodes), 1L)
})

test_that("Correct number of circles rendered for the first year", {
  circles <- getNodeSet(info$html, '//svg[@id="plot_bubbles"]//circle')
  expect_equal(length(circles), 10L)
})

test_that("Circles have numeric position attributes", {
  circles <- getNodeSet(info$html, '//svg[@id="plot_bubbles"]//circle')
  cx_values <- as.numeric(sapply(circles, xmlGetAttr, "cx"))
  cy_values <- as.numeric(sapply(circles, xmlGetAttr, "cy"))
  expect_true(all(!is.na(cx_values)))
  expect_true(all(!is.na(cy_values)))
})

test_that("Animation widgets are present", {
  # Check for year selector or play/pause buttons
  widgets <- getNodeSet(
    info$html,
    '//table[contains(@class,"selector")]//td | //input[@value="Play" or @value="Pause"]'
  )
  expect_true(length(widgets) >= 1L)
})