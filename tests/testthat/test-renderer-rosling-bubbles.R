library(testthat)
library(animint2)
library(XML)

context("Rosling Bubbles")

## ── 1. Data ──────────────────────────────────────────────────────────────────
set.seed(42)
rosling_data <- data.frame(
  id   = factor(rep(paste("Subject", 1:10), times = 5)),
  year = rep(1:5, each = 10),
  x    = rnorm(50, 5, 2),
  y    = rnorm(50, 5, 2),
  size = runif(50, 1, 5)
)

## ── 2. Viz ───────────────────────────────────────────────────────────────────
viz <- list(
  bubbles = ggplot() +
    geom_point(
      data = rosling_data,
      aes(x = x, y = y, size = size, fill = id, key = id),
      showSelected = "year", 
      clickSelects = "id",   
      shape = 21,
      color = "black"
    ) +
    ggtitle("Interactive Bubbles Plot"),
  time = list(variable = "year", ms = 1000)
)

## ── 3. Render & Wait ────────────────────────────────────────────────────────
info <- animint2HTML(viz)
test_url <- "http://127.0.0.1:4848/animint-htmltest/index.html"
remDr$navigate(test_url)

# Give it plenty of time to finish the first year animation
Sys.sleep(10) 
info$html <- XML::htmlParse(remDr$getPageSource(), asText = TRUE)

## ── 4. Tests ─────────────────────────────────────────────────────────────────

test_that("SVG container for bubbles plot exists", {
  svg_nodes <- getNodeSet(info$html, '//svg[@id="plot_bubbles"]')
  expect_true(length(svg_nodes) >= 1L)
})

test_that("Correct number of circles rendered in the plot area", {
  xpath <- '//svg[@id="plot_bubbles"]//g[contains(@class, "geom")]//circle'
  circles <- getNodeSet(info$html, xpath)
  expect_equal(length(circles), 10L)
})

test_that("Each circle has a numeric cx attribute", {
  xpath <- '//svg[@id="plot_bubbles"]//g[contains(@class, "geom")]//circle'
  circles <- getNodeSet(info$html, xpath)
  cx_values <- as.numeric(sapply(circles, xmlGetAttr, "cx"))
  expect_true(all(!is.na(cx_values)))
})

test_that("Animation widgets (Play/Pause) are present", {
  play_pause <- getNodeSet(info$html, '//*[@id="play_pause"] | //input[@value="Play" or @value="Pause"]')
  expect_true(length(play_pause) >= 1L)
})

test_that("clickSelects: circles have interactive attributes", {
  xpath <- '//svg[@id="plot_bubbles"]//g[contains(@class, "geom")]//circle'
  circles <- getNodeSet(info$html, xpath)
  has_attr <- sapply(circles, function(n) {
    !is.null(xmlGetAttr(n, "id")) || !is.null(xmlGetAttr(n, "class"))
  })
  expect_true(all(has_attr))
})

test_that("Year selector is registered in info object", {
  expect_true("year" %in% names(info$selectors))
})