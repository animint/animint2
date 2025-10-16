# Test for Issue #221: Multi-line text support throughout animint2
# Tests that newline characters (\n) work in all text contexts

library(animint2)
library(testthat)

context("Multi-line text rendering (Issue #221)")

# Test 1: Plot title with newlines
test_that("plot title supports multi-line text", {
  data <- data.frame(x = 1:5, y = 1:5)
  viz <- list(
    plot1 = ggplot(data, aes(x, y)) +
      geom_point() +
      ggtitle("Title Line 1\nTitle Line 2")
  )
  info <- animint2dir(viz, "test-title-multiline", open.browser = FALSE)
  json <- jsonlite::fromJSON(file.path(info$out.dir, "plot.json"))
  expect_true(grepl("<br/>", json$plots$plot1$title, fixed = TRUE))
  expect_equal(json$plots$plot1$title, "Title Line 1<br/>Title Line 2")
})

# Test 2: X-axis title with newlines
test_that("x-axis title supports multi-line text", {
  data <- data.frame(x = 1:5, y = 1:5)
  viz <- list(
    plot1 = ggplot(data, aes(x, y)) +
      geom_point() +
      xlab("X Axis\nLine 2")
  )
  info <- animint2dir(viz, "test-xaxis-multiline", open.browser = FALSE)
  json <- jsonlite::fromJSON(file.path(info$out.dir, "plot.json"))
  expect_true(grepl("<br/>", json$plots$plot1$xtitle, fixed = TRUE))
  expect_equal(json$plots$plot1$xtitle, "X Axis<br/>Line 2")
})

# Test 3: Y-axis title with newlines
test_that("y-axis title supports multi-line text", {
  data <- data.frame(x = 1:5, y = 1:5)
  viz <- list(
    plot1 = ggplot(data, aes(x, y)) +
      geom_point() +
      ylab("Y Axis\nLine 2")
  )
  info <- animint2dir(viz, "test-yaxis-multiline", open.browser = FALSE)
  json <- jsonlite::fromJSON(file.path(info$out.dir, "plot.json"))
  expect_true(grepl("<br/>", json$plots$plot1$ytitle, fixed = TRUE))
  expect_equal(json$plots$plot1$ytitle, "Y Axis<br/>Line 2")
})

# Test 4: geom_text labels with newlines
test_that("geom_text labels support multi-line text", {
  data <- data.frame(
    x = 1:3,
    y = 1:3,
    label = c("One", "Two\nLines", "Three\nLines\nHere")
  )
  viz <- list(
    plot1 = ggplot(data, aes(x, y, label = label)) +
      geom_text()
  )
  info <- animint2dir(viz, "test-geomtext-multiline", open.browser = FALSE)
  tsv_files <- list.files(info$out.dir, pattern = "text.*\\.tsv$", full.names = TRUE)
  expect_true(length(tsv_files) > 0)
  text_data <- read.table(tsv_files[1], header = TRUE, sep = "\t", quote = "\"")
  multiline_labels <- text_data$label[grepl("<br/>", text_data$label, fixed = TRUE)]
  expect_true(length(multiline_labels) >= 2)
  expect_true(any(grepl("Two<br/>Lines", multiline_labels, fixed = TRUE)))
  expect_true(any(grepl("Three<br/>Lines<br/>Here", multiline_labels, fixed = TRUE)))
})

# Test 5: Legend title with newlines
test_that("legend title supports multi-line text", {
  data <- data.frame(
    x = 1:6,
    y = 1:6,
    category = rep(c("A", "B", "C"), 2)
  )
  viz <- list(
    plot1 = ggplot(data, aes(x, y, color = category)) +
      geom_point() +
      scale_color_discrete(name = "Category\nName")
  )
  info <- animint2dir(viz, "test-legend-multiline", open.browser = FALSE)
  json <- jsonlite::fromJSON(file.path(info$out.dir, "plot.json"))
  expect_true("legend" %in% names(json$plots$plot1))
  legend_keys <- names(json$plots$plot1$legend)
  expect_true(length(legend_keys) > 0)
  has_multiline_title <- FALSE
  for (key in legend_keys) {
    legend_title <- json$plots$plot1$legend[[key]]$title
    if (!is.null(legend_title) && grepl("<br/>", legend_title, fixed = TRUE)) {
      has_multiline_title <- TRUE
      expect_equal(legend_title, "Category<br/>Name")
      break
    }
  }
  expect_true(has_multiline_title)
})

# Test 6: Tooltip text with newlines (already works, but let's ensure it still does)
test_that("tooltip text supports multi-line text (regression test)", {
  data <- data.frame(
    x = 1:3,
    y = 1:3,
    tooltip = c("Single", "Two\nLines", "Three\nLines\nText")
  )
  viz <- list(
    plot1 = ggplot(data, aes(x, y, tooltip = tooltip)) +
      geom_point()
  )
  info <- animint2dir(viz, "test-tooltip-multiline", open.browser = FALSE)
  tsv_files <- list.files(info$out.dir, pattern = "point.*\\.tsv$", full.names = TRUE)
  expect_true(length(tsv_files) > 0)
  point_data <- read.table(tsv_files[1], header = TRUE, sep = "\t", quote = "\"")
  expect_true("tooltip" %in% names(point_data))
  expect_true(any(grepl("\n", point_data$tooltip, fixed = TRUE)))
})

# Test 7: Helper function convertNewlinesToBreaks
test_that("convertNewlinesToBreaks works correctly", {
  expect_equal(
    animint2:::convertNewlinesToBreaks("Line1\nLine2"),
    "Line1<br/>Line2"
  )
  expect_equal(
    animint2:::convertNewlinesToBreaks("A\nB\nC\nD"),
    "A<br/>B<br/>C<br/>D"
  )
  expect_equal(
    animint2:::convertNewlinesToBreaks("No newlines here"),
    "No newlines here"
  )
  expect_equal(
    animint2:::convertNewlinesToBreaks(""),
    ""
  )
  result <- animint2:::convertNewlinesToBreaks(c("A\nB", "C", "D\nE\nF"))
  expect_equal(result, c("A<br/>B", "C", "D<br/>E<br/>F"))
})

cat("\n===============================================\n")
cat("All multi-line text tests completed!\n")
cat("===============================================\n")
