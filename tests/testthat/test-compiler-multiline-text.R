context("Multi-line text rendering (Issue #221)")

test_that("plot title supports multi-line text", {
  data <- data.frame(x = 1:5, y = 1:5)
  viz <- list(
    plot1 = ggplot(data, aes(x, y)) +
      geom_point() +
      ggtitle("Title Line 1\nTitle Line 2")
  )
  info <- animint2dir(viz, "test-title-multiline", open.browser = FALSE)
  expect_equal(info$plots$plot1$title, "Title Line 1<br/>Title Line 2")
})

test_that("x-axis title supports multi-line text", {
  data <- data.frame(x = 1:5, y = 1:5)
  viz <- list(
    plot1 = ggplot(data, aes(x, y)) +
      geom_point() +
      xlab("X Axis\nLine 2")
  )
  info <- animint2dir(viz, "test-xaxis-multiline", open.browser = FALSE)
  expect_equal(info$plots$plot1$xtitle, "X Axis<br/>Line 2")
})

test_that("y-axis title supports multi-line text", {
  data <- data.frame(x = 1:5, y = 1:5)
  viz <- list(
    plot1 = ggplot(data, aes(x, y)) +
      geom_point() +
      ylab("Y Axis\nLine 2")
  )
  info <- animint2dir(viz, "test-yaxis-multiline", open.browser = FALSE)
  expect_equal(info$plots$plot1$ytitle, "Y Axis<br/>Line 2")
})

test_that("geom_text labels support multi-line text", {
  data <- data.frame(
    x = 1:3, y = 1:3,
    label = c("One", "Two\nLines", "Three\nLines\nHere")
  )
  viz <- list(
    plot1 = ggplot(data, aes(x, y, label = label)) + geom_text()
  )
  info <- animint2dir(viz, "test-geomtext-multiline", open.browser = FALSE)
  tsv_files <- list.files(info$out.dir, pattern = "text.*\\.tsv$", full.names = TRUE)
  expect_equal(length(tsv_files), 1)
  text.df <- read.table(tsv_files, header = TRUE, sep = "\t", quote = "\"")
  has.br <- grepl("<br/>", text.df$label, fixed = TRUE)
  multiline_labels <- text.df$label[has.br]
  expect_equal(length(multiline_labels), 2)
  expect_match(multiline_labels, "Two<br/>Lines", fixed = TRUE, all = FALSE)
  expect_match(multiline_labels, "Three<br/>Lines<br/>Here",
               fixed = TRUE, all = FALSE)
})

test_that("legend title supports multi-line text", {
  data <- data.frame(x = 1:6, y = 1:6, category = rep(c("A", "B", "C"), 2))
  viz <- list(
    plot1 = ggplot(data, aes(x, y, color = category)) +
      geom_point() +
      scale_color_discrete(name = "Category\nName")
  )
  info <- animint2dir(viz, "test-legend-multiline", open.browser = FALSE)
  expect_equal(
    info$plots$plot1$legend$category$title, "Category<br/>Name")
})

test_that("convertNewlinesToBreaks works correctly", {
  expect_equal(animint2:::convertNewlinesToBreaks("Line1\nLine2"), "Line1<br/>Line2")
  expect_equal(animint2:::convertNewlinesToBreaks("A\nB\nC\nD"), "A<br/>B<br/>C<br/>D")
  expect_equal(animint2:::convertNewlinesToBreaks("No newlines here"), "No newlines here")
  expect_equal(animint2:::convertNewlinesToBreaks(""), "")
  result <- animint2:::convertNewlinesToBreaks(c("A\nB", "C", "D\nE\nF"))
  expect_equal(result, c("A<br/>B", "C", "D<br/>E<br/>F"))
})
