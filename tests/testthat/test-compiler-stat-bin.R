context("stat_bin/stat_count")

test_that("a_stat_bin throws error when y aesthetic present", {
  dat <- data.frame(x = c("a", "b", "c"), y = c(1, 5, 10))

  expect_error(a_plot_build(a_plot(dat, a_aes(x, y)) + a_stat_bin()),
    "must not be used with a y aesthetic.")

  skip("passes when validate_params=TRUE")
  expect_error(p <- a_plot_build(a_plot(dat, a_aes(x)) + a_stat_bin(y = 5)),
               "StatBin requires a continuous x variable the x variable is discrete")
})

test_that("bins specifies the number of bins", {
  df <- data.frame(x = 1:10)
  out <- function(x, ...) {
    a_layer_data(a_plot(df, a_aes(x)) + a_geom_histogram(...))
  }

  expect_equal(nrow(out(bins = 2)), 2)
  expect_equal(nrow(out(bins = 100)), 100)
})

test_that("a_geom_histogram defaults to pad = FALSE", {
  df <- data.frame(x = 1:3)
  out <- a_layer_data(a_plot(df, a_aes(x)) + a_geom_histogram(binwidth = 1))

  expect_equal(out$count, c(1, 1, 1))
})

test_that("a_geom_freqpoly defaults to pad = TRUE", {
  df <- data.frame(x = 1:3)
  out <- a_layer_data(a_plot(df, a_aes(x)) + a_geom_freqpoly(binwidth = 1))

  expect_equal(out$count, c(0, 1, 1, 1, 0))
})


# Underlying binning algorithm --------------------------------------------

comp_bin <- function(df, ...) {
  plot <- a_plot(df, a_aes(x = x)) + a_stat_bin(...)
  a_layer_data(plot)
}

test_that("Closed left or right", {
  dat <- data.frame(x = c(0, 10))

  res <- comp_bin(dat, binwidth = 10, pad = FALSE)
  expect_identical(res$count, c(1, 1))
  res <- comp_bin(dat, binwidth = 10, boundary = 5, pad = FALSE)
  expect_identical(res$count, c(1, 1))
  res <- comp_bin(dat, binwidth = 10, boundary = 0, pad = FALSE)
  expect_identical(res$count, 2)
  res <- comp_bin(dat, binwidth = 5, boundary = 0, pad = FALSE)
  expect_identical(res$count, c(1, 1))

  res <- comp_bin(dat, binwidth = 10, pad = FALSE, closed = "left")
  expect_identical(res$count, c(1, 1))
  res <- comp_bin(dat, binwidth = 10, boundary = 5, pad = FALSE, closed = "left")
  expect_identical(res$count, c(1, 1))
  res <- comp_bin(dat, binwidth = 10, boundary = 0, pad = FALSE, closed = "left")
  expect_identical(res$count, c(2))
  res <- comp_bin(dat, binwidth = 5, boundary = 0, pad = FALSE, closed = "left")
  expect_identical(res$count, c(1, 1))
})


test_that("Setting boundary and center", {
  # numeric
  df <- data.frame(x = c(0, 30))

  # Error if both boundary and center are specified
  expect_error(comp_bin(df, boundary = 5, center = 0), "one of `boundary` and `center`")

  res <- comp_bin(df, binwidth = 10, boundary = 0, pad = FALSE)
  expect_identical(res$count, c(1, 0, 1))
  expect_identical(res$xmin[1], 0)
  expect_identical(res$xmax[3], 30)

  res <- comp_bin(df, binwidth = 10, center = 0, pad = FALSE)
  expect_identical(res$count, c(1, 0, 0, 1))
  expect_identical(res$xmin[1], df$x[1] - 5)
  expect_identical(res$xmax[4], df$x[2] + 5)
})

test_that("weights are added", {
  df <- data.frame(x = 1:10, y = 1:10)
  p <- a_plot(df, a_aes(x = x, weight = y)) + a_geom_histogram(binwidth = 1)
  out <- a_layer_data(p)

  expect_equal(out$count, df$y)
})


# a_stat_count --------------------------------------------------------------

test_that("a_stat_count throws error when y aesthetic present", {
  dat <- data.frame(x = c("a", "b", "c"), y = c(1, 5, 10))

  expect_error(a_plot_build(a_plot(dat, a_aes(x, y)) + a_stat_count()),
    "must not be used with a y aesthetic.")
  skip("passes when validate_params=TRUE")
  expect_error(p <- a_plot_build(a_plot(dat, a_aes(x)) + a_stat_count(y = 5)),
    "must not be used with a y aesthetic.")
})

test_that("a_stat_count preserves x order for continuous and discrete", {
  # x is numeric
  b <- a_plot_build(a_plot(mtcars, a_aes(carb)) + a_geom_bar())
  expect_identical(b$data[[1]]$x, c(1,2,3,4,6,8))
  expect_identical(b$data[[1]]$y, c(7,10,3,10,1,1))

  # x is factor where levels match numeric order
  mtcars$carb2 <- factor(mtcars$carb)
  b <- a_plot_build(a_plot(mtcars, a_aes(carb2)) + a_geom_bar())
  expect_identical(b$data[[1]]$x, 1:6)
  expect_identical(b$data[[1]]$y, c(7,10,3,10,1,1))

  # x is factor levels differ from numeric order
  mtcars$carb3 <- factor(mtcars$carb, levels = c(4,1,2,3,6,8))
  b <- a_plot_build(a_plot(mtcars, a_aes(carb3)) + a_geom_bar())
  expect_identical(b$data[[1]]$x, 1:6)
  expect_identical(b$panel$ranges[[1]]$x.a_labels, c("4","1","2","3","6","8"))
  expect_identical(b$data[[1]]$y, c(10,7,10,3,1,1))
})
