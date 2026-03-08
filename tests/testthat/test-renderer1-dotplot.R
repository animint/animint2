library(testthat)
library(animint2)

test_that("geom_dotplot shows error", {
  expect_error(
    geom_dotplot(),
    "geom_point"
  )
})

test_that("geom_point creates a plot object", {
  df <- data.frame(x = "A", y = c(1, 1.1, 1.2, 1.3, 100, 200))
  p <- ggplot() + geom_point(aes(x, y), data = df)
  
  expect_true(is.ggplot(p))
})

test_that("geom_point produces no warnings", {
  df <- data.frame(x = "A", y = c(1, 1.1, 1.2, 1.3, 100, 200))
  
  expect_no_warning(
    ggplot() + geom_point(aes(x, y), data = df)
  )
})

test_that("geom_point produces no errors", {
  df <- data.frame(x = "A", y = c(1, 1.1, 1.2, 1.3, 100, 200))
  
  expect_no_error(
    ggplot() + geom_point(aes(x, y), data = df)
  )
})