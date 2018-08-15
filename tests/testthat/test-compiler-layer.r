context("a_layer")


# Parameters --------------------------------------------------------------

test_that("a_aesthetics go in a_aes_params", {
  l <- a_geom_point(size = "red")
  expect_equal(l$a_aes_params, list(size = "red"))
})

test_that("unknown params create error", {
  skip("passes when validate_params=TRUE")
expect_error(a_geom_point(blah = "red"), "Unknown parameters")
  })

test_that("Unknown params create error with validate_params = TRUE", {
  expect_error(a_geom_point(blah = "red", validate_params = TRUE),
               "Unknown parameters")
})

test_that("Unknown params don't create error with validate_params = FALSE", {
  expect_silent(a_geom_point(blah = "red", validate_params = FALSE))
})

test_that("Unknown params go in extra_params, not a_aes_params", {
  l <- a_geom_point(some_param = "value1",
                  size = "big",
                  validate_params = FALSE)
  expect_equal(l$extra_params, list(some_param = "value1"))
  expect_equal(l$a_aes_params, list(size = "big"))
})

# Calculated aesthetics ---------------------------------------------------

test_that("Bare name surround by .. is calculated", {
  expect_true(is_calculated_aes(a_aes(..density..)))
  expect_true(is_calculated_aes(a_aes(..DENSITY..)))
  expect_false(is_calculated_aes(a_aes(a..x..b)))
})

test_that("Calling using variable surround by .. is calculated", {
  expect_true(is_calculated_aes(a_aes(mean(..density..))))
  expect_true(is_calculated_aes(a_aes(mean(..DENSITY..))))
  expect_false(is_calculated_aes(a_aes(mean(a..x..b))))
})

test_that("strip_dots remove dots around calculated aesthetics", {
  expect_equal(strip_dots(a_aes(..density..))$x, quote(density))
  expect_equal(strip_dots(a_aes(mean(..density..)))$x, quote(mean(density)))
  expect_equal(strip_dots(a_aes(sapply(..density.., function(x) mean(x)))$x),
               quote(sapply(density, function(x) mean(x))))
})

# Data extraction ---------------------------------------------------------

test_that("a_layer_data returns a data.frame", {
  l <- a_geom_point()
  expect_equal(l$a_layer_data(mtcars), mtcars)
  l <- a_geom_point(data = head(mtcars))
  expect_equal(l$a_layer_data(mtcars), head(mtcars))
  l <- a_geom_point(data = head)
  expect_equal(l$a_layer_data(mtcars), head(mtcars))
  l <- a_geom_point(data = nrow)
  expect_error(l$a_layer_data(mtcars), "Data function must return a data.frame")
})
