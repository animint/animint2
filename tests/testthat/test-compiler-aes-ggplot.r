context("Creating aesthetic mappings")

test_that("a_aes() captures input expressions", {
  out <- a_aes(mpg, wt + 1)
  expect_equal(out$x, quote(mpg))
  expect_equal(out$y, quote(wt + 1))
})

test_that("a_aes_q() uses quoted calls and formulas", {
  out <- a_aes_q(quote(mpg), ~ wt + 1)
  expect_equal(out$x, quote(mpg))
  expect_equal(out$y, quote(wt + 1))
})

test_that("a_aes_string() parses strings", {
  expect_equal(a_aes_string("a + b")$x, quote(a + b))
})

test_that("a_aes_string() doesn't parse non-strings", {
  old <- options(OutDec = ",")
  on.exit(options(old))

  expect_equal(a_aes_string(0.4)$x, 0.4)
})

test_that("a_aes_q() & a_aes_string() preserves explicit NULLs", {
  expect_equal(a_aes_q(NULL), a_aes(NULL))
  expect_equal(a_aes_q(x = NULL), a_aes(NULL))
  expect_equal(a_aes_q(colour = NULL), a_aes(colour = NULL))

  expect_equal(a_aes_string(NULL), a_aes(NULL))
  expect_equal(a_aes_string(x = NULL), a_aes(NULL))
  expect_equal(a_aes_string(colour = NULL), a_aes(colour = NULL))
})

test_that("a_aes_all() converts strings into mappings", {
  expect_equal(
    a_aes_all(c("x", "y", "col", "pch")),
    a_aes(x, y, colour = col, shape = pch)
  )
})

test_that("a_aes evaluated in environment where plot created", {
  df <- data.frame(x = 1, y = 1)
  p <- a_plot(df, a_aes(foo, y)) + a_geom_point()

  # Accessing an undefined variable should result in error
  expect_error(a_layer_data(p), "'foo' not found")

  # Once it's defined we should get it back
  foo <- 0
  expect_equal(a_layer_data(p)$x, 0)

  # And regular variable shadowing should work
  f <- function() {
    foo <- 10
    a_plot(df, a_aes(foo, y)) + a_geom_point()
  }
  expect_equal(a_layer_data(f())$x, 10)
})
