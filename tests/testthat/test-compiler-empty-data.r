context('Empty data')

df0 <- data.frame(mpg = numeric(0), wt = numeric(0), am = numeric(0), cyl = numeric(0))

test_that("layers with empty data are silently omitted", {
  # Empty data (no visible points)
  d <- a_plot(df0, a_aes(mpg,wt)) + a_geom_point()
  expect_equal(nrow(a_layer_data(d)), 0)

  d <- a_plot() + a_geom_point(data = df0, a_aes(mpg,wt))
  expect_equal(nrow(a_layer_data(d)), 0)

  # Regular mtcars data, x=mpg, y=wt, normal points and points from empty data frame
  d <- a_plot(mtcars, a_aes(mpg, wt)) + a_geom_point() + a_geom_point(data = df0)
  expect_equal(nrow(a_layer_data(d, 1)), nrow(mtcars))
  expect_equal(nrow(a_layer_data(d, 2)), 0)

  # Regular mtcars data, but points only from empty data frame
  d <- a_plot(mtcars, a_aes(mpg, wt)) + a_geom_point(data = df0)
  expect_equal(nrow(a_layer_data(d, 1)), 0)
})


test_that("plots with empty data and vectors for aesthetics work", {
  d <- a_plot(NULL, a_aes(1:5, 1:5)) + a_geom_point()
  expect_equal(nrow(a_layer_data(d)), 5)

  d <- a_plot(data.frame(), a_aes(1:5, 1:5)) + a_geom_point()
  expect_equal(nrow(a_layer_data(d)), 5)

  d <- a_plot() + a_geom_point(a_aes(1:5, 1:5))
  expect_equal(nrow(a_layer_data(d)), 5)
})


test_that("layers with empty data are silently omitted with a_facet_wrap", {
  # Empty data, facet_wrap, throws error
  d <- a_plot(df0, a_aes(mpg, wt)) +
    a_geom_point() +
    a_facet_wrap(~cyl)
  expect_error(a_layer_data(d), "must have at least one value")

  d <- d + a_geom_point(data = mtcars)
  expect_equal(nrow(a_layer_data(d, 1)), 0)
  expect_equal(nrow(a_layer_data(d, 2)), nrow(mtcars))
})

test_that("layers with empty data are silently omitted with facet_grid", {
  d <- a_plot(df0, a_aes(mpg, wt)) +
    a_geom_point() +
    a_facet_grid(am ~ cyl)
  expect_error(a_layer_data(d), "must have at least one value")

  d <- d + a_geom_point(data = mtcars)
  expect_equal(nrow(a_layer_data(d, 1)), 0)
  expect_equal(nrow(a_layer_data(d, 2)), nrow(mtcars))
})


test_that("empty data overrides plot defaults", {
  # Should error when totally empty data frame because there's no x and y
  d <- a_plot(mtcars, a_aes(mpg, wt)) +
    a_geom_point() +
    a_geom_point(data = data.frame())
  expect_error(a_layer_data(d), "not found")

  # No extra points when x and y vars don't exist but are set
  d <- a_plot(mtcars, a_aes(mpg, wt)) +
    a_geom_point() +
    a_geom_point(data = data.frame(), x = 20, y = 3)
  expect_equal(nrow(a_layer_data(d, 1)), nrow(mtcars))
  expect_equal(nrow(a_layer_data(d, 2)), 0)

  # No extra points when x and y vars are empty, even when aesthetics are set
  d <- a_plot(mtcars, a_aes(mpg, wt)) +
    a_geom_point() +
    a_geom_point(data = df0, x = 20, y = 3)
  expect_equal(nrow(a_layer_data(d, 1)), nrow(mtcars))
  expect_equal(nrow(a_layer_data(d, 2)), 0)
})

test_that("a_layer inherits data from plot when data = NULL", {
  d <- a_plot(mtcars, a_aes(mpg, wt)) +
    a_geom_point(data = NULL)
  expect_equal(nrow(a_layer_data(d)), nrow(mtcars))
})

test_that("empty layers still generate one grob per panel", {
  df <- data.frame(x = 1:3, y = c("a", "b", "c"))

  d <- a_plot(df, a_aes(x, y)) +
    a_geom_point(data = df[0, ]) +
    a_geom_point() +
    a_facet_wrap(~y)

  expect_equal(length(a_layer_grob(d)), 3)
})

test_that("missing layers generate one grob per panel", {
  df <- data.frame(x = 1:4, y = 1:2, g = 1:2)
  base <- a_plot(df, a_aes(x, y)) + a_geom_point(shape = NA, na.rm = TRUE)

  expect_equal(length(a_layer_grob(base)), 1)
  expect_equal(length(a_layer_grob(base + a_facet_wrap(~ g))), 2)
})
