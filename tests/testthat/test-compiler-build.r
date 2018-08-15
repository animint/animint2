# Test the complete path from plot specification to rendered data
context("Plot building")

df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])

test_that("there is one data frame for each layer", {
  nlayers <- function(x) length(a_plot_build(x)$data)

  l1 <- a_plot(df, a_aes(x, y)) + a_geom_point()
  l2 <- a_plot(df, a_aes(x, y)) + a_geom_point() + a_geom_line()
  l3 <- a_plot(df, a_aes(x, y)) + a_geom_point() + a_geom_line() + a_geom_point()

  expect_equal(nlayers(l1), 1)
  expect_equal(nlayers(l2), 2)
  expect_equal(nlayers(l3), 3)
})

test_that("position aesthetics coerced to correct type", {
  l1 <- a_plot(df, a_aes(x, y)) + a_geom_point()
  d1 <- a_layer_data(l1, 1)

  expect_is(d1$x, "numeric")
  expect_is(d1$y, "numeric")

  l2 <- a_plot(df, a_aes(x, z)) + a_geom_point() + a_scale_x_discrete()
  d2 <- a_layer_data(l2, 1)

  expect_is(d2$x, "integer")
  expect_is(d2$y, "integer")
})

test_that("non-position aesthetics are mapped", {
  l1 <- a_plot(df, a_aes(x, y, fill = z, colour = z, shape = z, size = z)) +
    a_geom_point()
  d1 <- a_layer_data(l1, 1)

  expect_equal(sort(names(d1)), sort(c("x", "y", "fill", "group",
    "colour", "shape", "size", "PANEL", "alpha", "stroke")))

  l2 <- l1 + a_scale_colour_manual(values = c("blue", "red", "yellow"))
  d2 <- a_layer_data(l2, 1)
  expect_equal(d2$colour, c("blue", "red", "yellow"))
})

test_that("strings are not converted to factors", {
  df <- data.frame(x = 1:2, y = 2:1, a_label = c("alpha", "beta"), stringsAsFactors = FALSE)
  p <- a_plot(df, a_aes(x, y)) +
    a_geom_text(a_aes(a_label = a_label), parse = TRUE)

  expect_is(a_layer_data(p)$a_label, "character")
})
