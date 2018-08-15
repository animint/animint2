context("a_Scales")

test_that("buidling a plot does not affect its scales", {
  dat <- data.frame(x = rnorm(20), y = rnorm(20))

  p <- a_plot(dat, a_aes(x, y)) + a_geom_point()
  expect_equal(length(p$scales$scales), 0)

  a_plot_build(p)
  expect_equal(length(p$scales$scales), 0)
})

test_that("ranges update only for variables listed in a_aesthetics", {
  sc <- a_scale_alpha()

  sc$train_df(data.frame(alpha = 1:10))
  expect_equal(sc$range$range, c(1, 10))

  sc$train_df(data.frame(alpha = 50))
  expect_equal(sc$range$range, c(1, 50))

  sc$train_df(data.frame(beta = 100))
  expect_equal(sc$range$range, c(1, 50))

  sc$train_df(data.frame())
  expect_equal(sc$range$range, c(1, 50))

})

test_that("mapping works", {
  sc <- a_scale_alpha(range = c(0, 1), na.value = 0)
  sc$train_df(data.frame(alpha = 1:10))

  expect_equal(
    sc$map_df(data.frame(alpha = 1:10))[[1]],
    seq(0, 1, length.out = 10)
  )

  expect_equal(sc$map_df(data.frame(alpha = NA))[[1]], 0)

  expect_equal(
    sc$map_df(data.frame(alpha = c(-10, 11)))[[1]],
    c(0, 0))
})

test_that("identity scale preserves input values", {
  df <- data.frame(x = 1:3, z = letters[1:3])

  p1 <- a_plot(df,
    a_aes(x, z, colour = z, fill = z, shape = z, size = x, alpha = x)) +
    a_geom_point() +
    a_scale_colour_identity() +
    a_scale_fill_identity() +
    a_scale_shape_identity() +
    a_scale_size_identity() +
    a_scale_alpha_identity()
  d1 <- a_layer_data(p1)

  expect_equal(d1$colour, as.character(df$z))
  expect_equal(d1$fill, as.character(df$z))
  expect_equal(d1$shape, as.character(df$z))
  expect_equal(d1$size, as.numeric(df$z))
  expect_equal(d1$alpha, as.numeric(df$z))
})

test_that("position scales updated by all position a_aesthetics", {
  df <- data.frame(x = 1:3, y = 1:3)

  a_aesthetics <- list(
    a_aes(xend = x, yend = x),
    a_aes(xmin = x, ymin = x),
    a_aes(xmax = x, ymax = x),
    a_aes(xintercept = x, yintercept = y)
  )

  base <- a_plot(df, a_aes(x = 1, y = 1)) + a_geom_point()
  plots <- lapply(a_aesthetics, function(x) base %+% x)
  ranges <- lapply(plots, pranges)

  lapply(ranges, function(range) {
    expect_equal(range$x[[1]], c(1, 3))
    expect_equal(range$y[[1]], c(1, 3))
  })

})

test_that("position scales generate after stats", {
  df <- data.frame(x = factor(c(1, 1, 1)))
  plot <- a_plot(df, a_aes(x)) + a_geom_bar()
  ranges <- pranges(plot)

  expect_equal(ranges$x[[1]], c("1"))
  expect_equal(ranges$y[[1]], c(0, 3))

})

test_that("oob affects position values", {
  dat <- data.frame(x = c("a", "b", "c"), y = c(1, 5, 10))
  base <- a_plot(dat, a_aes(x, y)) +
    a_geom_bar(a_stat = "identity") +
    a_annotate("point", x = "a", y = c(-Inf, Inf))

  y_scale <- function(limits, oob = censor) {
    a_scale_y_continuous(limits = limits, oob = oob, expand = c(0, 0))
  }
  base + a_scale_y_continuous(limits = c(-0,5))

  expect_warning(low_censor <- cdata(base + y_scale(c(0, 5), censor)),
    "Removed 1 rows containing missing values")
  expect_warning(mid_censor <- cdata(base + y_scale(c(3, 7), censor)),
    "Removed 2 rows containing missing values")

  low_squish <- cdata(base + y_scale(c(0, 5), squish))
  mid_squish <- cdata(base + y_scale(c(3, 7), squish))

  # Points are always at the top and bottom
  expect_equal(low_censor[[2]]$y, c(0, 1))
  expect_equal(mid_censor[[2]]$y, c(0, 1))
  expect_equal(low_squish[[2]]$y, c(0, 1))
  expect_equal(mid_squish[[2]]$y, c(0, 1))

  # Bars depend on limits and oob
  expect_equal(low_censor[[1]]$y, c(0.2, 1))
  expect_equal(mid_censor[[1]]$y, c(0.5))
  expect_equal(low_squish[[1]]$y, c(0.2, 1, 1))
  expect_equal(mid_squish[[1]]$y, c(0, 0.5, 1))


})

test_that("scales looked for in appropriate place", {
  xlabel <- function(x) a_plot_build(x)$panel$x_scales[[1]]$name
  p0 <- qplot(mpg, wt, data = mtcars) + a_scale_x_continuous("0")
  expect_equal(xlabel(p0), "0")

  a_scale_x_continuous <- function(...) animint2::a_scale_x_continuous("1")
  p1 <- qplot(mpg, wt, data = mtcars)
  expect_equal(xlabel(p1), "1")

  f <- function() {
    a_scale_x_continuous <- function(...) animint2::a_scale_x_continuous("2")
    qplot(mpg, wt, data = mtcars)
  }
  p2 <- f()
  expect_equal(xlabel(p2), "2")

  rm(a_scale_x_continuous)
  p4 <- qplot(mpg, wt, data = mtcars)
  expect_equal(xlabel(p4), waiver())
})

test_that("find_global searches in the right places", {
  testenv <- new.env(parent = globalenv())

  # This should find the scale object in the package environment
  expect_identical(find_global("a_scale_colour_hue", testenv),
    animint2::a_scale_colour_hue)

  # Set an object with the same name in the environment
  testenv$a_scale_colour_hue <- "foo"

  # Now it should return the new object
  expect_identical(find_global("a_scale_colour_hue", testenv), "foo")

  # If we search in the empty env, we should end up with the object
  # from the ggplot2 namespace
  expect_identical(find_global("a_scale_colour_hue", emptyenv()),
    animint2::a_scale_colour_hue)
})
