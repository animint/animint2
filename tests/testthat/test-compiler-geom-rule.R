context("a_geom_rule")
# tests for a_geom_vline, a_geom_hline & a_geom_abline

df <- data.frame(x = 1:3, y = 3:1)
p <- a_plot(df, a_aes(x, y)) + a_geom_point()
p_col <- a_plot(df, a_aes(x, y, colour = factor(x))) + a_geom_point()

test_that("setting parameters makes one row df", {
  b <- p + a_geom_hline(yintercept = 1.5)
  expect_equal(a_layer_data(b, 2)$yintercept, 1.5)

  b <- p + a_geom_vline(xintercept = 1.5)
  expect_equal(a_layer_data(b, 2)$xintercept, 1.5)

  b <- p + a_geom_abline()
  expect_equal(a_layer_data(b, 2)$intercept, 0)
  expect_equal(a_layer_data(b, 2)$slope, 1)

  b <- p + a_geom_abline(slope = 0, intercept = 1)
  expect_equal(a_layer_data(b, 2)$intercept, 1)
  expect_equal(a_layer_data(b, 2)$slope, 0)
})

test_that("setting aesthetics generates one row for each input row", {
  b <- p + a_geom_hline(a_aes(yintercept = 1.5))
  expect_equal(a_layer_data(b, 2)$yintercept, rep(1.5, 3))

  b <- p + a_geom_vline(a_aes(xintercept = 1.5))
  expect_equal(a_layer_data(b, 2)$xintercept, rep(1.5, 3))

  b <- p + a_geom_abline(a_aes(slope = 0, intercept = 1))
  expect_equal(a_layer_data(b, 2)$intercept, rep(1, 3))
  expect_equal(a_layer_data(b, 2)$slope, rep(0, 3))
})
