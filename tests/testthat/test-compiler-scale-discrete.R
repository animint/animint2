context("scale_discrete")

# Ranges ------------------------------------------------------------------

test_that("discrete ranges also encompas continuous values", {
  df <- data.frame(x1 = c("a", "b", "c"), x2 = c(0, 2, 4), y = 1:3)

  base <- a_plot(df, a_aes(y = y)) + a_scale_x_discrete()

  x_range <- function(x) {
    a_layer_scales(x)$x$dimension()
  }

  expect_equal(x_range(base + a_geom_point(a_aes(x1))), c(1, 3))
  expect_equal(x_range(base + a_geom_point(a_aes(x2))), c(0, 4))
  expect_equal(x_range(base + a_geom_point(a_aes(x1)) + a_geom_point(a_aes(x2))), c(0, 4))
})

