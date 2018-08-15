context("a_geom_ribbon")

test_that("NAs are not dropped from the data", {
  df <- data.frame(x = 1:5, y = c(1, 1, NA, 1, 1))

  p <- a_plot(df, a_aes(x))+
    a_geom_ribbon(a_aes(ymin = y - 1, ymax = y + 1))

  expect_equal(a_layer_data(p)$ymin, c(0, 0, NA, 0, 0))
})
