context("freqpoly")

test_that("can do frequency polygon with categorical x", {
  df <- data.frame(x = rep(letters[1:3], 3:1))

  p <- a_plot(df, a_aes(x)) + a_geom_freqpoly(a_stat = "count")
  d <- a_layer_data(p)

  expect_is(d$x, "integer")
  expect_equal(d$x, 1:3)
  expect_equal(d$y, 3:1)
})
