context("a_stat_function")

test_that("uses scale limits, not data limits", {
  dat <- data.frame(x = c(0.1, 1:100))
  dat$y <- dexp(dat$x)

  base <- a_plot(dat, a_aes(x, y)) +
    a_stat_function(fun = dexp)

  full <- base +
    a_scale_x_continuous(limits = c(0.1, 100)) +
    a_scale_y_continuous()
  ret <- a_layer_data(full)

  full_log <- base +
    a_scale_x_log10(limits = c(0.1, 100)) +
    a_scale_y_continuous()
  ret_log <- a_layer_data(full_log)

  expect_equal(ret$y[c(1, 101)], ret_log$y[c(1, 101)])
  expect_equal(range(ret$x), c(0.1, 100))
  expect_equal(range(ret_log$x), c(-1, 2))
  expect_false(any(is.na(ret$y)))
  expect_false(any(is.na(ret_log$y)))
})

test_that("works with discrete x", {
  dat <- data.frame(x = c("a", "b"))

  base <- a_plot(dat, a_aes(x, group = 1)) +
    a_stat_function(fun = as.numeric, a_geom = "point", n = 2)
  ret <- a_layer_data(base)

  expect_equal(ret$x, 1:2)
  expect_equal(ret$y, 1:2)
})
