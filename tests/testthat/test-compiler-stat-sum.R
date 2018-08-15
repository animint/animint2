context("stat_sum")

test_that("handles grouping correctly", {
  d <- diamonds[1:1000, ]
  all_ones <- function(x) all.equal(mean(x), 1)

  base <- a_plot(d, a_aes(cut, clarity))

  ret <- a_layer_data(base + a_stat_sum())
  expect_equal(nrow(ret), 38)
  expect_equal(sum(ret$n), nrow(d))
  expect_true(all_ones(ret$prop))

  ret <- a_layer_data(base + a_stat_sum(a_aes(group = 1)))
  expect_equal(nrow(ret), 38)
  expect_equal(sum(ret$n), nrow(d))
  expect_equal(sum(ret$prop), 1)

  ret <- a_layer_data(base + a_stat_sum(a_aes(group = cut)))
  expect_equal(nrow(ret), 38)
  expect_equal(sum(ret$n), nrow(d))
  expect_true(all_ones(tapply(ret$prop, ret$x, FUN = sum)))

  ret <- a_layer_data(base + a_stat_sum(a_aes(group = cut, colour = cut)))
  expect_equal(nrow(ret), 38)
  expect_equal(sum(ret$n), nrow(d))
  expect_true(all_ones(tapply(ret$prop, ret$x, FUN = sum)))

  ret <- a_layer_data(base + a_stat_sum(a_aes(group = clarity)))
  expect_equal(nrow(ret), 38)
  expect_equal(sum(ret$n), nrow(d))
  expect_true(all_ones(tapply(ret$prop, ret$y, FUN = sum)))

  ret <- a_layer_data(base + a_stat_sum(a_aes(group = clarity, colour = cut)))
  expect_equal(nrow(ret), 38)
  expect_equal(sum(ret$n), nrow(d))
  expect_true(all_ones(tapply(ret$prop, ret$y, FUN = sum)))

  ret <- a_layer_data(base + a_stat_sum(a_aes(group = 1, weight = price)))
  expect_equal(nrow(ret), 38)
  expect_equal(sum(ret$n), sum(d$price))
  expect_equal(sum(ret$prop), 1)
})
