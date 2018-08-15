context("qplot")

test_that("qplot works with variables in data frame and parent env", {
  df <- data.frame(x = 1:10, a = 1:10)
  y <- 1:10
  b <- 1:10

  expect_is(qplot(x, y, data = df), "a_plot")
  expect_is(qplot(x, y, data = df, colour = a), "a_plot")
  expect_is(qplot(x, y, data = df, colour = b), "a_plot")

  bin <- 1
  expect_is(qplot(x, data = df, binwidth = bin), "a_plot")
})

test_that("qplot works in non-standard environments", {
  env <- new.env(parent = globalenv())
  expr <- quote({
    `-1-` <- 10
    x <- 1:10
    qplot(x, breaks = 0:`-1-`)
  })

  expect_is(eval(expr, env), "a_plot")

})

