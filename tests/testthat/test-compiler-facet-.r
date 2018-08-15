context("Facetting")

df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])

test_that("facets split up the data", {
  l1 <- a_plot(df, a_aes(x, y)) + a_geom_point() + a_facet_wrap(~z)
  l2 <- a_plot(df, a_aes(x, y)) + a_geom_point() + a_facet_grid(. ~ z)
  l3 <- a_plot(df, a_aes(x, y)) + a_geom_point() + a_facet_grid(z ~ .)

  d1 <- a_layer_data(l1)
  d2 <- a_layer_data(l2)
  d3 <- a_layer_data(l3)

  expect_equal(d1, d2)
  expect_equal(d1, d3)
  expect_equal(d1$PANEL, factor(1:3))
})

test_that("facets with free scales scale independently", {
  l1 <- a_plot(df, a_aes(x, y)) + a_geom_point() +
    a_facet_wrap(~z, scales = "free")
  d1 <- cdata(l1)[[1]]
  expect_true(sd(d1$x) < 1e-10)
  expect_true(sd(d1$y) < 1e-10)

  l2 <- a_plot(df, a_aes(x, y)) + a_geom_point() +
    a_facet_grid(. ~ z, scales = "free")
  d2 <- cdata(l2)[[1]]
  expect_true(sd(d2$x) < 1e-10)
  expect_equal(length(unique(d2$y)), 3)

  l3 <- a_plot(df, a_aes(x, y)) + a_geom_point() +
    a_facet_grid(z ~ ., scales = "free")
  d3 <- cdata(l3)[[1]]
  expect_equal(length(unique(d3$x)), 3)
  expect_true(sd(d3$y) < 1e-10)
})


test_that("shrink parameter affects scaling", {
  l1 <- a_plot(df, a_aes(1, y)) + a_geom_point()
  r1 <- pranges(l1)

  expect_equal(r1$x[[1]], c(1, 1))
  expect_equal(r1$y[[1]], c(1, 3))

  l2 <- a_plot(df, a_aes(1, y)) + a_stat_summary(fun.y = "mean")
  r2 <- pranges(l2)
  expect_equal(r2$y[[1]], c(2, 2))

  l3 <- a_plot(df, a_aes(1, y)) + a_stat_summary(fun.y = "mean") +
    a_facet_null(shrink = FALSE)
  r3 <- pranges(l3)
  expect_equal(r3$y[[1]], c(1, 3))
})
