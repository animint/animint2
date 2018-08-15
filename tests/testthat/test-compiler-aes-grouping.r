context("Aesthetics (grouping)")

df <- data.frame(
  x = 1:4,
  a = c("a", "a", "b", "b"),
  b = c("a", "b", "a", "b")
)

group <- function(x) as.vector(a_layer_data(x, 1)$group)
groups <- function(x) length(unique(group(x)))

test_that("one group per combination of discrete vars", {
  plot <- a_plot(df, a_aes(x, x)) + a_geom_point()
  expect_equal(group(plot), rep(NO_GROUP, 4))

  plot <- a_plot(df, a_aes(x, a)) + a_geom_point()
  expect_equal(group(plot), c(1, 1, 2, 2))
  plot <- a_plot(df, a_aes(x, b)) + a_geom_point()
  expect_equal(group(plot), c(1, 2, 1, 2))

  plot <- a_plot(df, a_aes(a, b)) + a_geom_point()
  expect_equal(groups(plot), 4)
})

test_that("a_label is not used as a grouping var", {
  plot <- a_plot(df, a_aes(x, x, a_label = a)) + a_geom_point()
  expect_equal(group(plot), rep(NO_GROUP, 4))

  plot <- a_plot(df, a_aes(x, x, colour = a, a_label = b)) + a_geom_point()
  expect_equal(group(plot), c(1, 1, 2, 2))
})

test_that("group a_aesthetic overrides defaults", {
  plot <- a_plot(df, a_aes(x, x, group = x)) + a_geom_point()
  expect_equal(groups(plot), 4)

  plot <- a_plot(df, a_aes(a, b, group = 1)) + a_geom_point()
  expect_equal(groups(plot), 1)
})

test_that("group param overrides defaults", {
  plot <- a_plot(df, a_aes(a, b)) + a_geom_point(group = 1)
  expect_equal(groups(plot), 1)
})
