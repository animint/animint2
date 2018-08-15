context("scale_manual")


test_that("names of values used in manual scales", {
   s <- a_scale_colour_manual(values = c("8" = "c","4" = "a","6" = "b"))
   s$train(c("4", "6", "8"))
   expect_equal(s$map(c("4", "6", "8")), c("a", "b", "c"))
})


dat <- data.frame(g = c("B","A","A"))
p <- a_plot(dat, a_aes(g, fill = g)) + a_geom_bar()
col <- c("A" = "red", "B" = "green", "C" = "blue")

cols <- function(x) a_plot_build(x)$data[[1]][, "fill"]

test_that("named values work regardless of order", {
  fill_scale <- function(order) a_scale_fill_manual(values = col[order],
    na.value = "black")

  # Order of value vector shouldn't matter
  expect_equal(cols(p + fill_scale(1:3)), c("red", "green"))
  expect_equal(cols(p + fill_scale(1:2)), c("red", "green"))
  expect_equal(cols(p + fill_scale(2:1)), c("red", "green"))
  expect_equal(cols(p + fill_scale(c(3, 2, 1))), c("red", "green"))
  expect_equal(cols(p + fill_scale(c(3, 1, 2))), c("red", "green"))
  expect_equal(cols(p + fill_scale(c(1, 3, 2))), c("red", "green"))
})

test_that("missing values replaced with na.value", {
  df <- data.frame(x = 1, y = 1:3, z = factor(c(1:2, NA), exclude = NULL))
  p <- a_plot(df, a_aes(x, y, colour = z)) +
    a_geom_point() + 
    a_scale_colour_manual(values = c("black", "black"), na.value = "red")

  expect_equal(a_layer_data(p)$colour, c("black", "black", "red"))
})

test_that("insufficient values raise an error", {
  df <- data.frame(x = 1, y = 1:3, z = factor(c(1:2, NA), exclude = NULL))
  p <- qplot(x, y, data = df, colour = z)

  expect_error(a_plot_build(p + a_scale_colour_manual(values = "black")),
    "Insufficient values")

  # Should be sufficient
  a_plot_build(p + a_scale_colour_manual(values = c("black", "black")))

})

test_that("values are matched when scale contains more unique valuesthan are in the data", {
  s <- a_scale_colour_manual(values = c("8" = "c", "4" = "a",
    "22" = "d", "6"  = "b"))
  s$train(c("4", "6", "8"))
  expect_equal(s$map(c("4", "6", "8")), c("a", "b", "c"))
})

