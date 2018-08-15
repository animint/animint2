context("Aes - setting values")

test_that("Aesthetic parameters must match length of data", {
  df <- data.frame(x = 1:5, y = 1:5)
  p <- a_plot(df, a_aes(x, y))

  set_colours <- function(colours) {
    a_layer_data(p + a_geom_point(colour = colours))
  }

  set_colours("red")
  expect_error(set_colours(rep("red", 2)), "must be either length 1")
  expect_error(set_colours(rep("red", 3)), "must be either length 1")
  expect_error(set_colours(rep("red", 4)), "must be either length 1")
  set_colours(rep("red", 5))

})

test_that("alpha affects only fill colour of solid geoms", {
  df <- data.frame(x = 1:2, y = 1)

  poly <- a_plot(df, a_aes(x = x, y)) +
    a_geom_polygon(fill = "red", colour = "red", alpha = 0.5)
  rect <- a_plot(df, a_aes(xmin = x, xmax = x + 1, ymin = 1, ymax = y + 1)) +
    a_geom_rect(fill = "red", colour = "red", alpha = 0.5)
  ribb <- a_plot(df, a_aes(x = x, ymin = 1, ymax = y + 1)) +
    a_geom_ribbon(fill = "red", colour = "red", alpha = 0.5)

  expect_equal(a_layer_grob(poly)[[1]]$gp$col[[1]], "red")
  expect_equal(a_layer_grob(rect)[[1]]$gp$col[[1]], "red")
  expect_equal(a_layer_grob(ribb)[[1]]$children[[1]]$gp$col[[1]], "red")

  expect_equal(a_layer_grob(poly)[[1]]$gp$fill[[1]], "#FF000080")
  expect_equal(a_layer_grob(rect)[[1]]$gp$fill[[1]], "#FF000080")
  expect_equal(a_layer_grob(ribb)[[1]]$children[[1]]$gp$fill[[1]], "#FF000080")
})
