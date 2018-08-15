context("a_annotate")

test_that("dates in segment annotation work", {
  dt <- structure(list(month = structure(c(1364774400, 1377993600),
      class = c("POSIXct", "POSIXt"), tzone = "UTC"), total = c(-10.3,
      11.7)), .Names = c("month", "total"), row.names = c(NA, -2L), class =
      "data.frame")

  p <- a_plot(dt, a_aes(month, total)) +
    a_geom_point() +
    a_annotate("segment",
      x = as.POSIXct("2013-04-01"),
      xend = as.POSIXct("2013-07-01"),
      y = -10,
      yend = 10
    )

  expect_true(all(c("xend", "yend") %in% names(a_layer_data(p, 2))))
})

test_that("segment annotations transform with scales", {
  # This should be a visual test, but contriubtion documentation does not
  # explain how to make one
  a_plot(mtcars, a_aes(wt, mpg)) +
    a_geom_point() +
    a_annotate("segment", x = 2, y = 10, xend = 5, yend = 30, colour = "red") +
    a_scale_y_reverse()
})
