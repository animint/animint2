context("annotate")

test_that("dates in segment annotation work", {
  dt <- structure(list(month = structure(c(1364774400, 1377993600),
      class = c("POSIXct", "POSIXt"), tzone = "UTC"), total = c(-10.3,
      11.7)), .Names = c("month", "total"), row.names = c(NA, -2L), class =
      "data.frame")

  p <- ggplot(dt, aes(month, total)) +
    geom_point() +
    annotate("segment",
      x = as.POSIXct("2013-04-01"),
      xend = as.POSIXct("2013-07-01"),
      y = -10,
      yend = 10
    )

  expect_true(all(c("xend", "yend") %in% names(layer_data(p, 2))))
})
