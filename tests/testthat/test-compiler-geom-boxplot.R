context("a_geom_boxplot")

test_that("can use US spelling of colour", {
  df <- data.frame(x = 1, y = c(1:5, 100))
  plot <- a_plot(df, a_aes(x, y)) + a_geom_boxplot(outlier.color = "red")

  gpar <- a_layer_grob(plot)[[1]]$children[[1]]$children[[1]]$gp
  expect_equal(gpar$col, "#FF0000FF")
})
