library(testthat)
library(animint2)

test_that("variable/value selectors filter values by variable", {
  # Reproduce issue #233: when a geom has multiple .variable/.value selectors,
  # each selector should only get values from rows matching that selector's
  # variable value, not from all rows.
  degree_expected <- 0:9
  linear_data <- data.frame(
    regularization = "polynomial degree",
    parameter = degree_expected,
    mse = runif(10),
    model = "Linear",
    stringsAsFactors = FALSE
  )
  neighbor_expected <- 1:10
  knn_data <- data.frame(
    regularization = "number of neighbors",
    parameter = neighbor_expected,
    mse = runif(10),
    model = "KNN",
    stringsAsFactors = FALSE
  )
  combined_data <- rbind(linear_data, knn_data)
  viz <- list(
    plot1 = ggplot() +
      geom_point(
        aes(x=parameter, y=mse, color=model),
        data=combined_data,
        showSelected=c(regularization="parameter"),
        size=5
      )
  )
  info <- animint2dir(viz, open.browser=FALSE)
  degree_values <- info$selector.values[["polynomial degree"]][[1]]$values
  expect_identical(degree_values, paste(degree_expected))
  neighbor_values <- info$selector.values[["number of neighbors"]][[1]]$values
  expect_identical(neighbor_values, paste(neighbor_expected))
})
