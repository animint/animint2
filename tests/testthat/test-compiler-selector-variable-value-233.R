library(testthat)
library(animint2)

test_that("variable/value selectors filter values by variable", {
  # Reproduce issue #233: when a geom has multiple .variable/.value selectors,
  # each selector should only get values from rows matching that selector's
  # variable value, not from all rows.
  linear_data <- data.frame(
    regularization = "polynomial degree",
    parameter = 0:9,
    mse = runif(10),
    model = "Linear",
    stringsAsFactors = FALSE
  )
  knn_data <- data.frame(
    regularization = "number of neighbors",
    parameter = 1:10,
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
  info <- animint2dir(viz, file.path(tempdir(), "test-issue-233"), open.browser=FALSE)
  poly_selector <- info$selectors[["polynomial degree"]]
  knn_selector <- info$selectors[["number of neighbors"]]
  expect_true(poly_selector$is.variable.value)
  expect_true(knn_selector$is.variable.value)
  poly_selected <- as.numeric(poly_selector$selected)
  knn_selected <- as.numeric(knn_selector$selected)
  expect_true(poly_selected >= 0 && poly_selected <= 9)
  expect_false(poly_selected == 10)
  expect_true(knn_selected >= 1 && knn_selected <= 10)
  expect_false(knn_selected == 0)
})

test_that("selector values extracted from filtered data only", {
  # Test the actual fix location: when extracting unique values for a
  # .variable/.value selector, g.data should be filtered to only rows
  # where the variable column matches the current selector name
  data1 <- data.frame(
    var_col = c("selector_A", "selector_A", "selector_A"),
    val_col = c(1, 2, 3),
    y = runif(3),
    stringsAsFactors = FALSE
  )
  data2 <- data.frame(
    var_col = c("selector_B", "selector_B", "selector_B"),
    val_col = c(4, 5, 6),
    y = runif(3),
    stringsAsFactors = FALSE
  )
  combined <- rbind(data1, data2)
  viz <- list(
    plot1 = ggplot() +
      geom_line(
        aes(x=val_col, y=y),
        data=combined,
        showSelected=c(var_col="val_col")
      )
  )
  info <- animint2dir(viz, file.path(tempdir(), "test-var-val-filter"), open.browser=FALSE)
  selector_A_val <- as.numeric(info$selectors[["selector_A"]]$selected)
  expect_true(selector_A_val %in% c(1,2,3))
  expect_false(selector_A_val %in% c(4,5,6))
  selector_B_val <- as.numeric(info$selectors[["selector_B"]]$selected)
  expect_true(selector_B_val %in% c(4,5,6))
  expect_false(selector_B_val %in% c(1,2,3))
})
