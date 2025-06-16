acontext("facet_wrap error handling for missing variables")

test_that("facet_wrap throws informative error when faceting variable is missing", {
# iris dataset without Species column
df <- iris[, c("Sepal.Length", "Petal.Length")]

  p <- ggplot() +
    facet_wrap(. ~ Species) +
    geom_point(aes(Sepal.Length, Petal.Length), data = df)

  expect_error(
    ggplot_build(p),
    "Missing faceting variables: Species. Ensure these exist in the data of at least one layer."
  )
})