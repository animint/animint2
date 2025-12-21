acontext("Facet error messages")
test_that("facet_wrap formula with missing variable gives clear error", {
  viz <- list(
    scatter = ggplot() +
      facet_wrap(. ~ NonExistentColumn) +
      geom_point(aes(Sepal.Length, Petal.Length), data = iris)
  )
  expect_error(
    animint2dir(viz, out.dir = tempfile(), open.browser = FALSE),
    "NonExistentColumn"
  )
})
test_that("facet_grid formula with missing variable gives clear error", {
  viz <- list(
    scatter = ggplot() +
      facet_grid(. ~ MissingVar) +
      geom_point(aes(Sepal.Length, Petal.Length), data = iris)
  )
  expect_error(
    animint2dir(viz, out.dir = tempfile(), open.browser = FALSE),
    "MissingVar"
  )
})
test_that("facet_wrap string notation works", {
  viz <- list(
    scatter = ggplot() +
      facet_wrap("Species") +
      geom_point(aes(Sepal.Length, Petal.Length), data = iris)
  )
  info <- animint2dir(viz, out.dir = tempfile(), open.browser = FALSE)
  expect_true(file.exists(file.path(info$out.dir, "index.html")))
})
