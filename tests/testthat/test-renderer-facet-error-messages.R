acontext("Facet error messages")
test_that("facet_wrap formula with missing variable gives clear error", {
  viz <- list(
    scatter = ggplot() +
      facet_wrap(. ~ NonExistentColumn) +
      geom_point(aes(Sepal.Length, Petal.Length), data = iris)
  )
  expect_error(
    animint2dir(viz, out.dir = tempfile(), open.browser = FALSE),
    "Facet variable not found in data: NonExistentColumn\nAvailable columns: Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, Species\nUse string notation like facet_wrap(\"var\") instead of formula notation facet_wrap(. ~ var)",
    fixed = TRUE
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
    "Facet variable not found in data: MissingVar\nAvailable columns: Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, Species\nUse string notation like facet_wrap(\"var\") instead of formula notation facet_wrap(. ~ var)",
    fixed = TRUE
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
