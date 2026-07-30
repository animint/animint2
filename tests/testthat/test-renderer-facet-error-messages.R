acontext("Facet error messages")
missing_var_error <- paste(
  "Facet variable not found in data: NonExistentColumn",
  "Available columns: Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, Species",
  sep = "\n"
)
missing_var_string_error <- paste(
  "Facet variable not found in data: MissingVar",
  "Available columns: Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, Species",
  sep = "\n"
)
bad_notation_error <- "Invalid facet_wrap formula (. ~ var). Use facet_wrap(~Species) or facet_wrap(\"Species\")"
test_that("facet_wrap string missing variable gives clear error", {
  viz <- list(
    scatter = ggplot() +
      facet_wrap("MissingVar") +
      geom_point(aes(Sepal.Length, Petal.Length), data = iris)
  )
  expect_error({
    animint2dir(viz)
  }, missing_var_string_error, fixed = TRUE)
})
test_that("facet_wrap formula with missing variable gives clear error", {
  viz <- list(
    scatter = ggplot() +
      facet_wrap(. ~ NonExistentColumn) +
      geom_point(aes(Sepal.Length, Petal.Length), data = iris)
  )
  expect_error({
    animint2dir(viz)
  }, missing_var_error, fixed = TRUE)
})
test_that("facet_wrap bad notation when variable exists gives clear error", {
  viz <- list(
    scatter = ggplot() +
      facet_wrap(. ~ Species) +
      geom_point(aes(Sepal.Length, Petal.Length), data = iris)
  )
  expect_error({
    animint2dir(viz)
  }, bad_notation_error, fixed = TRUE)
})
test_that("facet_grid missing variable gives clear error", {
  viz <- list(
    scatter = ggplot() +
      facet_grid(. ~ MissingVar) +
      geom_point(aes(Sepal.Length, Petal.Length), data = iris)
  )
  expect_error({
    animint2dir(viz)
  }, missing_var_string_error, fixed = TRUE)
})
test_that("facet_wrap tilde notation works", {
  viz <- list(
    scatter = ggplot() +
      facet_wrap(~Species) +
      geom_point(aes(Sepal.Length, Petal.Length), data = iris)
  )
  info <- animint2dir(viz, open.browser = FALSE)
  expect_true(file.exists(file.path(info$out.dir, "index.html")))
})
test_that("facet_wrap string notation works", {
  viz <- list(
    scatter = ggplot() +
      facet_wrap("Species") +
      geom_point(aes(Sepal.Length, Petal.Length), data = iris)
  )
  info <- animint2dir(viz, open.browser = FALSE)
  expect_true(file.exists(file.path(info$out.dir, "index.html")))
})
test_that("facet_grid formula notation works", {
  viz <- list(
    scatter = ggplot() +
      facet_grid(. ~ Species) +
      geom_point(aes(Sepal.Length, Petal.Length), data = iris)
  )
  info <- animint2dir(viz, open.browser = FALSE)
  expect_true(file.exists(file.path(info$out.dir, "index.html")))
})
