context("Invalid selector character validation")

test_that("selector name with # causes error in clickSelects", {
  viz <- list(
    plot1 = ggplot() +
      geom_point(
        aes(Sepal.Length, Petal.Length),
        data = data.frame(
          Sepal.Length = 1:10,
          Petal.Length = rnorm(10),
          regularization = "# nearest neighbors",
          parameter = 1:10
        ),
        clickSelects = c(regularization = "parameter")
      )
  )
  expect_error(
    animint2dir(viz, open.browser = FALSE),
    "  - '# nearest neighbors' contains '#'",
    fixed = TRUE
  )
})

test_that("selector name with @ causes error in clickSelects", {
  viz <- list(
    plot1 = ggplot() +
      geom_point(
        aes(Sepal.Length, Petal.Length),
        data = data.frame(
          Sepal.Length = 1:10,
          Petal.Length = rnorm(10),
          regularization = "model@version1",
          parameter = 1:10
        ),
        clickSelects = c(regularization = "parameter")
      )
  )
  expect_error(
    animint2dir(viz, open.browser = FALSE),
    "  - 'model@version1' contains '@'",
    fixed = TRUE
  )
})

test_that("selector name with ! causes error", {
  viz <- list(
    plot1 = ggplot() +
      geom_point(
        aes(x=1:10, y=rnorm(10)),
        data = data.frame(
          model = "model!important",
          parameter = 1:10
        ),
        clickSelects = c(model = "parameter")
      )
  )
  expect_error(
    animint2dir(viz, open.browser = FALSE),
    "  - 'model!important' contains '!'",
    fixed = TRUE
  )
})

test_that("valid selector names work with clickSelects", {
  viz <- list(
    plot1 = ggplot() +
      geom_point(
        aes(x=1:10, y=rnorm(10)),
        data = data.frame(
          regularization = "polynomial_degree",
          parameter = 0:9
        ),
        clickSelects = c(regularization = "parameter")
      )
  )
  info <- animint2dir(viz, open.browser = FALSE)
  expect_true("polynomial_degree" %in% names(info$selectors))
})

test_that("selector names with spaces work", {
  viz <- list(
    plot1 = ggplot() +
      geom_point(
        aes(x=1:10, y=rnorm(10)),
        data = data.frame(
          regularization = "nearest neighbors",
          parameter = 1:10
        ),
        clickSelects = c(regularization = "parameter")
      )
  )
  info <- animint2dir(viz, open.browser = FALSE)
  expect_true("nearest neighbors" %in% names(info$selectors))
})

test_that("multiple values with invalid characters all reported", {
  viz <- list(
    plot1 = ggplot() +
      geom_point(
        aes(x=1:10, y=rnorm(10)),
        data = data.frame(
          regularization = rep(c("#bad", "!worse"), 5),
          parameter = 1:10
        ),
        clickSelects = c(regularization = "parameter")
      )
  )
  err <- tryCatch(
    animint2dir(viz, open.browser = FALSE),
    error = function(e) conditionMessage(e)
  )
  expect_true(grepl("  - '#bad' contains '#'", err, fixed = TRUE))
  expect_true(grepl("  - '!worse' contains '!'", err, fixed = TRUE))
})
