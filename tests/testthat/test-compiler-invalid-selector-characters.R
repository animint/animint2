context("Invalid selector character validation")

# Test that selector names with special characters cause errors
# Note: Selector names come from the VALUES in the data when using .variable/.value pattern
test_that("selector name with # causes error in clickSelects", {
  viz <- list(
    plot1 = ggplot() +
      geom_point(
        aes(Sepal.Length, Petal.Length),
        data = data.frame(
          Sepal.Length = 1:10,
          Petal.Length = rnorm(10),
          regularization = "# nearest neighbors",  # This VALUE becomes the selector name
          parameter = 1:10
        ),
        clickSelects = c(regularization = "parameter")
      )
  )
  expect_error(
    animint2dir(viz, open.browser = FALSE),
    "Invalid character(s) in selector name(s).
Selector names cannot contain special characters that interfere with CSS selectors.
The following selector(s) contain invalid characters:
- # nearest neighbors

Please remove or replace these characters in your variable names.",
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
          regularization = "model@version1",  # This VALUE becomes selector name
          parameter = 1:10
        ),
        clickSelects = c(regularization = "parameter")
      )
  )
  expect_error(
    animint2dir(viz, open.browser = FALSE),
    "Invalid character(s) in selector name(s).
Selector names cannot contain special characters that interfere with CSS selectors.
The following selector(s) contain invalid characters:
- model@version1

Please remove or replace these characters in your variable names.",
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
    "Invalid character(s) in selector name(s).
Selector names cannot contain special characters that interfere with CSS selectors.
The following selector(s) contain invalid characters:
- model!important

Please remove or replace these characters in your variable names.",
    fixed = TRUE
  )
})

test_that("valid selector names work with clickSelects", {
  viz <- list(
    plot1 = ggplot() +
      geom_point(
        aes(x=1:10, y=rnorm(10)),
        data = data.frame(
          regularization = "polynomial_degree",  # Valid name
          parameter = 0:9
        ),
        clickSelects = c(regularization = "parameter")
      )
  )
  info <- animint2dir(viz, open.browser = FALSE)
  expect_true(TRUE, info = "Valid selector names should not cause errors")
})

test_that("selector names with spaces work", {
  viz <- list(
    plot1 = ggplot() +
      geom_point(
        aes(x=1:10, y=rnorm(10)),
        data = data.frame(
          regularization = "nearest neighbors",  # spaces are OK
          parameter = 1:10
        ),
        clickSelects = c(regularization = "parameter")
      )
  )
  info <- animint2dir(viz, open.browser = FALSE)
  expect_true(TRUE, info = "Selector names with spaces should work")
})

test_that("multiple values with invalid characters all reported", {
  viz <- list(
    plot1 = ggplot() +
      geom_point(
        aes(x=1:10, y=rnorm(10)),
        data = data.frame(
          regularization = rep(c("#bad", "!worse"), 5),  # Both have invalid chars
          parameter = 1:10
        ),
        clickSelects = c(regularization = "parameter")
      )
  )
  expect_error(
    animint2dir(viz, open.browser = FALSE),
    "Invalid character(s) in selector name(s).\nSelector names cannot contain special characters that interfere with CSS selectors.\nThe following selector(s) contain invalid characters:\n- #bad\n- !worse\n\nPlease remove or replace these characters in your variable names.",
    fixed = TRUE
  )
})
