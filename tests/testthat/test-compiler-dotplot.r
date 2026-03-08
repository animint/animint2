context("Dotplot removal")

test_that("geom_dotplot throws error with removal message", {
  dat <- data.frame(x = LETTERS[1:2], y = rnorm(30), g = LETTERS[3:5])
  
  # Test that any call to geom_dotplot produces the expected error
  expect_error(
    ggplot(dat, aes(x, y)) + geom_dotplot(),
    "geom_dotplot.*removed"
  )
  
  expect_error(
    ggplot(dat, aes(x = x, y = y, fill = g)) +
      geom_dotplot(binwidth = 0.2, binaxis = "y", position = "dodge", stackdir = "center"),
    "geom_dotplot.*removed"
  )
  
  expect_error(
    ggplot(dat, aes(y)) + geom_dotplot(binwidth = .4, method = "histodot"),
    "geom_dotplot.*removed"
  )
  
  expect_error(
    ggplot(dat, aes(x = y)) + geom_dotplot(binwidth = .4, method = "dotdensity"),
    "geom_dotplot.*removed"
  )
})