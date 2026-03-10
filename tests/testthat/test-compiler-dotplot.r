context("Dotplot removal")

test_that("geom_dotplot throws error with removal message", {
  dat <- data.frame(x = LETTERS[1:2], y = rnorm(30), g = LETTERS[3:5])
  
  # Test that any call to geom_dotplot produces the expected error
  expect_error(
    ggplot(dat, aes(x, y)) + geom_dotplot(),
    "geom_dotplot() has been removed from animint2. Use geom_point() instead. See issue #289 for details: https://github.com/animint/animint2/issues/289",
    fixed = TRUE
  )
})
