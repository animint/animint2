acontext("geom_raster error handling")

test_that("geom_raster throws an informative error", {
  expect_error(
    ggplot(data.frame(x = 1, y = 1), aes(x, y)) + geom_raster(),
    "geom_raster is not supported in animint2. Please use geom_tile instead."
  )
})