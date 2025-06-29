acontext("geom_raster not defined")

test_that("geom_raster is not defined", {
  expect_error(
    geom_raster(),
    'could not find function "geom_raster"'
  )
})
