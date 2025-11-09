test_that("geom_point default shape is 21", {
  expect_equal(geom_point()$geom$default_aes$shape, 21)
})
