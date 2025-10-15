acontext("coord_equal fix for issue #234")

test_that("fixed_spaces normalizes to fill available space", {
  # Test with iris petal data ranges (issue #234 example)
  ranges <- list(list(x.range = c(1, 7), y.range = c(0, 2.5)))
  spaces <- animint2:::fixed_spaces(ranges, ratio = 1)
  
  # At least one dimension should be 1 (fills available space)
  max_prop <- max(spaces$x, spaces$y)
  expect_equal(max_prop, 1)
  
  # Both proportions should be positive and <= 1
  expect_gt(spaces$x, 0)
  expect_lte(spaces$x, 1)
  expect_gt(spaces$y, 0)
  expect_lte(spaces$y, 1)
})
