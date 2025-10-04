acontext("coord_equal fix for issue #234")

test_that("fixed_spaces normalizes to fill available space", {
  # Test with iris petal data ranges (issue #234 example)
  ranges <- list(list(x.range = c(1, 7), y.range = c(0, 2.5)))
  spaces <- animint2:::fixed_spaces(ranges, ratio = 1)
  
  # At least one dimension should be 1 (fills available space)
  max_prop <- max(spaces$x, spaces$y)
  expect_equal(max_prop, 1)
  
  # Both proportions should be positive and <= 1
  expect_true(all(spaces$x > 0 & spaces$x <= 1))
  expect_true(all(spaces$y > 0 & spaces$y <= 1))
})

test_that("coord_equal compiles and creates proper layout", {
  library(data.table)
  data.mat <- as.matrix(iris[,c("Petal.Width","Petal.Length")])
  
  # Create the visualization from issue #234
  viz <- animint(
    plot1=ggplot(data=data.table(data.mat), aes(Petal.Length, Petal.Width))+
      geom_point()+
      coord_equal()+
      theme_animint(width=800)
  )
  
  # Verify it compiles without error
  info <- animint2dir(viz, open.browser = FALSE)
  expect_true(file.exists(file.path(info$out.dir, "plot.json")))
})