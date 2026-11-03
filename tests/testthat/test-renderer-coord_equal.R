acontext("coord_equal renderer tests")

test_that("coord_equal fills available space (issue #234)", {
  # Create the iris petal plot from issue #234
  # This test ensures coord_equal() doesn't shrink the plot unnecessarily
  library(data.table)
  data.mat <- as.matrix(iris[,c("Petal.Width","Petal.Length")])
  
  # Create the visualization with a specific size
  viz <- animint(
    plot1=ggplot(data=data.table(data.mat), aes(Petal.Length, Petal.Width))+
      geom_point()+
      coord_equal()+
      theme_animint(width=500, height=500)
  )
  
  # Render the plot using animint2HTML (not animint2dir)
  info <- animint2HTML(viz)
  
  # Extract SVG plotting area dimensions
  # Get the SVG element to check overall plot dimensions
  svg.nodes <- getNodeSet(info$html, '//svg[@id="plot_plot1"]')
  expect_equal(length(svg.nodes), 1)
  svg.attrs <- xmlAttrs(svg.nodes[[1]])
  
  # Extract width and height attributes
  svg.width <- as.numeric(svg.attrs[["width"]])
  svg.height <- as.numeric(svg.attrs[["height"]])
  
  # The key test: with coord_equal(), at least one dimension should 
  # fill the available space (close to 500px as specified in theme_animint)
  # On the old buggy code (before the fix in commit e4b9634b), BOTH dimensions
  # would be unnecessarily shrunk below the available space.
  # With the fix, at least one dimension uses nearly all available space.
  
  # The iris petal data has aspect ratio of ~0.417 (y_range=2.5, x_range=6)
  # So with coord_equal (ratio=1), the x-axis fills space (width≈500)
  # and y-axis is constrained by aspect ratio (height≈200)
  
  # Test that at least ONE dimension is close to requested size (500)
  max_dimension <- max(svg.width, svg.height)
  expect_gt(max_dimension, 450)
  
  # Both dimensions should be positive
  expect_gt(svg.width, 0)
  expect_gt(svg.height, 0)
  # Issue #234 acceptance: fill available space. Aspect-ratio math is covered by
  # test-compiler-coord-equal-fix.R and test-renderer1-coord.R (normDiffs).
})
