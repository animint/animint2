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
  expect_gt(max_dimension, 450, 
            info="At least one dimension should fill available space")
  
  # Both dimensions should be positive
  expect_gt(svg.width, 0)
  expect_gt(svg.height, 0)
  
  # Verify aspect ratio is maintained (ratio = 1 for coord_equal)
  x.axes <- getNodeSet(info$html, '//svg[@id="plot_plot1"]//g[contains(@class, "xaxis")]')
  y.axes <- getNodeSet(info$html, '//svg[@id="plot_plot1"]//g[contains(@class, "yaxis")]')
  
  expect_equal(length(x.axes), 1)
  expect_equal(length(y.axes), 1)
  
  xdiff <- getTickDiff(x.axes[[1]])
  ydiff <- getTickDiff(y.axes[[1]], axis = "y")
  
  # With ratio = 1 (coord_equal), normalized diffs should be equal
  diffs <- normDiffs(xdiff, ydiff, ratio = 1)
  expect_equal(diffs[1], diffs[2], tolerance = 1, scale = 1)
})
