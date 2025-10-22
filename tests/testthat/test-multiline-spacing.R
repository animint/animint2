acontext("Multiline text spacing and positioning")

library(animint2)

test_that("multiline plot title stays above plot area", {
  skip_if_not_installed("chromote")
  
  data <- data.frame(x = 1:5, y = 1:5)
  
  viz <- list(
    plot1 = ggplot(data, aes(x, y)) +
      geom_point() +
      ggtitle("First Line\nSecond Line\nThird Line")
  )
  
  info <- animint2HTML(viz)
  
  # Get title element bounding box
  title_bbox <- get_element_bbox(".plottitle")
  
  # Get first data point (which should be below the title)
  point_bbox <- get_element_bbox("circle.geom")
  
  # CRITICAL TEST: Title bottom must be ABOVE first data point
  # With multiline titles, the bottom of the title (top + height) 
  # should not overlap with plot area
  title_bottom <- title_bbox$top + title_bbox$height
  
  expect_true(
    title_bottom < point_bbox$top,
    info = sprintf(
      "Title bottom (%.1f) must be above plot area/points (%.1f). Gap: %.1f pixels",
      title_bottom, point_bbox$top, point_bbox$top - title_bottom
    )
  )
})

test_that("multiline axis labels have consistent spacing", {
  skip_if_not_installed("chromote")
  
  data <- data.frame(x = 1:5, y = 1:5)
  
  viz <- list(
    plot1 = ggplot(data, aes(x, y)) +
      geom_point() +
      xlab("X Axis Label\nSubtitle") +
      ylab("Y Axis Label\nSubtitle")
  )
  
  info <- animint2HTML(viz)
  
  # Get X-axis label and ticks
  xtitle_bbox <- get_element_bbox(".xtitle")
  xaxis_bbox <- get_element_bbox(".xaxis")
  
  # Get Y-axis label and ticks  
  ytitle_bbox <- get_element_bbox(".ytitle")
  yaxis_bbox <- get_element_bbox(".yaxis")
  
  # Calculate spacing (distance between axis ticks and labels)
  # For X-axis: vertical distance from axis to label
  x_spacing <- xtitle_bbox$top - (xaxis_bbox$top + xaxis_bbox$height)
  
  # For Y-axis: horizontal distance from label to axis
  y_spacing <- yaxis_bbox$left - (ytitle_bbox$left + ytitle_bbox$width)
  
  # TEST: Spacing should be approximately equal (within 15 pixels tolerance)
  # This accounts for rotation and different orientations
  expect_true(
    abs(x_spacing - y_spacing) < 15,
    info = sprintf(
      "X-axis spacing (%.1f px) and Y-axis spacing (%.1f px) should be consistent. Difference: %.1f px",
      x_spacing, y_spacing, abs(x_spacing - y_spacing)
    )
  )
})

test_that("multiline labels match single-line spacing baseline", {
  skip_if_not_installed("chromote")
  
  # Test 1: Single-line labels
  viz_single <- list(
    plot1 = ggplot(data.frame(x=1:5, y=1:5), aes(x, y)) +
      geom_point() +
      xlab("X Label") +
      ylab("Y Label")
  )
  
  info_single <- animint2HTML(viz_single)
  
  # Test 2: Multi-line labels  
  viz_multi <- list(
    plot1 = ggplot(data.frame(x=1:5, y=1:5), aes(x, y)) +
      geom_point() +
      xlab("X Label\nLine 2") +
      ylab("Y Label\nLine 2")
  )
  
  info_multi <- animint2HTML(viz_multi)
  
  # Get single-line spacing
  xtitle_s <- get_element_bbox(".xtitle")
  xaxis_s <- get_element_bbox(".xaxis")
  single_spacing <- xtitle_s$top - (xaxis_s$top + xaxis_s$height)
  
  # Get multi-line spacing
  xtitle_m <- get_element_bbox(".xtitle")
  xaxis_m <- get_element_bbox(".xaxis")
  multi_spacing <- xtitle_m$top - (xaxis_m$top + xaxis_m$height)
  
  # TEST: Spacing should be similar (within 10 pixels)
  # The base margin should be the same; multiline just extends downward
  expect_true(
    abs(single_spacing - multi_spacing) < 10,
    info = sprintf(
      "Multiline spacing (%.1f) should match single-line baseline (%.1f). Difference: %.1f px",
      multi_spacing, single_spacing, abs(single_spacing - multi_spacing)
    )
  )
})
