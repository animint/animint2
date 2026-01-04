context("Issue #279: facet_wrap spacing with custom height")

test_that("facet_wrap vertical layout height scales proportionally with custom height parameter (#279)", {
  skip_on_cran()
  
  # Create simple test data with 2 facets for vertical layout  
  # This setup is most likely to show excessive spacing issues
  test_data <- data.frame(
    x = c(1, 5, 10, 1, 5, 10),
    y = c(1, 5, 10, 1, 5, 10),
    category = c("A", "A", "A", "B", "B", "B")
  )
  
  # Test Case 1: Default height (baseline for comparison)
  viz_default <- ggplot() +
    geom_point(aes(x, y), data = test_data, size = 3) +
    facet_wrap(~category, ncol = 1) +  # Vertical layout
    ggtitle("Default height")
  
  # Test Case 2: Custom height (2x default)
  viz_custom_800 <- ggplot() +
    geom_point(aes(x, y), data = test_data, size = 3) +
    facet_wrap(~category, ncol = 1) +
    theme_animint(height = 800) +  # 2x default 400
    ggtitle("Custom height 800")
  
  # Test Case 3: Custom height (3x default)
  viz_custom_1200 <- ggplot() +
    geom_point(aes(x, y), data = test_data, size = 3) +
    facet_wrap(~category, ncol = 1) +
    theme_animint(height = 1200) +  # 3x default 400
    ggtitle("Custom height 1200")
  
  # Render all three visualizations
  dir_default <- tempfile()
  dir_800 <- tempfile()
  dir_1200 <- tempfile()
  
  info_default <- animint2dir(
    list(plot = viz_default), 
    out.dir = dir_default,
    open.browser = FALSE
  )
  
  info_800 <- animint2dir(
    list(plot = viz_custom_800), 
    out.dir = dir_800,
    open.browser = FALSE
  )
  
  info_1200 <- animint2dir(
    list(plot = viz_custom_1200), 
    out.dir = dir_1200,
    open.browser = FALSE
  )
  
  # Read the generated plot.json files to verify height settings
  json_default <- jsonlite::fromJSON(file.path(dir_default, "plot.json"))
  json_800 <- jsonlite::fromJSON(file.path(dir_800, "plot.json"))
  json_1200 <- jsonlite::fromJSON(file.path(dir_1200, "plot.json"))
  
  # Verify that custom heights are correctly stored in plot.json
  expect_equal(json_800$plots$plot$options$height, 800)
  expect_equal(json_1200$plots$plot$options$height, 1200)
  
  # The bug in issue #279 was: excessive vertical spacing below facets
  # when using custom height != 400 (default)
  # Expected behavior: spacing should scale proportionally, not excessively
  
  # We verify this indirectly by checking that the layout information
  # is consistent across different height values
  
  # Check that panel layout is identical (same number of rows/cols)
  expect_equal(json_default$plots$plot$layout$ROW, json_800$plots$plot$layout$ROW)
  expect_equal(json_default$plots$plot$layout$ROW, json_1200$plots$plot$layout$ROW)
  expect_equal(json_default$plots$plot$layout$COL, json_800$plots$plot$layout$COL)
  expect_equal(json_default$plots$plot$layout$COL, json_1200$plots$plot$layout$COL)
  
  # Check that height_proportion values are identical
  # (these control relative spacing, not absolute)
  expect_equal(json_default$plots$plot$layout$height_proportion, 
               json_800$plots$plot$layout$height_proportion)
  expect_equal(json_default$plots$plot$layout$height_proportion,
               json_1200$plots$plot$layout$height_proportion)
})

test_that("facet_wrap grid layout (2x2) with custom height has proportional spacing (#279)", {
  skip_on_cran()
  
  # Test with a 2x2 grid layout (similar to ROC example in issue)
  test_data_grid <- data.frame(
    x = rep(1:10, 4),
    y = rep(1:10, 4),
    category = rep(c("A", "B", "C", "D"), each = 10)
  )
  
  # Default height
  viz_default <- ggplot() +
    geom_point(aes(x, y), data = test_data_grid) +
    facet_wrap(~category, ncol = 2)
  
  # Custom height (1.5x default)
  viz_custom <- ggplot() +
    geom_point(aes(x, y), data = test_data_grid) +
    facet_wrap(~category, ncol = 2) +
    theme_animint(height = 600)
  
  dir_default <- tempfile()
  dir_custom <- tempfile()
  
  info_default <- animint2dir(list(plot = viz_default), out.dir = dir_default, open.browser = FALSE)
  info_custom <- animint2dir(list(plot = viz_custom), out.dir = dir_custom, open.browser = FALSE)
  
  # Verify custom height is applied
  json_custom <- jsonlite::fromJSON(file.path(dir_custom, "plot.json"))
  expect_equal(json_custom$plots$plot$options$height, 600)
  
  # Check that layout proportions are consistent
  json_default <- jsonlite::fromJSON(file.path(dir_default, "plot.json"))
  expect_equal(json_default$plots$plot$layout$height_proportion,
               json_custom$plots$plot$layout$height_proportion)
  
  # Verify we have 2 rows and 2 columns as expected
  expect_equal(max(json_custom$plots$plot$layout$ROW), 2)
  expect_equal(max(json_custom$plots$plot$layout$COL), 2)
})

test_that("facet_wrap with multiple custom height values maintains proportional spacing (#279)", {
  skip_on_cran()
  
  # Test that spacing proportions are maintained across various custom heights
  test_data <- data.frame(
    x = c(1, 5, 10, 1, 5, 10),
    y = c(1, 5, 10, 1, 5, 10),
    category = c("X", "X", "X", "Y", "Y", "Y")
  )
  
  heights_to_test <- c(400, 600, 800, 1000, 1200)
  layout_proportions <- list()
  
  for (i in seq_along(heights_to_test)) {
    h <- heights_to_test[i]
    
    if (h == 400) {
      # Default height
      viz <- ggplot() +
        geom_point(aes(x, y), data = test_data) +
        facet_wrap(~category, ncol = 1)
    } else {
      # Custom height
      viz <- ggplot() +
        geom_point(aes(x, y), data = test_data) +
        facet_wrap(~category, ncol = 1) +
        theme_animint(height = h)
    }
    
    test_dir <- tempfile()
    info <- animint2dir(list(plot = viz), out.dir = test_dir, open.browser = FALSE)
    
    # Extract layout proportions
    json <- jsonlite::fromJSON(file.path(test_dir, "plot.json"))
    layout_proportions[[i]] <- json$plots$plot$layout$height_proportion
  }
  
  # All layout proportions should be identical
  # (height should scale, but proportions should remain constant)
  for (i in 2:length(layout_proportions)) {
    expect_equal(layout_proportions[[1]], layout_proportions[[i]],
                 info = sprintf("Height %d proportions differ from default (400)", heights_to_test[i]))
  }
})
