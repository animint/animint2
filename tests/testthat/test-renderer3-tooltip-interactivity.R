acontext("tooltip-interactivity")

data("CO2")

plot_viz <- ggplot() + 
  geom_point(aes(conc, uptake, color=Treatment, tooltip=Plant),
             data = CO2)+
  geom_rect(aes(xmin=100, xmax=200, ymin=20, ymax=30, 
              tooltip="Test Rectangle"), alpha=0.5)
viz <- list(p=plot_viz)
info <- animint2HTML(viz)

test_that("tooltip shows correct content on hover interaction", {
  # Initial state - tooltip should be hidden
  tooltip_div <- getNodeSet(info$html, '//div[@class="animint-tooltip"]')[[1]]
  expect_equal(xmlGetAttr(tooltip_div, "style"), "opacity: 0;")
  
  # Find the rectangle element
  rect_node <- getNodeSet(info$html, '//g[contains(@class,"geom2_rect")]//rect')[[1]]
  rect_x <- as.numeric(xmlGetAttr(rect_node, "x"))
  rect_y <- as.numeric(xmlGetAttr(rect_node, "y"))
  rect_width <- as.numeric(xmlGetAttr(rect_node, "width"))
  rect_height <- as.numeric(xmlGetAttr(rect_node, "height"))
  # Calculate center coordinates
  rect_center_x <- rect_x + rect_width / 2
  rect_center_y <- rect_y + rect_height / 2
  # Simulate hover on rectangle
  remDr$Input$dispatchMouseEvent(type = "mouseMoved", 
                                x = rect_center_x + 10,  # Adjust for 10px left margin in the plot
                                y = rect_center_y)
  Sys.sleep(1)  # Wait for tooltip
  # Verify tooltip is visible and shows correct content
  tooltip_div <- getNodeSet(getHTML(), '//div[@class="animint-tooltip"]')[[1]]
  expect_match(xmlGetAttr(tooltip_div, "style"), "opacity: 1;")
  expect_equal(xmlValue(tooltip_div), "Test Rectangle")
  
  # Simulate mouseout (move mouse to background)
  remDr$Input$dispatchMouseEvent(type = "mouseMoved", x = 0, y = 0)
  clickID("background")
  Sys.sleep(1)
  
  # Verify tooltip is hidden again
  tooltip_div <- getNodeSet(getHTML(), '//div[@class="animint-tooltip"]')[[1]]
  expect_match(xmlGetAttr(tooltip_div, "style"), "opacity: 0;")
  expect_equal(xmlValue(tooltip_div), "") # Content should be cleared
})