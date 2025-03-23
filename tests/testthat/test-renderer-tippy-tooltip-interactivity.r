acontext("tooltip-interactivity")

library(animint2)
library(testthat)

data(CO2)

plot_viz <- ggplot() + 
  geom_point(aes(conc, uptake, color=Treatment, tooltip=Plant),
             data = CO2) +
  geom_rect(aes(xmin=100, xmax=200, ymin=20, ymax=30, 
                tooltip="Test Rectangle"), alpha=0.5)
viz <- list(p=plot_viz)

info <- animint2HTML(viz)

test_that("aria-expanded updates correctly on hover interaction", {
  # Initial state - aria-expanded should be "false"
  initial_circle <- getNodeSet(info$html, '//circle[@data-tippy-content="Mn3"]')[[1]]
  initial_rect <- getNodeSet(info$html, '//rect[@data-tippy-content="Test Rectangle"]')[[1]]
 
  expect_equal(xmlGetAttr(initial_circle, "aria-expanded"), "false")
  expect_equal(xmlGetAttr(initial_rect, "aria-expanded"), "false")

  rect_x <- as.numeric(xmlGetAttr(initial_rect, "x"))
  rect_y <- as.numeric(xmlGetAttr(initial_rect, "y"))
  rect_width <- as.numeric(xmlGetAttr(initial_rect, "width"))
  rect_height <- as.numeric(xmlGetAttr(initial_rect, "height"))
  
  # Calculate center coordinates of the rectangle
  rect_center_x <- rect_x + rect_width / 2
  rect_center_y <- rect_y + rect_height / 2
  
  # Simulate hover on rectangle
  remDr$Input$dispatchMouseEvent(type = "mouseMoved", x = rect_center_x, y = rect_center_y)
  Sys.sleep(1)  # Wait for the tooltip to activate
  
  # Get updated rectangle element
  updated_rect <- getNodeSet(getHTML(), '//rect[@data-tippy-content="Test Rectangle"]')[[1]]
  updated_rect_aria <- xmlGetAttr(updated_rect, "aria-expanded")
  
  # Verify rectangle tooltip activation
  expect_equal(updated_rect_aria, "true")
  
  # Simulate mouseout (move mouse to background)
  remDr$Input$dispatchMouseEvent(type = "mouseMoved", x = 0, y = 0)
  Sys.sleep(1)  # Wait for the tooltips to deactivate
  
  # Get final rectangle element
  final_rect <- getNodeSet(getHTML(), '//rect[@data-tippy-content="Test Rectangle"]')[[1]]
  final_rect_aria <- xmlGetAttr(final_rect, "aria-expanded")
  expect_equal(final_rect_aria, "false")
})
