acontext("tooltip-interactivity")

data("CO2")

plot_viz <- ggplot() + 
  geom_point(aes(conc, uptake, color=Treatment, tooltip=Plant , id = paste0(CO2$Plant, "_", seq_len(nrow(CO2)))), # Unique ID for every circle
             data = CO2)+
  geom_rect(aes(xmin=100, xmax=200, ymin=20, ymax=30, 
              tooltip="Test Rectangle"), alpha=0.5)
viz <- list(p=plot_viz)
info <- animint2HTML(viz)

test_that(".animint-tooltip exists and is hidden initially", {
  # Initial state - tooltip should be hidden
  tooltip_div <- getNodeSet(info$html, '//div[@class="animint-tooltip"]')[[1]]
  expect_equal(xmlGetAttr(tooltip_div, "style"), "opacity: 0;")
})
test_that("tooltip shows correct content on hover interaction", {
  # Get rectangle position on the viewport
  rect_position <- get_element_bbox('g[class*=\"geom2_rect\"] rect')
  # Hover over the rect
  remDr$Input$dispatchMouseEvent(
    type = "mouseMoved", 
    x = rect_position$left,
    y = rect_position$top
  )
  Sys.sleep(0.5)
  # Verify tooltip is visible and shows correct content
  tooltip_div <- getNodeSet(getHTML(), '//div[@class="animint-tooltip"]')[[1]]
  expect_match(xmlGetAttr(tooltip_div, "style"), "opacity: 1;")
  expect_equal(xmlValue(tooltip_div), "Test Rectangle")
  # Simulate mouseout
  remDr$Input$dispatchMouseEvent(type = "mouseMoved", x = 0, y = 0)
})

test_that("Interactivity does not mess up tooltips", {
  # Hide all nonchilled points
  clickID("plot_p_Treatment_variable_nonchilled")
  # Hover over a chilled point
  bbox_chilled <- get_element_bbox('circle#Qc1_22')
  remDr$Input$dispatchMouseEvent(
    type = "mouseMoved",
    x = bbox_chilled$center_x,
    y = bbox_chilled$center_y
  )
  Sys.sleep(0.2)
  tooltip_div <- getNodeSet(getHTML(), '//div[@class="animint-tooltip"]')[[1]]
  tooltip_text <- xmlValue(tooltip_div)
  expect_match(tooltip_text, "Qc1", info = "Expected tooltip for chilled point Qc1")

  # show nonchilled points
  clickID("plot_p_Treatment_variable_nonchilled")
  Sys.sleep(0.2)

  # Hover over a nonchilled point
  bbox_nonchilled <- get_element_bbox('circle#Qn1_3')
  remDr$Input$dispatchMouseEvent(
    type = "mouseMoved",
    x = bbox_nonchilled$center_x,
    y = bbox_nonchilled$center_y
  )
  Sys.sleep(0.2)
  tooltip_div <- getNodeSet(getHTML(), '//div[@class="animint-tooltip"]')[[1]]
  tooltip_text <- xmlValue(tooltip_div)
  expect_match(tooltip_text, "Qn1", info = "Expected tooltip for nonchilled point Qn1")

  # Move mouse away at the end
  remDr$Input$dispatchMouseEvent(type = "mouseMoved", x = 0, y = 0)
})

test_that("tooltip is hidden after mouseout", {
  tooltip_div <- getNodeSet(getHTML(), '//div[@class="animint-tooltip"]')[[1]]
  expect_match(xmlGetAttr(tooltip_div, "style"), "opacity: 0;")
  expect_equal(xmlValue(tooltip_div), "") # Content should be cleared
})