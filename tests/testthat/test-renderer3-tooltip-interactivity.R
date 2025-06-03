acontext("tooltip-interactivity")

data("CO2")

plot_viz <- ggplot() + 
  geom_point(aes(conc, uptake, color=Treatment, tooltip=Plant),
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
  rect_position <- runtime_evaluate(
  "(() => {
      const rect = document.querySelector('g[class*=\"geom2_rect\"] rect');
      const box = rect.getBoundingClientRect();
      return {
        left: box.left,
        top: box.top
      };
  })()"
  )
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
  clickID("background")
})

test_that("tooltip is hidden after mouseout", {
  tooltip_div <- getNodeSet(getHTML(), '//div[@class="animint-tooltip"]')[[1]]
  expect_match(xmlGetAttr(tooltip_div, "style"), "opacity: 0;")
  expect_equal(xmlValue(tooltip_div), "") # Content should be cleared
})