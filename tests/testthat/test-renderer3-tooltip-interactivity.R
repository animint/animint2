acontext("tooltip-interactivity")

data("CO2")

plot_viz <- ggplot() + 
  geom_point(aes(conc, uptake, color=Treatment, tooltip=Plant , id = paste0(Plant, "_", seq_len(nrow(CO2)))), # Unique ID for every circle
             data = CO2)+
  geom_rect(aes(xmin=100, xmax=200, ymin=20, ymax=30, 
              tooltip="Test Rectangle"), alpha=0.5)
viz <- list(p=plot_viz)
info <- animint2HTML(viz)

tooltip.xpath <- '//div[@class="animint-tooltip"]'
test_that(".animint-tooltip exists and is hidden initially", {
  # Initial state - tooltip should be hidden
  opacity <- getStyleValue(info$html, tooltip.xpath, "opacity")
  expect_identical(opacity, "0")
})
test_that("tooltip shows correct content on hover interaction", {
  mouseMoved('g[class*=\"geom2_rect\"] rect')
  opacity <- getStyleValue(getHTML(), tooltip.xpath, "opacity")
  expect_gt(as.numeric(opacity), 0)
  tooltip_div <- getNodeSet(getHTML(), tooltip.xpath)[[1]]
  expect_equal(xmlValue(tooltip_div), "Test Rectangle")
  mouseMoved()
})

test_that("Interactivity does not mess up tooltips", {
  # Hide all nonchilled points
  clickID("plot_p_Treatment_variable_nonchilled")
  Sys.sleep(0.5)
  mouseMoved('circle#Qc1_22')
  tooltip_div <- getNodeSet(getHTML(), tooltip.xpath)[[1]]
  tooltip_text <- xmlValue(tooltip_div)
  expect_match(tooltip_text, "Qc1", info = "Expected tooltip for chilled point Qc1")
  clickID("plot_p_Treatment_variable_nonchilled")
  Sys.sleep(0.2)
  mouseMoved('circle#Mn3_60')
  tooltip_div <- getNodeSet(getHTML(), tooltip.xpath)[[1]]
  tooltip_text <- xmlValue(tooltip_div)
  expect_match(tooltip_text, "Mn3", info = "Expected tooltip for nonchilled point Qn1")
  mouseMoved()
})

test_that("tooltip is hidden after mouseout", {
  opacity <- getStyleValue(getHTML(), tooltip.xpath, "opacity")
  expect_identical(opacity, "0")
})
