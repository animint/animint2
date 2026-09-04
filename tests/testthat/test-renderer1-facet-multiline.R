acontext("facet-multiline")

get_strip_tspan_labels <- function(html, side) {
  strip_class <- if (side == "right") "rightStrip" else "topStrip"
  xpath <- sprintf("//g[@class='%s']//tspan", strip_class)
  nodes <- getNodeSet(html, xpath)
  as.character(sapply(nodes, xmlValue))
}

# positive = strip extends into panel past the shared edge
strip_panel_intrusion <- function(strip_sel, panel_sel, side) {
  strip_bbox <- get_element_bbox(strip_sel)
  panel_bbox <- get_element_bbox(panel_sel)
  expect_gt(strip_bbox$width, 0)
  expect_gt(panel_bbox$width, 0)
  if (side == "top") {
    (strip_bbox$top + strip_bbox$height) - panel_bbox$top
  } else {
    (panel_bbox$left + panel_bbox$width) - strip_bbox$left
  }
}

p <- ggplot(mtcars, aes(mpg, wt)) + geom_point(colour = "grey50", size = 4)

singleGridViz <- list(singleGridPlot = p + facet_grid(cyl ~ ., labeller = label_both))
multiGridViz <- list(multiGridPlot = p + facet_grid(cyl + am ~ ., labeller = label_both))
singleWrapViz <- list(singleWrapPlot = p + facet_wrap(~cyl, labeller = label_both))
multiWrapViz <- list(multiWrapPlot = p + facet_wrap(~cyl + am, labeller = label_both))

# mtcars facet_grid(cyl + am ~ .): 3 cyl x 2 am = 6 panels; 2 strip lines each => 12 tspans
test_that("facet_grid() multi-variable strip labels render on separate lines", {
  info <- animint2HTML(multiGridViz)
  labels <- get_strip_tspan_labels(info$html, "right")
  expect_equal(length(labels), 12)
  expect_equal(labels[1], "cyl: 4")
  expect_equal(labels[2], "am: 0")
  expect_equal(sum(grepl("; ", labels)), 0)
})

# mtcars facet_wrap(~cyl + am): 6 panels; 2 strip lines each => 12 tspans
test_that("facet_wrap() multi-variable strip labels render on separate lines", {
  info <- animint2HTML(multiWrapViz)
  labels <- get_strip_tspan_labels(info$html, "top")
  expect_equal(length(labels), 12)
  expect_equal(labels[1], "cyl: 4")
  expect_equal(labels[2], "am: 0")
  expect_equal(sum(grepl(", ", labels)), 0)
})

pixel_slack <- 1

test_that("multiline top strips do not intrude more than single-line strips", {
  animint2HTML(singleWrapViz)
  single_overlap <- strip_panel_intrusion(
    "#plot_singleWrapPlot .topStrip text",
    "#plot_singleWrapPlot .bgr1 rect.background_rect",
    "top")
  animint2HTML(multiWrapViz)
  multi_overlap <- strip_panel_intrusion(
    "#plot_multiWrapPlot .topStrip text",
    "#plot_multiWrapPlot .bgr1 rect.background_rect",
    "top")
  expect_lte(multi_overlap, single_overlap + pixel_slack)
})

test_that("multiline right strips do not intrude more than single-line strips", {
  animint2HTML(singleGridViz)
  single_overlap <- strip_panel_intrusion(
    "#plot_singleGridPlot .rightStrip text",
    "#plot_singleGridPlot .bgr1 rect.background_rect",
    "right")
  animint2HTML(multiGridViz)
  multi_overlap <- strip_panel_intrusion(
    "#plot_multiGridPlot .rightStrip text",
    "#plot_multiGridPlot .bgr1 rect.background_rect",
    "right")
  expect_lte(multi_overlap, single_overlap + pixel_slack)
})
