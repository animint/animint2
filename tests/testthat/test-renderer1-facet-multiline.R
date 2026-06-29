acontext("facet-multiline")

get_strip_tspan_labels <- function(html, side) {
  strip_class <- if (side == "right") "rightStrip" else "topStrip"
  xpath <- sprintf("//g[@class='%s']//tspan", strip_class)
  nodes <- getNodeSet(html, xpath)
  as.character(sapply(nodes, xmlValue))
}

p <- ggplot(mtcars, aes(mpg, wt)) + geom_point(colour = "grey50", size = 4)

multiGridViz <- list(multiGridPlot = p + facet_grid(cyl + am ~ ., labeller = label_both))
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
