context("Rosling Bubbles")

# Ensure we have the live rendered HTML (not the "Not found" placeholder)
if (!grepl("Interactive animation", saveXML(info$html))) {
  file.copy("animint-htmltest", "tests/testthat/", recursive=TRUE, overwrite=TRUE)
  remDr$Page$navigate("http://127.0.0.1:4848/animint-htmltest/index.html")
  Sys.sleep(4)
  info$html <- getHTML()
}

test_that("animint2HTML returns a non-empty HTML document", {
  html_str <- saveXML(info$html)
  expect_true(nchar(html_str) > 0)
})

test_that("SVG container for bubbles plot exists", {
  svg_nodes <- getNodeSet(info$html, '//svg[@id="plot_bubbles"]')
  expect_true(length(svg_nodes) >= 1L)
})

test_that("Correct number of circles rendered in the plot area", {
  xpath   <- '//svg[@id="plot_bubbles"]//circle'
  circles <- getNodeSet(info$html, xpath)
  expect_equal(length(circles), n_subjects)
})

test_that("Each circle has numeric cx and cy attributes", {
  xpath   <- '//svg[@id="plot_bubbles"]//circle'
  circles <- getNodeSet(info$html, xpath)
  cx_vals <- as.numeric(sapply(circles, xmlGetAttr, "cx"))
  cy_vals <- as.numeric(sapply(circles, xmlGetAttr, "cy"))
  expect_true(all(!is.na(cx_vals)))
  expect_true(all(!is.na(cy_vals)))
})

test_that("Animation widgets (Play/Pause) are present", {
  inputs <- getNodeSet(info$html, '//input')
  vals   <- sapply(inputs, function(n) xmlGetAttr(n, "value"))
  expect_true(any(sapply(vals, is.null)))
})

test_that("Year selector is registered in info selectors", {
  expect_true("year" %in% names(info$selectors))
})

test_that("Circles have class attributes (clickSelects rendered)", {
  xpath    <- '//svg[@id="plot_bubbles"]//circle'
  circles  <- getNodeSet(info$html, xpath)
  has_attr <- sapply(circles, function(n) {
    !is.null(xmlGetAttr(n, "id")) || !is.null(xmlGetAttr(n, "class"))
  })
  expect_true(all(unlist(has_attr)))
})

test_that("Before click: all 10 circles present", {
  xpath   <- '//svg[@id="plot_bubbles"]//circle'
  circles <- getNodeSet(info$html, xpath)
  expect_equal(length(circles), n_subjects)
})

test_that("After JS click on Subject 1 legend: its circle is removed (9 remain)", {
  remDr$Runtime$evaluate(
    expression = 'document.getElementById("plot_bubbles_id_variable_Subject_1_svg").dispatchEvent(new MouseEvent("click", {bubbles: true}))'
  )
  Sys.sleep(2)
  info$html <<- getHTML()
  xpath   <- '//svg[@id="plot_bubbles"]//circle'
  circles <- getNodeSet(info$html, xpath)
  expect_equal(length(circles), n_subjects - 1L)
})

test_that("After second JS click on Subject 1 legend: circle is restored (10 again)", {
  remDr$Runtime$evaluate(
    expression = 'document.getElementById("plot_bubbles_id_variable_Subject_1_svg").dispatchEvent(new MouseEvent("click", {bubbles: true}))'
  )
  Sys.sleep(2)
  info$html <<- getHTML()
  xpath   <- '//svg[@id="plot_bubbles"]//circle'
  circles <- getNodeSet(info$html, xpath)
  expect_equal(length(circles), n_subjects)
})