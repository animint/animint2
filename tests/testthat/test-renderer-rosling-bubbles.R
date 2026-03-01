animint2HTML_local <- function(plotList) {
  tmp_name <- "test_render_final"
  tmp_path <- file.path(getwd(), tmp_name)
  if(dir.exists(tmp_path)) unlink(tmp_path, recursive = TRUE, force = TRUE)
  
  info <- animint2dir(plotList, out.dir = tmp_path, open.browser = FALSE)
  url <- paste0("http://127.0.0.1:4848/", tmp_name, "/index.html")
  
  message("--- Debug: Navigating via JS...")
  # Fire and forget navigation
  try(.GlobalEnv$b$Runtime$evaluate(paste0("window.location.href = '", url, "'")), silent = TRUE)
  
  message("--- Debug: Waiting 15s for D3...")
  Sys.sleep(15) 
  
  message("--- Debug: Forcing DOM Capture with explicit timeout...")
  # We ask Chrome to give us the HTML but we wrap it in a promise that R won't kill
  res <- .GlobalEnv$b$Runtime$evaluate(
    "document.getElementsByTagName('html')[0].innerHTML",
    returnByValue = TRUE,
    awaitPromise = TRUE
  )
  
  if (is.null(res$result$value)) stop("Capture failed - Bridge broken.")
  
  # Reconstruct full HTML from innerHTML
  full_html <- paste0("<html>", res$result$value, "</html>")
  info$html <- XML::htmlParse(full_html, asText = TRUE)
  return(info)
}

context("Rosling Bubbles Hard Test")

library(animint2)
library(XML)

## Override getHTML to capture live post-JS DOM via Runtime$evaluate
## The default getHTML uses getPageSource() which returns pre-JS HTML
getHTML <- function(){
  res <- remDr$Runtime$evaluate("document.documentElement.outerHTML")
  XML::htmlParse(res$result$value, asText = TRUE)
}

## Override animint2HTML with longer wait for animint.js to finish rendering
animint2HTML <- function(plotList) {
  unlink("animint-htmltest", recursive=TRUE)
  res <- animint2dir(plotList, out.dir="animint-htmltest", open.browser=FALSE)
  remDr$refresh()
  Sys.sleep(8)
  res$html <- getHTML()
  res
}

## ── 1. Data ────────────────────────────────────────────────────────────────
set.seed(42)
rosling_data <- data.frame(
  id   = factor(rep(paste("Subject", 1:10), times = 5)),
  year = rep(1:5, each = 10),
  x    = rnorm(50, 5, 2),
  y    = rnorm(50, 5, 2),
  size = runif(50, 1, 5)
)

## ── 2. Viz ─────────────────────────────────────────────────────────────────
viz <- animint(
  bubbles = ggplot() +
    geom_point(
      data = rosling_data,
      aes(x = x, y = y, size = size, fill = id, key = id),
      showSelected  = "year",
      clickSelects  = "id",
      shape = 21
    ) +
    ggtitle("Interactive Bubbles Plot"),
  time = list(variable = "year", ms = 1000)
)

## ── 3. Render ─────────────────────────────────────────────────────────────
info <- animint2HTML(viz)

## ── 4. Tests ───────────────────────────────────────────────────────────────
test_that("animint2HTML returns a non-empty HTML document", {
  html_str <- saveXML(info$html)
  expect_true(nchar(html_str) > 0)
})

test_that("SVG container for bubbles plot exists", {
  svg_nodes <- getNodeSet(info$html, '//svg[@id="plot_bubbles"]')
  expect_equal(length(svg_nodes), 1L)
})

test_that("Correct number of circles rendered for the first year (showSelected)", {
  circles <- getNodeSet(info$html, '//svg[@id="plot_bubbles"]//circle')
  expect_equal(length(circles), 10L)
})

test_that("Each circle has a numeric cx attribute (x position rendered)", {
  circles <- getNodeSet(info$html, '//svg[@id="plot_bubbles"]//circle')
  cx_values <- as.numeric(sapply(circles, xmlGetAttr, "cx"))
  expect_true(all(!is.na(cx_values)))
})

test_that("Each circle has a numeric cy attribute (y position rendered)", {
  circles <- getNodeSet(info$html, '//svg[@id="plot_bubbles"]//circle')
  cy_values <- as.numeric(sapply(circles, xmlGetAttr, "cy"))
  expect_true(all(!is.na(cy_values)))
})

test_that("Year time animation widget is present in the rendered HTML", {
  ## The year selector renders as a <td> inside a table with class
  ## "table_selector_widgets" — confirmed from getSelectorWidgets() in helpers
  year_td <- getNodeSet(
    info$html,
    '//table[contains(@class,"selector")]//td | //table[contains(@class,"animint")]//td'
  )
  ## Also check for any element with "year" in id/class (time variable widget)
  year_el <- getNodeSet(
    info$html,
    '//*[contains(@id,"year") or contains(@class,"year")]'
  )
  ## Also check for Play/Pause buttons from time animation
  play_btn <- getNodeSet(
    info$html,
    '//input[@value="Play" or @value="Pause" or @value="Next"]'
  )
  expect_true(
    length(year_td) >= 1L || length(year_el) >= 1L || length(play_btn) >= 1L
  )
})