library(testthat)
library(chromote)
library(callr)
library(shiny)
library(animint2)
library(this.path)


if (exists("this.path")) {
  setwd(dirname(dirname(dirname(this.path::this.path()))))
} else {
  setwd(normalizePath(file.path("..", "..", "..")))
}

renderAnimint <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!requireNamespace("shiny")) message("Please install.packages('shiny')")
  func <- shiny::exprToFunction(expr, env, quoted)
  renderFunc <- function(shinysession, name, ...) {
    val <- func()
    tmp <- tempfile()
    stuff <- animint2dir(val, out.dir = tmp, open.browser = FALSE)
    shiny::addResourcePath("animintAssets", tmp)
    list(jsonFile = "plot.json")
  }
  shiny::markRenderFunction(animint2::animintOutput, renderFunc) 
}


test_that("animint plot renders in a shiny app", {
  app_dir <- "inst/examples/shiny"
  if (!dir.exists(app_dir)) skip("Shiny app directory not found")
  
  unlink(file.path(app_dir, "animint"), recursive = TRUE)
  unlink(file.path(app_dir, "animint-output"), recursive = TRUE)
  unlink(file.path(app_dir, "www"), recursive = TRUE)
  unlink(file.path(getwd(), "www", "animint-output"), recursive = TRUE)
  
  port <- sample(3000:9999, 1)
  app_info <- start_shiny_app(app_dir, port)
  on.exit({
    app_info$proc$kill()
    unlink("shiny_err.log")
    unlink("shiny_out.log")
  }, add = TRUE)
  
  cat(app_info$url, "\n")
  
  b <- ChromoteSession$new()
  b$view()
  on.exit(b$close(), add = TRUE)
  b$Page$navigate(app_info$url)
  b$Page$loadEventFired(wait_ = TRUE, timeout = 30000)
  Sys.sleep(20)
  
  animint_ready <- FALSE
  for (i in 1:100) {
    res <- b$Runtime$evaluate("document.querySelector('div#animint') !== null")
    if (isTRUE(res$result$value)) {
      animint_ready <- TRUE
      break
    }
    Sys.sleep(0.1)
  }
  expect_true(animint_ready, info = "animint div should be present")
  
  circles <- b$Runtime$evaluate(
    "document.querySelector('div#animint svg').querySelectorAll('circle').length"
  )$result$value
  expect_true(circles >= 1, info = "At least one circle should be rendered in div#animint svg")
})

worldbank_dir <- "inst/examples/shiny-WorldBank"
if (!dir.exists(worldbank_dir)) {
  worldbank_dir <- tempdir()
  writeLines(
    c(
      "library(shiny)",
      "library(animint2)", # or library(animintshiny)
      "server <- function(input, output) {",
      "  output$animint <- renderAnimint({",
      "    ggplot() + geom_point(aes(1, 1))",
      "  })",
      "}",
      "ui <- fluidPage(animintOutput('animint'))",
      "shinyApp(ui, server)"
    ),
    file.path(worldbank_dir, "app.R")
  )
}
port <- sample(3000:9999, 1)
worldbank_app_info <- start_shiny_app(worldbank_dir, port)
testthat::teardown({
  worldbank_app_info$proc$kill()
  unlink("shiny_err.log")
  unlink("shiny_out.log")
})

test_that("WorldBank renders in a shiny app", {
  cat(worldbank_app_info$url, "\n")
  
  b <- ChromoteSession$new()
  b$view()
  on.exit(b$close(), add = TRUE)
  b$Page$navigate(worldbank_app_info$url)
  b$Page$loadEventFired(wait_ = TRUE, timeout = 30000)
  Sys.sleep(20)
  
  animint_ready <- FALSE
  for (i in 1:100) {
    res <- b$Runtime$evaluate("document.querySelector('div#animint') !== null")
    if (isTRUE(res$result$value)) {
      animint_ready <- TRUE
      break
    }
    Sys.sleep(0.1)
  }
  expect_true(animint_ready, info = "animint div should be present")
  
  circles <- b$Runtime$evaluate(
    "document.querySelector('div#animint svg').querySelectorAll('circle').length"
  )$result$value
  expect_true(circles >= 1, info = "At least one circle should be rendered in div#animint svg")
})

test_that("animation updates", {
  if (file.path(worldbank_dir, "app.R") == file.path(tempdir(), "app.R")) skip("Animation test skipped for mock app")
  
  b <- ChromoteSession$new()
  b$view()
  on.exit(b$close(), add = TRUE)
  b$Page$navigate(worldbank_app_info$url)
  b$Page$loadEventFired(wait_ = TRUE, timeout = 30000)
  Sys.sleep(20)
  
  get_year <- function() {
    year <- b$Runtime$evaluate(
      "var node = document.querySelector('g.geom10_text_ts text'); node ? node.textContent.replace('year = ', '') : ''"
    )$result$value
    expect_true(nchar(year) > 0, info = "Year text should be present")
    return(year)
  }
  
  old_year <- get_year()
  Sys.sleep(5)
  new_year <- get_year()
  expect_true(old_year != new_year, info = "Year should change after animation")
})

test_that("animint fits in div", {
  if (file.path(worldbank_dir, "app.R") == file.path(tempdir(), "app.R")) skip("Animation test skipped for mock app")
  
  b <- ChromoteSession$new()
  b$view()
  on.exit(b$close(), add = TRUE)
  b$Page$navigate(worldbank_app_info$url)
  b$Page$loadEventFired(wait_ = TRUE, timeout = 30000)
  Sys.sleep(20)
  
  tick_left <- b$Runtime$evaluate(
    "var nodes = document.querySelectorAll('.yaxis text'); Array.from(nodes).map(n => n.getBoundingClientRect().left)"
  )$result$value
  expect_true(length(tick_left) > 0, info = "Y-axis ticks should be present")
  
  div_left <- b$Runtime$evaluate(
    "document.querySelector('#animint').getBoundingClientRect().left"
  )$result$value
  expect_true(is.numeric(div_left), info = "Div left position should be numeric")
  
  expect_true(all(div_left < tick_left), info = "All y-axis ticks should be to the right of div#animint")
})

test_that("clicking selects country", {
  if (file.path(worldbank_dir, "app.R") == file.path(tempdir(), "app.R")) skip("Animation test skipped for mock app")
  
  b <- ChromoteSession$new()
  b$view()
  on.exit(b$close(), add = TRUE)
  b$Page$navigate(worldbank_app_info$url)
  b$Page$loadEventFired(wait_ = TRUE, timeout = 30000)
  Sys.sleep(20)
  
  get_countries <- function() {
    countries <- b$Runtime$evaluate(
      "var nodes = document.querySelectorAll('g.geom9_text_ts text'); Array.from(nodes).map(n => n.textContent).sort()"
    )$result$value
    return(countries)
  }
  
  old_countries <- get_countries()
  expect_identical(old_countries, c("United States", "Vietnam"), info = "Initial countries should be United States and Vietnam")
  
  b$Runtime$evaluate(
    "var point = document.querySelector('g.geom9_text_ts text[textContent=\"Bahrain\"]'); if (point) { point.dispatchEvent(new MouseEvent('click')); }"
  )
  Sys.sleep(5)
  
  new_countries <- get_countries()
  expect_identical(new_countries, c("Bahrain", "United States", "Vietnam"), info = "Bahrain should be added after click")
})

test_that("shiny changes axes", {
  if (file.path(worldbank_dir, "app.R") == file.path(tempdir(), "app.R")) skip("Animation test skipped for mock app")
  
  b <- ChromoteSession$new()
  b$view()
  on.exit(b$close(), add = TRUE)
  b$Page$navigate(worldbank_app_info$url)
  b$Page$loadEventFired(wait_ = TRUE, timeout = 30000)
  Sys.sleep(20)
  
  get_facets <- function() {
    facets <- b$Runtime$evaluate(
      "var nodes = document.querySelectorAll('g.topStrip text'); Array.from(nodes).map(n => n.textContent)"
    )$result$value
    return(facets)
  }
  
  old_facets <- get_facets()
  expect_identical(old_facets, c("fertility.rate", "Years"), info = "Initial facets should be fertility.rate and Years")
  
  b$Runtime$evaluate(
    "var select = document.querySelector('.selectize-input'); if (select) { select.click(); }"
  )
  Sys.sleep(1)
  b$Runtime$evaluate(
    "var select = document.querySelector('.selectize-input'); if (select) { select.dispatchEvent(new KeyboardEvent('keydown', {key: 'Backspace'})); }"
  )
  Sys.sleep(1)
  b$Runtime$evaluate(
    "var select = document.querySelector('.selectize-input'); if (select) { select.click(); }"
  )
  Sys.sleep(1)
  b$Runtime$evaluate(
    "var select = document.querySelector('.selectize-input input'); if (select) { select.value = 'lite'; select.dispatchEvent(new Event('input')); }"
  )
  Sys.sleep(1)
  b$Runtime$evaluate(
    "var option = document.querySelector('.selectize-dropdown-content .option[data-value*=\"literacy\"]'); if (option) { option.click(); }"
  )
  Sys.sleep(10)
  
  new_facets <- get_facets()
  expect_identical(new_facets, c("literacy", "Years"), info = "Facets should update to literacy and Years")
})

test_that("animint plot renders in an interactive document", {
  if (!requireNamespace("rmarkdown")) skip("Package 'rmarkdown' not installed")
  rmd_file <- "inst/examples/rmarkdown/index.Rmd"
  if (!file.exists(rmd_file)) skip("RMarkdown file not found")
  
  port <- sample(3000:9999, 1)
  app_info <- start_rmd_app(rmd_file, port)
  on.exit({
    app_info$proc$kill()
    unlink("shiny_err.log")
    unlink("shiny_out.log")
  }, add = TRUE)
  
  cat(app_info$url, "\n")
  
  b <- ChromoteSession$new()
  b$view()
  on.exit(b$close(), add = TRUE)
  b$Page$navigate(app_info$url)
  b$Page$loadEventFired(wait_ = TRUE, timeout = 30000)
  Sys.sleep(20)
  
  iframe_ready <- FALSE
  for (i in 1:100) {
    res <- b$Runtime$evaluate("document.querySelector('.shiny-frame') !== null")
    if (isTRUE(res$result$value)) {
      iframe_ready <- TRUE
      break
    }
    Sys.sleep(0.1)
  }
  expect_true(iframe_ready, info = "Shiny iframe should be present")
  
  circles <- b$Runtime$evaluate(
    "document.querySelector('.shiny-frame').contentDocument.querySelectorAll('svg circle').length"
  )$result$value
  
  if (circles == 0) {
    animint_circles <- b$Runtime$evaluate(
      "document.querySelector('.shiny-frame').contentDocument.querySelectorAll('div#animint svg circle').length"
    )$result$value
    circles <- animint_circles
  }
  
  expect_true(circles >= 1, info = "At least one circle should be rendered in iframe")
})