library(testthat)
library(chromote)
library(callr)
library(shiny)
library(animint2)


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

# Helper function to start Shiny app 
start_shiny_app <- function(app_dir, port) {
  if (!dir.exists(app_dir)) stop("App directory does not exist: ", app_dir)
  app_url <- sprintf("http://127.0.0.1:%d", port)
  proc <- callr::r_bg(function(app_dir, port) {
    shiny::runApp(app_dir, port = port, launch.browser = FALSE)
  }, args = list(app_dir = app_dir, port = port), stderr = "shiny_err.log", stdout = "shiny_out.log")
  
  start_time <- Sys.time()
  app_started <- FALSE
  while (Sys.time() - start_time < 30) {
    if (!proc$is_alive()) {
      err <- paste(readLines("shiny_err.log", warn = FALSE), collapse = "\n")
      cat("Shiny process stderr:\n", err, "\n")
      stop("Shiny app failed: ", proc$get_error())
    }
    con <- try(socketConnection("localhost", port, open = "r+", timeout = 5), silent = TRUE)
    if (!inherits(con, "try-error")) {
      close(con)
      app_started <- TRUE
      break
    }
    Sys.sleep(0.5)
  }
  if (!app_started) stop("Failed to start Shiny app after 30 seconds")
  return(list(proc = proc, url = app_url))
}

# Helper function to start RMarkdown app 
start_rmd_app <- function(rmd_file, port) {
  if (!file.exists(rmd_file)) stop("RMarkdown file does not exist: ", rmd_file)
  if (!requireNamespace("rmarkdown")) stop("Package 'rmarkdown' is not installed")
  app_url <- sprintf("http://127.0.0.1:%d", port)
  proc <- callr::r_bg(function(rmd_file, port) {
    rmarkdown::run(file = rmd_file, shiny_args = list(port = port, launch.browser = FALSE))
  }, args = list(rmd_file = rmd_file, port = port), stderr = "shiny_err.log", stdout = "shiny_out.log")
  
  start_time <- Sys.time()
  app_started <- FALSE
  while (Sys.time() - start_time < 30) {
    if (!proc$is_alive()) {
      err <- paste(readLines("shiny_err.log", warn = FALSE), collapse = "\n")
      cat("RMarkdown process stderr:\n", err, "\n")
      stop("RMarkdown app failed: ", proc$get_error())
    }
    con <- try(socketConnection("localhost", port, open = "r+", timeout = 5), silent = TRUE)
    if (!inherits(con, "try-error")) {
      close(con)
      app_started <- TRUE
      break
    }
    Sys.sleep(0.5)
  }
  if (!app_started) stop("Failed to start RMarkdown app after 30 seconds")
  return(list(proc = proc, url = app_url))
}

# Helper function to start RMarkdown app
start_rmd_app <- function(app_dir, port) {
  if (!dir.exists(app_dir)) stop("App directory does not exist: ", app_dir)
  if (!requireNamespace("rmarkdown")) stop("Package 'rmarkdown' is not installed")
  app_url <- sprintf("http://127.0.0.1:%d", port)
  proc <- callr::r_bg(function(app_dir, port) {
    rmarkdown::run(dir = app_dir, shiny_args = list(port = port, launch.browser = FALSE))
  }, args = list(app_dir = app_dir, port = port), stderr = "shiny_err.log", stdout = "shiny_out.log")
  
  start_time <- Sys.time()
  app_started <- FALSE
  while (Sys.time() - start_time < 30) {
    if (!proc$is_alive()) {
      err <- paste(readLines("shiny_err.log", warn = FALSE), collapse = "\n")
      cat("RMarkdown process stderr:\n", err, "\n")
      stop("RMarkdown app failed: ", proc$get_error())
    }
    con <- try(socketConnection("localhost", port, open = "r+", timeout = 5), silent = TRUE)
    if (!inherits(con, "try-error")) {
      close(con)
      app_started <- TRUE
      break
    }
    Sys.sleep(0.5)
  }
  if (!app_started) stop("Failed to start RMarkdown app after 30 seconds")
  return(list(proc = proc, url = app_url))
}

# Test animint plot rendering in a Shiny app
test_that("animint plot renders in a shiny app", {
  app_dir <- "examples/shiny"
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
  
  cat("Attempting to access app at:", app_info$url, "\n")
  
  b <- ChromoteSession$new()
  b$view()
  on.exit(b$close(), add = TRUE)
  b$Page$navigate(app_info$url)
  b$Page$loadEventFired(wait_ = TRUE, timeout = 30000)
  Sys.sleep(20)  # Match RSelenium's 20s wait
  
  div_classes <- b$Runtime$evaluate(
    "Array.from(document.querySelectorAll('div')).map(d => d.className).join(', ')"
  )$result$value
  cat("All div classes:\n", div_classes, "\n")
  
  animint_ready <- FALSE
  animint_html <- ""
  for (i in 1:100) {
    res <- b$Runtime$evaluate("document.querySelector('div#animint') !== null")
    if (isTRUE(res$result$value)) {
      animint_ready <- TRUE
      animint_html <- b$Runtime$evaluate(
        "document.querySelector('div#animint').outerHTML"
      )$result$value
      break
    }
    Sys.sleep(0.1)
  }
  expect_true(animint_ready, info = "animint div should be present")
  cat("Animint div HTML (first 1000 chars):\n", substr(animint_html, 1, 1000), "\n")
  
  circles <- b$Runtime$evaluate(
    "document.querySelector('div#animint svg').querySelectorAll('circle').length"
  )$result$value
  cat("Number of circle elements:\n", circles, "\n")
  
  expect_true(circles >= 1, info = "At least one circle should be rendered in div#animint svg")
})

# WorldBank app once for all related tests
worldbank_dir <- "examples/shiny-WorldBank"
if (dir.exists(worldbank_dir)) {
  port <- sample(3000:9999, 1)
  worldbank_app_info <- start_shiny_app(worldbank_dir, port)
  testthat::teardown({
    worldbank_app_info$proc$kill()
    unlink("shiny_err.log")
    unlink("shiny_out.log")
  })
}

test_that("WorldBank renders in a shiny app", {
  if (!dir.exists(worldbank_dir)) skip("WorldBank app directory not found")
  
  b <- ChromoteSession$new()
  b$view()
  on.exit(b$close(), add = TRUE)
  b$Page$navigate(worldbank_app_info$url)
  b$Page$loadEventFired(wait_ = TRUE, timeout = 30000)
  Sys.sleep(20)  # Match RSelenium's 1s + 20s
  
  div_classes <- b$Runtime$evaluate(
    "Array.from(document.querySelectorAll('div')).map(d => d.className).join(', ')"
  )$result$value
  cat("All div classes:\n", div_classes, "\n")
  
  animint_ready <- FALSE
  animint_html <- ""
  for (i in 1:100) {
    res <- b$Runtime$evaluate("document.querySelector('div#animint') !== null")
    if (isTRUE(res$result$value)) {
      animint_ready <- TRUE
      animint_html <- b$Runtime$evaluate(
        "document.querySelector('div#animint').outerHTML"
      )$result$value
      break
    }
    Sys.sleep(0.1)
  }
  expect_true(animint_ready, info = "animint div should be present")
  cat("Animint div HTML (first 1000 chars):\n", substr(animint_html, 1, 1000), "\n")
  
  circles <- b$Runtime$evaluate(
    "document.querySelector('div#animint svg').querySelectorAll('circle').length"
  )$result$value
  cat("Number of circle elements:\n", circles, "\n")
  
  expect_true(circles >= 1, info = "At least one circle should be rendered in div#animint svg")
})

test_that("animation updates", {
  if (!dir.exists(worldbank_dir)) skip("WorldBank app directory not found")
  
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
  Sys.sleep(5)  # Match RSelenium's 5s wait
  new_year <- get_year()
  expect_true(old_year != new_year, info = "Year should change after animation")
})

test_that("animint fits in div", {
  if (!dir.exists(worldbank_dir)) skip("WorldBank app directory not found")
  
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
  if (!dir.exists(worldbank_dir)) skip("WorldBank app directory not found")
  
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
  Sys.sleep(5)  # Match RSelenium's wait after click
  
  new_countries <- get_countries()
  expect_identical(new_countries, c("Bahrain", "United States", "Vietnam"), info = "Bahrain should be added after click")
})

test_that("shiny changes axes", {
  if (!dir.exists(worldbank_dir)) skip("WorldBank app directory not found")
  
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
  Sys.sleep(10)  # Match RSelenium's 10s wait
  
  new_facets <- get_facets()
  expect_identical(new_facets, c("literacy", "Years"), info = "Facets should update to literacy and Years")
})

# Test RMarkdown rendering
test_that("animint plot renders in an interactive document", {
  if (!requireNamespace("rmarkdown")) skip("Package 'rmarkdown' not installed")
  rmd_file <- "examples/rmarkdown/index.Rmd"
  if (!file.exists(rmd_file)) skip("RMarkdown file not found")
  
  port <- sample(3000:9999, 1)
  app_info <- start_rmd_app(rmd_file, port)
  on.exit({
    app_info$proc$kill()
    unlink("shiny_err.log")
    unlink("shiny_out.log")
  }, add = TRUE)
  
  cat("Attempting to access RMarkdown app at:", app_info$url, "\n")
  
  b <- ChromoteSession$new()
  b$view()
  on.exit(b$close(), add = TRUE)
  b$Page$navigate(app_info$url)
  b$Page$loadEventFired(wait_ = TRUE, timeout = 30000)
  Sys.sleep(20)  
  
  iframe_ready <- FALSE
  iframe_html <- ""
  for (i in 1:100) {
    res <- b$Runtime$evaluate("document.querySelector('.shiny-frame') !== null")
    if (isTRUE(res$result$value)) {
      iframe_ready <- TRUE
      iframe_html <- b$Runtime$evaluate(
        "document.querySelector('.shiny-frame').contentDocument.documentElement.outerHTML"
      )$result$value
      break
    }
    Sys.sleep(0.1)
  }
  expect_true(iframe_ready, info = "Shiny iframe should be present")
  cat("Iframe HTML (first 1000 chars):\n", substr(iframe_html, 1, 1000), "\n")
  
  circles <- b$Runtime$evaluate(
    "document.querySelector('.shiny-frame').contentDocument.querySelectorAll('svg circle').length"
  )$result$value
  cat("Number of circle elements in iframe:\n", circles, "\n")
  
  if (circles == 0) {
    animint_circles <- b$Runtime$evaluate(
      "document.querySelector('.shiny-frame').contentDocument.querySelectorAll('div#animint svg circle').length"
    )$result$value
    cat("Number of circle elements in div#animint svg:\n", animint_circles, "\n")
  }
  
  expect_true(circles >= 1, info = "At least one circle should be rendered in iframe")
})