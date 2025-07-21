library(testthat)
library(chromote)
library(callr)
library(shiny)
library(animint2)

# Set working directory to repository root
setwd(normalizePath(file.path("..", "..")))
if (!dir.exists("inst/examples/shiny")) {
  cat("Working directory is: ", getwd(), "\n")
  stop("Working directory is not the repository root: ", getwd())
}


test_that("animint plot renders in a shiny app", {
  app_dir <- "inst/examples/shiny"
  if (!dir.exists(app_dir)) skip("Shiny app directory not found")
  
  port <- sample(3000:9999, 1)
  app_info <- start_shiny_app(app_dir, port)
  on.exit({
    app_info$proc$kill()
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

test_that("WorldBank shiny app functionality", {
  worldbank_dir <- "inst/examples/shiny-WorldBank"
  if (!dir.exists(worldbank_dir)) skip("WorldBank app directory not found")
  if (file.path(worldbank_dir, "app.R") == file.path(tempdir(), "app.R")) {
    skip("Tests skipped for mock app")
  }
  
  port <- sample(3000:9999, 1)
  app_info <- start_shiny_app(worldbank_dir, port)
  on.exit({
    app_info$proc$kill()
  }, add = TRUE)
  
  cat("Navigating to: ", app_info$url, "\n")
  
  b <- ChromoteSession$new()
  b$view()
  on.exit(b$close(), add = TRUE)
  b$Page$navigate(app_info$url)
  b$Page$loadEventFired(wait_ = TRUE, timeout = 1800)
  Sys.sleep(10) # Increased for full rendering
  
  
  animint_ready <- FALSE
  for (i in 1:800) {
    res <- b$Runtime$evaluate("document.querySelector('div#animint') !== null")
    if (isTRUE(res$result$value)) {
      animint_ready <- TRUE
      break
    }
    Sys.sleep(0.1)
  }
  expect_true(animint_ready, info = "animint div should be present")
  
  
  circles <- b$Runtime$evaluate(
    "document.querySelector('div#animint svg')?.querySelectorAll('circle').length || 0"
  )$result$value
  expect_true(circles >= 1, info = "At least one circle should be rendered")
  
  
  get_year <- function() {
    year <- b$Runtime$evaluate(
      "var nodes = document.querySelectorAll('svg text'); var t = Array.from(nodes).find(n => n.textContent.includes('year = ')); t ? t.textContent.replace('year = ', '') : ''"
    )$result$value
    expect_true(nchar(year) > 0, info = "Year text should be present")
    return(year)
  }
  
  old_year <- get_year()
  Sys.sleep(10)
  new_year <- get_year()
  expect_true(old_year != new_year, info = "Year should change after animation")
  
  # Test 4: Div left position
  div_left <- b$Runtime$evaluate(
    "document.querySelector('#animint').getBoundingClientRect().left"
  )$result$value
  expect_true(is.numeric(div_left), info = "Div left position should be numeric")
})

test_that("animint plot renders in an interactive document", {
  if (!requireNamespace("rmarkdown")) skip("Package 'rmarkdown' not installed")
  rmd_file <- "inst/examples/rmarkdown/index.Rmd"
  if (!file.exists(rmd_file)) skip("RMarkdown file not found")
  
  port <- sample(3000:9999, 1)
  app_info <- start_rmd_app(rmd_file, port)
  on.exit({
    app_info$proc$kill()
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