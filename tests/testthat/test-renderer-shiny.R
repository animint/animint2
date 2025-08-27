print("Running shiny tests...")
port <- 3147
setwd(normalizePath(file.path("..", "..")))
test_that("animint plot renders in a shiny app", {
  app_dir <- file.path("inst", "examples", "shiny")
  if (!dir.exists(app_dir)) skip("Shiny app directory not found")
  app_info <- start_app("shiny", app_dir, port)
  on.exit(app_info$proc$kill(), add = TRUE)
  remDr$navigate(app_info$url)
  Sys.sleep(20)
  # Wait for animint div to be present
  animint_ready <- FALSE
  while (TRUE) {
    res <- remDr$Runtime$evaluate("document.querySelector('div#animint') !== null")
    if (isTRUE(res$result$value)) {
      animint_ready <- TRUE
      break
    }
    Sys.sleep(0.1)
  }
  expect_true(animint_ready, info = "animint div should be present") 
  # Check for rendered circles
  circles <- remDr$Runtime$evaluate(
    "document.querySelector('div#animint svg').querySelectorAll('circle').length"
  )$result$value
  expect_true(circles >= 1, info = "At least one circle should be rendered in div#animint svg")
})
test_that("WorldBank shiny app functionality", {
  worldbank_dir <- file.path("inst", "examples", "shiny-WorldBank")
  if (!dir.exists(worldbank_dir)) skip("WorldBank app directory not found")
  if (file.path(worldbank_dir, "app.R") == file.path(tempdir(), "app.R")) {
    skip("Tests skipped for mock app")
  }
  app_info <- start_app("shiny", worldbank_dir, port)
  on.exit(app_info$proc$kill(), add = TRUE)
  remDr$navigate(app_info$url)
  Sys.sleep(10)
  # Wait for animint div to be present
  animint_ready <- FALSE
  while (TRUE) {
    res <- remDr$Runtime$evaluate("document.querySelector('div#animint') !== null")
    if (isTRUE(res$result$value)) {
      animint_ready <- TRUE
      break
    }
    Sys.sleep(0.1)
  }
  expect_true(animint_ready, info = "animint div should be present")
  # Check for rendered circles
  circles <- remDr$Runtime$evaluate(
    "document.querySelector('div#animint svg')?.querySelectorAll('circle').length || 0"
  )$result$value
  expect_true(circles >= 1, info = "At least one circle should be rendered")
  get_year <- function() {
    year <- remDr$Runtime$evaluate(
      "var nodes = document.querySelectorAll('svg text'); var t = Array.from(nodes).find(n => n.textContent.includes('year = ')); t ? t.textContent.replace('year = ', '') : ''"
    )$result$value
    expect_true(nchar(year) > 0, info = "Year text should be present")
    return(year)
  }
  old_year <- get_year()
  Sys.sleep(10)
  new_year <- get_year()
  expect_true(old_year != new_year, info = "Year should change after animation")
  div_left <- remDr$Runtime$evaluate(
    "document.querySelector('#animint').getBoundingClientRect().left"
  )$result$value
  expect_true(is.numeric(div_left), info = "Div left position should be numeric")
})
test_that("animint plot renders in an interactive document", {
  if (!requireNamespace("rmarkdown")) skip("Package 'rmarkdown' not installed")
  rmd_file <- file.path("inst", "examples", "rmarkdown", "index.Rmd")
  if (!file.exists(rmd_file)) skip("RMarkdown file not found")
  app_info <- start_app("rmd", rmd_file, port)
  on.exit(app_info$proc$kill(), add = TRUE)
  remDr$navigate(app_info$url)
  Sys.sleep(30)
  iframe_ready <- FALSE
  while (TRUE) {
    res <- remDr$Runtime$evaluate("document.querySelector('.shiny-frame') !== null")
    if (isTRUE(res$result$value)) {
      iframe_ready <- TRUE
      break
    }
    Sys.sleep(0.1)
  }
  expect_true(iframe_ready, info = "Shiny iframe should be present")
  circles <- remDr$Runtime$evaluate(
    "document.querySelector('.shiny-frame').contentDocument.querySelectorAll('svg circle').length"
  )$result$value
  if (circles == 0) {
    animint_circles <- remDr$Runtime$evaluate(
      "document.querySelector('.shiny-frame').contentDocument.querySelectorAll('div#animint svg circle').length"
    )$result$value
    circles <- animint_circles
  }
  expect_true(circles >= 1, info = "At least one circle should be rendered in iframe")
})